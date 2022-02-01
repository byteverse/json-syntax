{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Json.Smile
  ( -- * Encode JSON Document
    encode
    -- * Encode JSON Atoms
    -- ** Integer
  , encodeBigInteger
    -- ** String
  , encodeString
  , encodeAsciiString
    -- ** Key
  , encodeKey
  , encodeAsciiKey
  ) where

import Prelude hiding (Bool(..))

import Control.Monad.ST (ST)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bits (countLeadingZeros,complement,unsafeShiftR,(.&.),(.|.))
import Data.Bits (testBit)
import Data.Bytes.Builder (Builder)
import Data.Foldable (foldMap)
import Data.Int (Int32)
import Data.Primitive (ByteArray(ByteArray),newByteArray)
import Data.Primitive (writeByteArray,byteArrayFromListN,sizeofByteArray)
import Data.Primitive (MutableByteArray(..),unsafeFreezeByteArray)
import Data.Primitive (readByteArray,copyMutableByteArray,indexByteArray)
import Data.Text.Short (ShortText)
import Data.Word (Word8,Word32,Word64)
import Data.Word.Zigzag (toZigzag32,toZigzag64)
import GHC.Word (Word(..))
import Json (Value(..), Member(..))
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Arithmetic.Nat as Nat
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as B
import qualified Data.Bytes.Builder.Bounded as Bounded
import qualified Data.Bytes.Builder.Bounded.Unsafe as Unsafe
import qualified Data.Bytes.Text.Ascii as Ascii
import qualified Data.ByteString.Short as SBS
import qualified Data.Number.Scientific as Sci
import qualified Data.Text.Short as TS
import qualified GHC.Exts as Exts
import qualified GHC.Integer.GMP.Internals as GMP
import qualified Prelude

-- | Encode a Json 'Value' to the Smile binary format.
-- This encoder does not produce backreferences.
encode :: Value -> Builder
{-# noinline encode #-}
encode v0 = header <> recurse v0
  where
  header = B.bytes $ Ascii.fromString ":)\n\x00"
  recurse :: Value -> Builder
  recurse (Object obj) = B.word8 0xFA <> foldMap recMember obj <> B.word8 0xFB
  recurse (Array arr) = B.word8 0xF8 <> foldMap recurse arr <> B.word8 0xF9
  recurse (String str) = encodeString str
  recurse (Number x)
    | Just i32 <- Sci.toInt32 x
    , -16 <= i32 && i32 <= 15
    , w5 <- fromIntegral @Word32 @Word8 (toZigzag32 i32)
      = B.word8 (0xC0 + w5)
    | Just i32 <- Sci.toInt32 x
      = B.fromBounded Nat.constant (Bounded.word8 0x24 `Bounded.append` vlqSmile64 (fromIntegral @Word32 @Word64 (toZigzag32 i32)))
    | Just i64 <- Sci.toInt64 x
      = B.fromBounded Nat.constant (Bounded.word8 0x25 `Bounded.append` vlqSmile64 (toZigzag64 i64))
    | otherwise = Sci.withExposed encodeSmallDecimal encodeBigDecimal x
  recurse Null = B.word8 0x21
  recurse False = B.word8 0x22
  recurse True = B.word8 0x23
  recMember :: Member -> Builder
  recMember Member{key,value} = encodeKey key <> recurse value
  encodeSmallDecimal :: Int -> Int -> Builder
  encodeSmallDecimal !c !e = encodeBigDecimal (fromIntegral c) (fromIntegral e)
  encodeBigDecimal :: Integer -> Integer -> Builder
  encodeBigDecimal c e = case e of
    0 -> encodeBigInteger c
    _ -> B.word8 0x2A -- bigdecimal token tag
      <> vlqSmile ( fromIntegral @Word32 @Natural
                  $ toZigzag32 scale)
      <> vlqSmile (fromIntegral @Int @Natural $ sizeofByteArray raw) -- size of byte digits
      <> B.sevenEightSmile (Bytes.fromByteArray raw) -- 7/8 encoding of byte digits
      where
      scale :: Int32
      -- WARNING smile can't handle exponents outside int32_t, so this truncates
      -- WARNING "scale" is what Java BigDecimal thinks, which is
      -- negative of all mathematics since exponential notation was invented 💩
      scale = fromIntegral @Integer @Int32 (-e)
      raw = integerToBase256ByteArray c

-- | Encode a number using as SMILE @BigInteger@ token type (prefix @0x26@).
encodeBigInteger :: Integer -> Builder
encodeBigInteger n = B.word8 0x26
  <> vlqSmile (fromIntegral @Int @Natural $ sizeofByteArray raw) -- size of byte digits
  <> B.sevenEightSmile (Bytes.fromByteArray raw) -- 7/8 encoding of byte digits
  where
  !raw = integerToBase256ByteArray n

integerToBase256ByteArray :: Integer -> ByteArray
integerToBase256ByteArray c = if c == 0
  then byteArrayFromListN 1 [0::Word8]
  else case c of
    GMP.Jp# bn -> unsafeDupablePerformIO $ do
      let nDigits256 = fromIntegral @Word @Int (W# (GMP.sizeInBaseBigNat bn 256#))
      mut <- newByteArray nDigits256
      let !(MutableByteArray mut#) = mut
      !_ <- GMP.exportBigNatToMutableByteArray bn mut# 0## 1#
      -- This is safe because Jp cannot have zero inside it.
      w0 :: Word8 <- readByteArray mut 0
      if testBit w0 7
        then do
          -- If the upper bit is 1, then we must introduce a leading
          -- zero byte.
          dst <- newByteArray (nDigits256 + 1)
          writeByteArray dst 0 (0x00 :: Word8)
          copyMutableByteArray dst 1 mut 0 nDigits256
          unsafeFreezeByteArray dst
        else unsafeFreezeByteArray mut
    GMP.Jn# bn -> twosComplementBigNat bn
    GMP.S# i -> case i Exts.># 0# of
      1# -> encodePosWordBase256 (W# (Exts.int2Word# i))
      _ -> encodeNegWordBase256 (W# (Exts.int2Word# i))

twosComplementBigNat :: GMP.BigNat -> ByteArray
twosComplementBigNat bn = unsafeDupablePerformIO $ do
  let nDigits256 = fromIntegral @Word @Int (W# (GMP.sizeInBaseBigNat bn 256#))
  mut <- newByteArray nDigits256
  let !(MutableByteArray mut#) = mut
  !_ <- GMP.exportBigNatToMutableByteArray bn mut# 0## 1#
  -- First, complement
  let goComplement !ix = if ix >= 0
        then do
          w :: Word8 <- readByteArray mut ix
          writeByteArray mut ix (complement w)
          goComplement (ix - 1)
        else pure ()
  goComplement (nDigits256 - 1)
  -- Second, add one
  let goAddOne !ix = if ix >= 0
        then do
          w :: Word8 <- readByteArray mut ix
          case w of
            0xFF -> do
              writeByteArray mut ix (0 :: Word8)
              goAddOne (ix - 1)
            _ -> writeByteArray mut ix (w + 1)
        else pure ()
  goAddOne (nDigits256 - 1)
  leader :: Word8 <- readByteArray mut 0
  if testBit leader 7
    then unsafeFreezeByteArray mut
    else do
      dst <- newByteArray (nDigits256 + 1)
      copyMutableByteArray dst 1 mut 0 nDigits256
      writeByteArray dst 0 (0xFF :: Word8)
      unsafeFreezeByteArray dst

-- Only works on 64-bit architectures.
-- Precondition: must be a bit pattern of a positive integer
encodePosWordBase256 :: Word -> ByteArray
encodePosWordBase256 !w = runByteArrayST $ do
  -- If a positive number broken into its constituent bytes has
  -- leading byte with MSB 1, then we want to have a zero byte
  -- in front. That is, we want to produce:
  --
  -- 0000_0000 1101_1010 01010101
  --
  -- instead of
  --
  -- 1101_1010 01010101
  --
  -- Because the latter would be misinterpreted as a negative number.
  let !total = quot (72 - countLeadingZeros w) 8
  dst <- newByteArray total
  let go !ix !acc = if ix >= 0 
        then do
          writeByteArray dst ix (fromIntegral @Word @Word8 acc)
          go (ix - 1) (unsafeShiftR acc 8)
        else unsafeFreezeByteArray dst
  go (total - 1) w

-- Same deal as encodePosWordBase256.
-- Andrew Martin: I am not sure why we need to subtract from 72
-- instead of 71. But if we change it, a bunch of tests fail.
encodeNegWordBase256 :: Word -> ByteArray
encodeNegWordBase256 !w = runByteArrayST $ do
  let !total = quot (72 - countLeadingZeros (complement w)) 8
  dst <- newByteArray total
  let go !ix !acc = if ix >= 0 
        then do
          writeByteArray dst ix (fromIntegral @Word @Word8 acc)
          go (ix - 1) (unsafeShiftR acc 8)
        else unsafeFreezeByteArray dst
  go (total - 1) w

-- | Encode a string in which all characters are ASCII. This precondition
-- is not checked. Resulting output will be corrupt if this condition
-- is not satisfied.
encodeAsciiString :: ShortText -> Builder
encodeAsciiString !str
  | n == 0 = B.word8 0x20
  | n <= 64 = B.copyCons (0x40 + fromIntegral (n - 1)) (Bytes.fromShortByteString (TS.toShortByteString str))
  | otherwise = B.word8 0xe0 <> B.shortTextUtf8 str <> B.word8 0xFC
  where
  n = SBS.length (TS.toShortByteString str)

-- | Encode a string.
encodeString :: ShortText -> Builder
{-# inline encodeString #-}
encodeString !str = case SBS.length (TS.toShortByteString str) of
  0 -> B.word8 0x20
  n -> case TS.isAscii str of
    Prelude.True
      | n <= 64 -> B.copyCons (0x40 + fromIntegral (n - 1)) (Bytes.fromShortByteString (TS.toShortByteString str))
      | otherwise -> B.word8 0xe0 <> B.shortTextUtf8 str <> B.word8 0xFC
    Prelude.False
      | n <= 65 -> B.copyCons (0x80 + fromIntegral (n - 2)) (Bytes.fromShortByteString (TS.toShortByteString str))
      | otherwise -> B.word8 0xE4 <> B.shortTextUtf8 str <> B.word8 0xFC

-- | Encode a key.
encodeKey :: ShortText -> Builder
{-# inline encodeKey #-}
encodeKey !str = case SBS.length (TS.toShortByteString str) of
  0 -> B.word8 0x20
  n | n <= 64
    && TS.isAscii str
    , w8 <- fromIntegral @Int @Word8 (n - 1)
    -> B.copyCons (0x80 + w8) (Bytes.fromShortByteString (TS.toShortByteString str))
  n | n < 56
    , w8 <- fromIntegral @Int @Word8 (n - 2)
    -> B.copyCons (0xC0 + w8) (Bytes.fromShortByteString (TS.toShortByteString str))
    | otherwise -> B.word8 0x34 <> B.shortTextUtf8 str <> B.word8 0xFC

-- | Encode a key in which all characters are ASCII. This precondition
-- is not checked. Resulting output will be corrupt if this condition
-- is not satisfied.
encodeAsciiKey :: ShortText -> Builder
encodeAsciiKey str = case SBS.length (TS.toShortByteString str) of
  0 -> B.word8 0x20
  n | n <= 64
    , w8 <- fromIntegral @Int @Word8 (n - 1)
    -> B.word8 (0x80 + w8) <> B.shortTextUtf8 str
    | otherwise -> B.word8 0x34 <> B.shortTextUtf8 str <> B.word8 0xFC

vlqSmile :: Natural -> Builder
vlqSmile n0 =
  let (rest, lastBits) = take6bits n0
   in loop rest <> B.word8 (lastBits .|. 0x80)
  where
  loop n
    | n == 0 = mempty
    | (rest, bits) <- take7bits n
      = loop rest <> B.word8 bits
  take7bits :: Natural -> (Natural, Word8)
  take7bits n = (n `unsafeShiftR` 7, fromIntegral @Natural @Word8 n .&. 0x7F)
  take6bits :: Natural -> (Natural, Word8)
  take6bits n = (n `unsafeShiftR` 6, fromIntegral @Natural @Word8 n .&. 0x3F)

-- Precondition: input is not zero
vlqSmile64 :: Word64 -> Bounded.Builder 10
vlqSmile64 !n0 = Unsafe.construct $ \buf ix0 -> do
  let !w0 = fromIntegral @Word64 @Word8 n0 .&. 0x3F
  writeByteArray buf ix0 (0x80 .|. w0)
  let !acc0 = n0 `unsafeShiftR` 6
  let loop !acc !ix = case acc of
        0 -> pure ix
        _ -> do
          let !w = fromIntegral @Word64 @Word8 acc .&. 0x7F
          writeByteArray buf ix w
          let !acc' = acc `unsafeShiftR` 7
          loop acc' (ix + 1)
  ix1 <- loop acc0 (ix0 + 1)
  reverseBytes buf ix0 (ix1 - 1)
  pure ix1

-- Reverse the bytes in the designated slice. This takes
-- an inclusive start offset and an inclusive end offset.
--
-- Copied from bytebuild
reverseBytes :: MutableByteArray s -> Int -> Int -> ST s ()
{-# inline reverseBytes #-}
reverseBytes arr begin end = go begin end where
  go ixA ixB = if ixA < ixB
    then do
      a :: Word8 <- readByteArray arr ixA
      b :: Word8 <- readByteArray arr ixB
      writeByteArray arr ixA b
      writeByteArray arr ixB a
      go (ixA + 1) (ixB - 1)
    else pure ()
