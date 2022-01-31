{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Json.Smile
  ( encodeSimple
  ) where

import Prelude hiding (Bool(..))

import Data.Bits (unsafeShiftR,(.&.),(.|.))
import Data.Bytes.Builder (Builder)
import Data.Foldable (foldMap')
import Data.Int (Int32)
import Data.Primitive.ByteArray (ByteArray,newByteArray,byteArrayFromListN,sizeofByteArray)
import Data.Primitive.ByteArray (MutableByteArray(..),unsafeFreezeByteArray)
import Data.Text.Short (ShortText)
import Data.Word (Word8,Word32)
import Data.Word.Zigzag (toZigzag32,toZigzag64)
import GHC.Word (Word(..))
import Json (Value(..), Member(..))
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as B
import qualified Data.Bytes.Text.Ascii as Ascii
import qualified Data.ByteString.Short as SBS
import qualified Data.Number.Scientific as Sci
import qualified Data.Text.Short as TS
import qualified GHC.Integer.GMP.Internals as GMP

-- | Encode a Json 'Value' to the Smile binary format.
-- This encoder does not produce backreferences.
encodeSimple :: Value -> Builder
encodeSimple v0 = header <> recurse v0
  where
  header = B.bytes $ Ascii.fromString ":)\n\x00"
  recurse :: Value -> Builder
  recurse (Object obj) = B.word8 0xFA <> foldMap' recMember obj <> B.word8 0xFB
  recurse (Array arr) = B.word8 0xF8 <> foldMap' recurse arr <> B.word8 0xF9
  recurse (String str) = encodeStringSimple str
  recurse (Number x)
    | Just i32 <- Sci.toInt32 x
    , -16 <= i32 && i32 <= 15
    , w5 <- fromIntegral @Word32 @Word8 (toZigzag32 i32)
      = B.word8 (0xC0 + w5)
    | Just i32 <- Sci.toInt32 x
      = B.word8 0x24 <> vlqSmile (fromIntegral $ toZigzag32 i32)
    | Just i64 <- Sci.toInt64 x
      = B.word8 0x25 <> vlqSmile (fromIntegral $ toZigzag64 i64)
    | otherwise = Sci.withExposed encodeSmallDecimal encodeBigDecimal x
  recurse Null = B.word8 0x21
  recurse False = B.word8 0x22
  recurse True = B.word8 0x23
  recMember :: Member -> Builder
  recMember Member{key,value} = encodeKeySimple key <> recurse value
  encodeSmallDecimal :: Int -> Int -> Builder
  encodeSmallDecimal c e = encodeBigDecimal (fromIntegral c) (fromIntegral e)
  encodeBigDecimal :: Integer -> Integer -> Builder
  encodeBigDecimal c e
    =  B.word8 0x2A -- bigdecimal token tag
    <> vlqSmile ( fromIntegral @Word32 @Natural
                $ toZigzag32 scale)
    <> vlqSmile (fromIntegral @Int @Natural $ sizeofByteArray coeffBytes) -- size of byte digits
    <> B.sevenEightSmile (Bytes.fromByteArray coeffBytes) -- 7/8 encoding of byte digits
    where
    scale :: Int32
    -- WARNING smile can't handle exponents outside int32_t, so this truncates
    -- WARNING "scale" is what Java BigDecimal thinks, which is negative of all mathematics since exponential notation was invented 💩
    scale = fromIntegral @Integer @Int32 (-e)
    coeffBytes :: ByteArray
    coeffBytes =
      if c == 0 then byteArrayFromListN 1 [0::Word8]
      else unsafeDupablePerformIO $ do
        let nDigits256 = W# (GMP.sizeInBaseInteger c 256#)
        mut <- newByteArray (fromIntegral @Word @Int nDigits256)
        let !(MutableByteArray mut#) = mut
        !_ <- GMP.exportIntegerToMutableByteArray c mut# 0## 1#
        unsafeFreezeByteArray mut

encodeStringSimple :: ShortText -> Builder
encodeStringSimple str = case SBS.length (TS.toShortByteString str) of
  0 -> B.word8 0x20
  n | n <= 64
    && TS.isAscii str
    , w8 <- fromIntegral @Int @Word8 (n - 1)
    -> B.word8 (0x40 + w8) <> B.shortTextUtf8 str
    | n <= 65
    , w8 <- fromIntegral @Int @Word8 (n - 2)
    -> B.word8 (0x80 + w8) <> B.shortTextUtf8 str
    | otherwise
    -> B.word8 0xE4 <> B.shortTextUtf8 str <> B.word8 0xFC

encodeKeySimple :: ShortText -> Builder
encodeKeySimple str = case SBS.length (TS.toShortByteString str) of
  0 -> B.word8 0x20
  n | n <= 64
    && TS.isAscii str
    , w8 <- fromIntegral @Int @Word8 (n - 1)
    -> B.word8 (0x80 + w8) <> B.shortTextUtf8 str
  n | n < 56
    , w8 <- fromIntegral @Int @Word8 (n - 2)
    -> B.word8 (0xC0 + w8) <> B.shortTextUtf8 str
    | otherwise -> B.word8 0x34 <> B.shortTextUtf8 str <> B.word8 0xFC

vlqSmile :: Natural -> Builder
vlqSmile n0 =
  let (rest, lastBits) = take127bits n0
   in loop rest <> B.word8 (lastBits .|. 0x80)
  where
  loop n
    | n == 0 = mempty
    | (rest, bits) <- take127bits n
      = loop rest <> B.word8 bits
  take127bits :: Natural -> (Natural, Word8)
  take127bits n = (n `unsafeShiftR` 7, fromIntegral @Natural @Word8 n .&. 0x7F)
