{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Json.Smile
  ( encodeSimple
  ) where

import Prelude hiding (Bool(..))

import Data.Bits (finiteBitSize,unsafeShiftL,unsafeShiftR,(.&.),(.|.))
import Data.Bytes.Builder (Builder)
import Data.Foldable (foldMap')
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.Text.Short (ShortText)
import Data.Word (Word8,Word32)
import Data.Word.Zigzag (toZigzag,toZigzag32,toZigzag64)
import Json (Value(..), Member(..))
import Numeric.Natural (Natural)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as B
import qualified Data.Bytes.Text.Ascii as Ascii
import qualified Data.ByteString.Short as SBS
import qualified Data.Number.Scientific as Sci
import qualified Data.Text.Short as TS

-- | Encode a Json 'Value' to the Smile binary format.
-- This encoder does not produce backreferences.
encodeSimple :: Value -> Builder
encodeSimple v0 = header <> recurse v0
  where
  header = B.bytes $ Ascii.fromString ":)\n\x00"
  recurse :: Value -> Builder
  recurse (Object obj) = B.word8 0xFA <> foldMap' recMember obj <> B.word8 0xFB
  recurse (Array arr) = B.word8 0xF8 <> foldMap' recurse arr <> B.word8 0xF9
  recurse (String str) = B.word8 0xE4 <> B.shortTextUtf8 str <> B.word8 0xFC
  recurse (Number x)
    | Just i32 <- Sci.toInt32 x
      = B.word8 0x24 <> vlqSmile (fromIntegral $ toZigzag32 i32)
    | Just i64 <- Sci.toInt64 x
      = B.word8 0x25 <> vlqSmile (fromIntegral $ toZigzag64 i64)
    | otherwise = Sci.withExposed encodeSmall encodeBig x
  recurse Null = B.word8 0x21
  recurse False = B.word8 0x22
  recurse True = B.word8 0x23
  recMember :: Member -> Builder
  recMember Member{key,value} = encodeKeySimple key <> recurse value
  encodeSmall :: Int -> Int -> Builder
  encodeSmall c e
    =  B.word8 0x2A -- bigdecimal token tag
    <> vlqSmile (fromIntegral @Word32 @Natural $ toZigzag32 scale) -- exponent
    <> vlqSmile (fromIntegral @Int @Natural nCoeffBytes) -- size of byte array
    <> B.sevenEightSmile (Bytes.fromShortByteString $ SBS.pack coeffBytes)
    where
    -- | WARNING performs modulo since SMILE doesn't support exponents larger than 32-bit
    scale = fromIntegral @Int @Int32 e
    nCoeffBytes =
      let nbits = finiteBitSize c
          nbytes = nbits `div` 8
       in if nbits `mod` 8 == 0 then nbytes else nbytes + 1
    coeffBytes :: [Word8] -- big-endian
    coeffBytes = [nCoeffBytes - 1 .. 0] <&> \i ->
      fromIntegral @Int @Word8 (unsafeShiftL c (8*i))
  encodeBig :: Integer -> Integer -> Builder
  encodeBig c e = errorWithoutStackTrace ("TODO Smile encoding only supports numbers that \
                \can be represented by Int*10^Int for now: " ++ show (Sci.large c e))

encodeKeySimple :: ShortText -> Builder
encodeKeySimple str = case SBS.length (TS.toShortByteString str) of
  0 -> B.word8 0x20
  1 -> B.word8 0x80 <> B.shortTextUtf8 str
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
