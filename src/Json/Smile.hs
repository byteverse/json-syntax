{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Json.Smile
  ( encodeSimple
  ) where

import Prelude hiding (Bool(..))

import Data.Bytes.Builder (Builder)
import Data.Foldable (foldMap')
import Data.Text.Short (ShortText)
import Data.Word (Word8)
import Json (Value(..), Member(..))

import qualified Data.Bytes.Builder as B
import qualified Data.Bytes.Text.Ascii as Ascii
import qualified Data.Number.Scientific as Sci
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Short as TS

-- | Encode a Json 'Value' to the Smile binary format.
-- This encoder does not produce backreferences.
encodeSimple :: Value -> Builder
encodeSimple v0 = header <> recurse v0
  where
  header = B.bytes $ Ascii.fromString ":)\n\x04"
  recurse :: Value -> Builder
  recurse (Object obj) = B.word8 0xFA <> foldMap' recMember obj <> B.word8 0xFB
  recurse (Array arr) = B.word8 0xF8 <> foldMap' recurse arr <> B.word8 0xF9
  recurse (String str) = B.word8 0xE4 <> B.shortTextUtf8 str <> B.word8 0xFC
  recurse (Number x)
    | Just i32 <- Sci.toInt32 x
      = B.word8 0x24 <> B.int32LEB128 i32
    | Just i64 <- Sci.toInt64 x
      = B.word8 0x25 <> B.int64LEB128 i64
    | otherwise = errorWithoutStackTrace
        ("TODO Smile encoding only supports Int32/Int64 numbers for now: " ++ show x)
  recurse Null = B.word8 0x21
  recurse False = B.word8 0x22
  recurse True = B.word8 0x23
  recMember :: Member -> Builder
  recMember Member{key,value} = encodeKeySimple key <> recurse value

encodeKeySimple :: ShortText -> Builder
encodeKeySimple str = case SBS.length (TS.toShortByteString str) of
  0 -> B.word8 0x20
  1 -> B.word8 0x80 <> B.shortTextUtf8 str
  n | n < 56
    , w8 <- fromIntegral @Int @Word8 (n - 2)
    -> B.word8 (0xC0 + w8) <> B.shortTextUtf8 str
    | otherwise -> B.word8 0x34 <> B.shortTextUtf8 str <> B.word8 0xFC
