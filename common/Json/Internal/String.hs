{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Json.Internal.String
  ( advance
  , Result(..)
  , CanMemcpy
  , pattern YesMemcpy
  , pattern NoMemcpy
  , copyAndUnescape
  , c2w
  , byteArrayToShortByteString
  , hasLess
  , hasMore
  , hasZeroAscii
  , hasValueAscii
  ) where

import Prelude hiding (length)

import Control.Monad.ST (ST)
import Data.Bits (complement, (.&.), (.|.), xor)
import Data.Bits (unsafeShiftR)
import Data.Bytes (Bytes)
import Data.Bytes.Types (Bytes(..))
import Data.Char (ord)
import Data.Primitive (ByteArray,MutableByteArray)
import Data.Primitive.ByteArray (indexByteArray)
import Data.Text.Short (ShortText)
import Data.Word (Word8, Word16, Word64)
import GHC.Exts (Char(C#),word2Int#,chr#)
import GHC.Word (Word16(W16#))

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser.Utf8 as Utf8
import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.Primitive as PM
import qualified Data.Text.Short.Unsafe as TS


advance :: Bytes -> Result
{-# inline advance #-}
advance bs0 = loop bs0 YesMemcpy
  where
  loop bs canMemcpy = case advAlign bs canMemcpy of
    Continue canMemcpy' bs' -> case advWords bs' canMemcpy' of
      Continue canMemcpy'' bs'' -> case advChar bs'' canMemcpy'' of
        Continue canMemcpy''' bs''' -> loop bs''' canMemcpy'''
        res -> res
      res -> res
    res -> res


newtype CanMemcpy = CanMemcpy Int

pattern YesMemcpy :: CanMemcpy
pattern YesMemcpy = CanMemcpy 1

pattern NoMemcpy :: CanMemcpy
pattern NoMemcpy = CanMemcpy 0

data Result
  = Continue !CanMemcpy {-# UNPACK #-} !Bytes
  | Finish !CanMemcpy !Int {-# UNPACK #-} !Bytes -- offset of final byte of string, bytes after string
  | EndOfInput
  | IsolatedEscape

advAlign :: Bytes -> CanMemcpy -> Result
{-# inline advAlign #-}
advAlign bs@Bytes{offset} canMemcpy
  | offset `rem` 8 == 0 = Continue canMemcpy bs
  | otherwise = case advChar bs canMemcpy of
      Continue canMemcpy' bs' -> advAlign bs' canMemcpy'
      res -> res

advChar :: Bytes -> CanMemcpy -> Result
{-# inline advChar #-}
advChar bs canMemcpy = case Bytes.uncons bs of
  Nothing -> EndOfInput
  Just (b, bs') -> case b of
    92 -> case Bytes.uncons bs' of
      Nothing -> IsolatedEscape
      Just (_, bs'') -> Continue NoMemcpy bs''
    34 -> Finish canMemcpy (offset bs) bs'
    _ | 31 < b && b < 127 -> Continue canMemcpy bs'
      | otherwise -> Continue NoMemcpy bs'

advWords :: Bytes -> CanMemcpy -> Result
{-# inline advWords #-}
advWords bs0 canMemcpy0 = Continue canMemcpy0 (loop bs0)
  where
  loop bs@Bytes{array,offset,length}
    | offset + 8 > length = bs
    | otherwise =
      let w = indexByteArray array (offset `div` 8)
       in if canAdvWord w
          then loop (Bytes.unsafeDrop 8 bs)
          else bs

canAdvWord :: Word64 -> Bool
{-# inline canAdvWord #-}
canAdvWord w
   = (hasLess 32 w .|. hasMore 126 w == 0) -- is ascii and not a control character (incl. delete)
  .&. (hasValueAscii 92 w .|. hasValueAscii 34 w == 0) -- backslash and double-quote resp.


------ JSON String Parser ------

copyAndUnescape :: Bytes -> Either () ShortText
copyAndUnescape bs0 = P.parseBytesEither parser bs0
  where
  parser = do
    !dst <- P.effect $ PM.newByteArray (Bytes.length bs0)
    let go !ix = do
          P.isEndOfInput >>= \case
            True -> pure ix
            False -> Utf8.any# () `P.bindFromCharToLifted` \c -> case c of
              '\\'# -> Latin.any () >>= \case
                '"' -> do
                  P.effect (PM.writeByteArray dst ix (c2w '"'))
                  go (ix + 1)
                '\\' -> do
                  P.effect (PM.writeByteArray dst ix (c2w '\\'))
                  go (ix + 1)
                't' -> do
                  P.effect (PM.writeByteArray dst ix (c2w '\t'))
                  go (ix + 1)
                'n' -> do
                  P.effect (PM.writeByteArray dst ix (c2w '\n'))
                  go (ix + 1)
                'r' -> do
                  P.effect (PM.writeByteArray dst ix (c2w '\r'))
                  go (ix + 1)
                '/' -> do
                  P.effect (PM.writeByteArray dst ix (c2w '/'))
                  go (ix + 1)
                'b' -> do
                  P.effect (PM.writeByteArray dst ix (c2w '\b'))
                  go (ix + 1)
                'f' -> do
                  P.effect (PM.writeByteArray dst ix (c2w '\f'))
                  go (ix + 1)
                'u' -> do
                  w <- Latin.hexFixedWord16 ()
                  if 0xD800 <= w && w < 0xDFFF
                    then go =<< P.effect (encodeUtf8Char dst ix '\xFFFD')
                    else go =<< P.effect (encodeUtf8Char dst ix (w16ToChar w))
                _ -> P.fail ()
              _ -> go =<< P.effect (encodeUtf8Char dst ix (C# c))
    !sz <- go 0
    str <- P.effect $
            PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray dst sz
    pure $ TS.fromShortByteStringUnsafe (byteArrayToShortByteString str)

encodeUtf8Char :: MutableByteArray s -> Int -> Char -> ST s Int
encodeUtf8Char !marr !ix !c
  | c < '\128' = do
      PM.writeByteArray marr ix (c2w c)
      pure (ix + 1)
  | c < '\x0800' = do
      PM.writeByteArray marr ix
        (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 6 .|. 0b11000000))
      PM.writeByteArray marr (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (ord c))))
      pure (ix + 2)
  | c <= '\xffff' = do
      PM.writeByteArray marr ix
        (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 12 .|. 0b11100000))
      PM.writeByteArray marr (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 6))))
      PM.writeByteArray marr (ix + 2)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (ord c))))
      pure (ix + 3)
  | otherwise = do
      PM.writeByteArray marr ix
        (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 18 .|. 0b11110000))
      PM.writeByteArray marr (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 12))))
      PM.writeByteArray marr (ix + 2)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 6))))
      PM.writeByteArray marr (ix + 3)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (ord c))))
      pure (ix + 4)

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- Precondition: Not in the range [U+D800 .. U+DFFF]
w16ToChar :: Word16 -> Char
w16ToChar (W16# w) = C# (chr# (word2Int# w))

byteArrayToShortByteString :: ByteArray -> BSS.ShortByteString
byteArrayToShortByteString (PM.ByteArray x) = BSS.SBS x


------ Bit Twidling Hacks ------

-- | 'hasLess n x' tests if a word @x@ contains an unsigned byte with value less
-- than @n@. Specifically for @n=1@, it can be used to find a 0-byte by
-- examining one 'Word64' at a time, or any byte by XORing @x@ with a mask
-- first.
-- Returns non-zero for true, zero for false.
-- Uses 4 arithmetic/logical operations when @n@ is constant.
--
-- Requirements: @x ≥ 0@ and @0 ≤ n ≤ 128@.
hasLess :: Int -> Word64 -> Word64
{-# inline hasLess #-}
hasLess n x = (x - subtrahends) .&. complement x .&. highBits
  -- seems like the idea is to look for where the subtraction causes underflow
  -- into the high bit of the next lower byte
  where
  subtrahends = eightBytesOf1 * fromIntegral n

-- | 'hasMore n x' tests if a word @x@ contains an unsigned byte with value
-- greater than @n@.
-- Returns non-zero for true, zero for false.
-- Uses 3 arithmetic/logical operations when @n@ is constant.
--
-- Requirements: @x ≥ 0@ and @0 ≤ n ≤ 127@.
hasMore :: Int -> Word64 -> Word64
{-# inline hasMore #-}
hasMore n x = ((x + addends) .|. x) .&. highBits
  -- seems like the idea is to try to cause a carry into the high bit of each
  -- byte, keeping any high bits that might have been there already
  where
  addends = eightBytesOf1 * (127 - fromIntegral n)

-- | 'hasZeroAscii x' tests if a word (which is already known to contain no
-- bytes > 127) contains any zeros. It works by causing overflow into the high
-- bit if any of the input low bits are set. If any byte is greater than 128,
-- this will likely give the wrong answer, but it takes two additional
-- operations to work correctly in such environments.
-- Returns non-zero for true, zero for false.
-- Uses 3 arithmentic/logical operations.
--
-- Requirements: all bytes in @x@ have high bit clear
hasZeroAscii :: Word64 -> Word64
{-# inline hasZeroAscii #-}
hasZeroAscii x = complement $ (x + lowBits) .|. lowBits
  where
  lowBits = eightBytesOf1 * 127

-- | 'hasValueAscii n x` tests if a word contains any bytes equal to @n@.
-- To do so, we XOR the value to test with a word that has been filled with the
-- byte values in which we're interested. Because XORing a value with itself
-- results in a zero byte and nonzero otherwise, we can pass the result to
-- haszero.
--
-- This version works with 'hasZeroAscii', and so has the same restrictions.
-- It uses a total of 4 arithmentic/logical operations for constant @n@
--
-- Requirements: @0 ≤ n ≤ 127@, and all bytes in @x@ have high bit clear.
hasValueAscii :: Int -> Word64 -> Word64
{-# inline hasValueAscii #-}
hasValueAscii n x = hasZeroAscii (x `xor` mask)
  where
  mask = eightBytesOf1 * fromIntegral n

-- equal to C @~0UL/255@, whcich shows up in several hacks
eightBytesOf1 :: Word64
{-# inline eightBytesOf1 #-}
eightBytesOf1 = 0x01_01_01_01_01_01_01_01

-- mask with only the high-order bit set in each byte
highBits :: Word64
highBits = eightBytesOf1 * 128
