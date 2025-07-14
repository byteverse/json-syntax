{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Json
  ( -- * Types
    Value (..)
  , Member (..)
  , SyntaxException (..)

    -- * Classes
  , ToValue (..)

    -- * Functions
  , decode
  , decodeNewlineDelimited
  , encode
  , toChunks
  , toShortText
  , toText
  , toBytes
  , toByteArray

    -- * Infix Synonyms
  , pattern (:->)

    -- * Constants
  , emptyArray
  , emptyObject

    -- * Value Construction
  , int
  , int8
  , int16
  , int32
  , int64
  , word8
  , word16
  , word32
  , word64
  , bool
  , text
  , shortText

    -- * Array Construction
  , arrayFromList

    -- * Object Construction
  , objectFromList
  , object1
  , object2
  , object3
  , object4
  , object5
  , object6
  , object7
  , object8
  , object9
  , object10
  , object11
  , object12
  , object13
  , object14
  , object15
  , object16
  ) where

import Prelude hiding (Bool (False, True))

import Control.Exception (Exception)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Run (runSmallArrayST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT)
import Data.Bits (unsafeShiftR, (.&.), (.|.))
import Data.Builder.ST (Builder)
import Data.Bytes.Chunks (Chunks)
import Data.Bytes.Parser (Parser)
import Data.Bytes.Types (Bytes (..))
import Data.Char (ord)
import Data.Foldable (foldlM)
import Data.Number.Scientific (Scientific)
import Data.Primitive (Array, ByteArray (ByteArray), MutableByteArray, Prim, PrimArray, SmallArray)
import Data.Text.Internal (Text(Text))
import Data.Text.Short (ShortText)
import GHC.Exts (Char (C#), Int (I#), ltWord#, minusWord#)
import GHC.Exts (Int#, ByteArray#, word8ToWord#)
import GHC.Exts (RuntimeRep(IntRep,TupleRep,BoxedRep), Levity(Unlifted))
import GHC.Exts (and#, xor#)
import GHC.Int (Int16, Int32, Int64, Int8)
import GHC.Word (Word16, Word32, Word64, Word8(W8#), Word(W#))

import qualified Data.Builder.ST as B
import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as BLDR
import qualified Data.Bytes.Chunks as ByteChunks
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser.Unsafe as Unsafe
import qualified Data.Bytes.Parser.Utf8 as Utf8
import qualified Data.Chunks as Chunks
import qualified Data.List as List
import qualified Data.Number.Scientific as SCI
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as Contiguous
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS
import qualified Prelude

{- | The JSON syntax tree described by the ABNF in RFC 7159. Notable
design decisions include:

* @True@ and @False@ are their own data constructors rather than
  being lumped together under a data constructor for boolean values.
  This improves performance when decoding the syntax tree to a @Bool@.
* @Object@ uses an association list rather than a hash map. This is
  the data type that key-value pairs can be parsed into most cheaply.
-}
data Value
  = Object !(SmallArray Member)
  | Array !(SmallArray Value)
  | String {-# UNPACK #-} !Text
  | Number {-# UNPACK #-} !Scientific
  | Null
  | True
  | False
  deriving stock (Eq, Show)

{- | Exceptions that can happen while parsing JSON. Do not pattern
match on values of this type. New data constructors may be added
at any time without a major version bump.
-}
data SyntaxException
  = EmptyInput
  | ExpectedColon
  | ExpectedCommaOrRightBracket
  | ExpectedFalse
  | ExpectedNull
  | ExpectedQuote
  | ExpectedQuoteOrRightBrace
  | ExpectedTrue
  | IncompleteArray
  | IncompleteEscapeSequence
  | IncompleteObject
  | IncompleteString
  | InvalidEscapeSequence
  | InvalidLeader
  | InvalidNumber
  | LeadingZero
  | UnexpectedLeftovers
  | PossibleOverflow
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

{- | A key-value pair in a JSON object. The name of this type is
taken from section 4 of RFC 7159.
-}
data Member = Member
  { key :: {-# UNPACK #-} !Text
  , value :: !Value
  }
  deriving stock (Eq, Show)

-- | An array with no elements (i.e. @[]@)
emptyArray :: Value
{-# NOINLINE emptyArray #-}
emptyArray = Array mempty

-- | An object with no members (i.e. @{}@)
emptyObject :: Value
{-# NOINLINE emptyObject #-}
emptyObject = Object mempty

isSpace :: Word8 -> Prelude.Bool
{-# INLINE isSpace #-}
isSpace w =
  w == c2w ' '
    || w == c2w '\t'
    || w == c2w '\r'
    || w == c2w '\n'

-- | Decode a JSON syntax tree from a byte sequence.
decode :: Bytes -> Either SyntaxException Value
{-# NOINLINE decode #-}
decode = P.parseBytesEither parser

parser :: Parser SyntaxException s Value
{-# INLINE parser #-}
parser = do
  optimizedSkipSpace
  result <- Latin.any EmptyInput >>= parserStep
  optimizedSkipSpace
  P.endOfInput UnexpectedLeftovers
  pure result

{- | Decode newline-delimited JSON. Both the LF and the CRLF conventions
are supported. The newline character (or character sequence) following
the final object may be omitted. This also allows blanks lines consisting
of only whitespace.

It's not strictly necessary for this to be a part of this library, but
newline-delimited JSON is somewhat common in practice. It's nice to have
this here instead of having to reimplement it in a bunch of different
applications.

Note: To protect against malicious input, this rejects byte sequences with
more than 10 million newlines. If this is causing a problem for you, open
an issue.

Other note: in the future, this function might be changed transparently
to parallelize the decoding of large input (at least 1000 lines) with
GHC sparks.
-}
decodeNewlineDelimited :: Bytes -> Either SyntaxException (SmallArray Value)
{-# NOINLINE decodeNewlineDelimited #-}
decodeNewlineDelimited !everything =
  let maxVals = Bytes.count 0x0A everything + 1
   in if maxVals > 10000000
        then Left PossibleOverflow
        else runST $ runExceptT $ do
          !dst <- PM.newSmallArray maxVals Null
          !total <-
            foldlM
              ( \ !ix b ->
                  let clean = Bytes.dropWhile isSpace (Bytes.dropWhileEnd isSpace b)
                   in if Bytes.null clean
                        then pure ix
                        else do
                          v <- except (decode clean)
                          lift (PM.writeSmallArray dst ix v)
                          pure (ix + 1)
              )
              0
              (Bytes.split 0x0A everything)
          lift $ PM.shrinkSmallMutableArray dst total
          dst' <- lift $ PM.unsafeFreezeSmallArray dst
          pure dst'

toChunks :: Value -> Chunks
{-# INLINE toChunks #-}
toChunks = BLDR.run 512 . encode

toBytes :: Value -> Bytes
{-# INLINE toBytes #-}
toBytes = ByteChunks.concat . toChunks

toByteArray :: Value -> ByteArray
{-# INLINE toByteArray #-}
toByteArray = ByteChunks.concatU . toChunks

toShortText :: Value -> ShortText
{-# INLINE toShortText #-}
toShortText v = case toByteArray v of
  ByteArray x -> TS.fromShortByteStringUnsafe (BSS.SBS x)

toText :: Value -> Text
{-# INLINE toText #-}
toText = TS.toText . toShortText

-- | Encode a JSON syntax tree.
encode :: Value -> BLDR.Builder
{-# NOINLINE encode #-}
encode v0 = BLDR.rebuild $ case v0 of
  True -> BLDR.ascii4 't' 'r' 'u' 'e'
  False -> BLDR.ascii5 'f' 'a' 'l' 's' 'e'
  Null -> BLDR.ascii4 'n' 'u' 'l' 'l'
  String s -> BLDR.textJsonString s
  Number n -> SCI.builderUtf8 n
  Array ys -> case PM.sizeofSmallArray ys of
    0 -> BLDR.ascii2 '[' ']'
    _ ->
      let !(# z #) = PM.indexSmallArray## ys 0
       in BLDR.ascii '['
            <> encode z
            <> foldrTail
              ( \v b -> BLDR.ascii ',' <> encode v <> b
              )
              (BLDR.ascii ']')
              ys
  Object ys -> case PM.sizeofSmallArray ys of
    0 -> BLDR.ascii2 '{' '}'
    _ ->
      let !(# z #) = PM.indexSmallArray## ys 0
       in BLDR.ascii '{'
            <> encodeMember z
            <> foldrTail
              ( \v b -> BLDR.ascii ',' <> encodeMember v <> b
              )
              (BLDR.ascii '}')
              ys

encodeMember :: Member -> BLDR.Builder
encodeMember Member {key, value} =
  BLDR.textJsonString key
    <> BLDR.ascii ':'
    <> encode value

foldrTail :: (a -> b -> b) -> b -> PM.SmallArray a -> b
{-# INLINE foldrTail #-}
foldrTail f z !ary = goFoldrTail 1
 where
  !sz = PM.sizeofSmallArray ary
  goFoldrTail i
    | i == sz = z
    | (# x #) <- PM.indexSmallArray## ary i =
        f x (goFoldrTail (i + 1))

-- Precondition: skip over all space before calling this.
-- It will not skip leading space for you. It does not skip
-- over trailing space either.
parserStep :: Char -> Parser SyntaxException s Value
{-# NOINLINE parserStep #-}
parserStep = \case
  '{' -> objectTrailedByBrace
  '[' -> arrayTrailedByBracket
  't' -> do
    Latin.char3 ExpectedTrue 'r' 'u' 'e'
    pure True
  'f' -> do
    Latin.char4 ExpectedFalse 'a' 'l' 's' 'e'
    pure False
  'n' -> do
    Latin.char3 ExpectedNull 'u' 'l' 'l'
    pure Null
  '"' -> do
    start <- Unsafe.cursor
    string start `P.bindFromByteArrayIntIntToLifted` \ !(# arr, off, len #) -> pure (String (Text (ByteArray arr) (I# off) (I# len)))
  '-' -> fmap Number (SCI.parserNegatedUtf8Bytes InvalidNumber)
  '0' ->
    Latin.trySatisfy (\c -> c >= '0' && c <= '9') >>= \case
      Prelude.True -> P.fail LeadingZero
      Prelude.False -> fmap Number (SCI.parserTrailingUtf8Bytes InvalidNumber 0)
  c
    | c >= '1' && c <= '9' ->
        fmap Number (SCI.parserTrailingUtf8Bytes InvalidNumber (ord c - 48))
  _ -> P.fail InvalidLeader

objectTrailedByBrace :: Parser SyntaxException s Value
{-# INLINE objectTrailedByBrace #-}
objectTrailedByBrace = do
  optimizedSkipSpace
  Latin.any IncompleteObject >>= \case
    '}' -> pure emptyObject
    '"' -> do
      start <- Unsafe.cursor
      string start `P.bindFromByteArrayIntIntToLifted` \ !(# arr, off, len #) -> do
        let theKey = Text (ByteArray arr) (I# off) (I# len)
        optimizedSkipSpace
        Latin.char ExpectedColon ':'
        optimizedSkipSpace
        val <- Latin.any IncompleteObject >>= parserStep
        let !mbr = Member theKey val
        !b0 <- P.effect B.new
        b1 <- P.effect (B.push mbr b0)
        objectStep b1
    _ -> P.fail ExpectedQuoteOrRightBrace

optimizedSkipSpace :: Parser SyntaxException s ()
{-# noinline optimizedSkipSpace #-}
optimizedSkipSpace = do
  let goOptimizedSkipSpace = do
        result <- Latin.trySatisfy
          (\c -> case c of
            ' ' -> Prelude.True
            _ -> if c > ' '
              then Prelude.False
              else case c of
                '\r' -> Prelude.True
                '\t' -> Prelude.True
                '\n' -> Prelude.True
                _ -> Prelude.False
          )
        case result of
          Prelude.True -> goOptimizedSkipSpace
          Prelude.False -> pure ()
  goOptimizedSkipSpace

objectStep :: Builder s Member -> Parser SyntaxException s Value
objectStep !b = do
  optimizedSkipSpace
  Latin.any IncompleteObject >>= \case
    ',' -> do
      optimizedSkipSpace
      Latin.char ExpectedQuote '"'
      start <- Unsafe.cursor
      !theKey <- P.bindFromByteArrayIntIntToLifted (string start) (\(# arr, off, len #) -> pure (Text (ByteArray arr) (I# off) (I# len)))
      optimizedSkipSpace
      Latin.char ExpectedColon ':'
      optimizedSkipSpace
      val <- Latin.any IncompleteObject >>= parserStep
      let !mbr = Member theKey val
      P.effect (B.push mbr b) >>= objectStep
    '}' -> do
      !r <- P.effect (B.freeze b)
      let !arr = Chunks.concat r
      pure (Object arr)
    _ -> P.fail ExpectedCommaOrRightBracket

-- This eats all the space at the front of the input. There
-- is no need to skip over it before calling this function.
-- RFC 7159 defines array as:
--
-- > begin-array = ws LBRACKET ws
-- > array = begin-array [ value *( value-separator value ) ] end-array
--
-- This parser handles everything after the LBRACKET character.
arrayTrailedByBracket :: Parser SyntaxException s Value
{-# INLINE arrayTrailedByBracket #-}
arrayTrailedByBracket = do
  optimizedSkipSpace
  Latin.any IncompleteArray >>= \case
    ']' -> pure emptyArray
    c -> do
      !b0 <- P.effect B.new
      val <- parserStep c
      b1 <- P.effect (B.push val b0)
      arrayStep b1

-- From RFC 7159:
--
-- > value-separator = ws COMMA ws
-- > array = begin-array [ value *( value-separator value ) ] end-array
--
-- This handles the all values after the first one. That is:
--
-- > *( value-separator value )
arrayStep :: Builder s Value -> Parser SyntaxException s Value
arrayStep !b = do
  optimizedSkipSpace
  Latin.any IncompleteArray >>= \case
    ',' -> do
      optimizedSkipSpace
      val <- Latin.any IncompleteArray >>= parserStep
      P.effect (B.push val b) >>= arrayStep
    ']' -> do
      !r <- P.effect (B.freeze b)
      let !arr = Chunks.concat r
      pure (Array arr)
    _ -> P.fail ExpectedCommaOrRightBracket

c2w :: Char -> Word8
{-# INLINE c2w #-}
c2w = fromIntegral . ord

string :: forall s. Int -> Parser SyntaxException s (# ByteArray#, Int#, Int# #)
{-# NOINLINE string #-}
string !start@(I# start# ) = goShare
 where
  goNoShare :: Parser SyntaxException s (# ByteArray#, Int#, Int# #)
  goNoShare = do
    P.any IncompleteString `P.bindFromLiftedToByteArrayIntInt` \theChar -> case theChar of
      92 -> P.any InvalidEscapeSequence `P.bindFromLiftedToByteArrayIntInt` \_ -> goNoShare -- backslash
      34 ->
        -- double quote (string is finished)
        Unsafe.cursor `P.bindFromLiftedToByteArrayIntInt` \ !pos ->
        Unsafe.unconsume (pos - start) `P.bindFromLiftedToByteArrayIntInt` \ !_ ->
        let end = pos - 1
            maxLen = end - start
         in copyAndEscape# maxLen
      _ -> goNoShare
  goShare :: Parser SyntaxException s (# ByteArray#, Int#, Int# #)
  goShare = do
    P.any IncompleteString `P.bindFromLiftedToByteArrayIntInt` \theChar -> case theChar of
      92 -> P.any InvalidEscapeSequence `P.bindFromLiftedToByteArrayIntInt` \_ -> goNoShare -- backslash
      34 ->
        -- double quote (string is finished)
        Unsafe.cursor `P.bindFromLiftedToByteArrayIntInt` \ !pos ->
        Unsafe.expose `P.bindFromLiftedToByteArrayIntInt` \ !(ByteArray src) ->
        let !end = pos - 1
            !(I# len) = end - start
         in P.pureByteArrayIntInt (# src, start#, len #)
      W8# w ->
        let !w' = minusWord# (word8ToWord# w) 32##
         in case ltWord# w' 96## of
              1# -> goShare
              _ -> goNoShare

copyAndEscape# :: forall s. Int -> Parser @('TupleRep '[ 'BoxedRep 'Unlifted, 'IntRep, 'IntRep ]) SyntaxException s (# ByteArray#, Int#, Int# #)
{-# noinline copyAndEscape# #-}
copyAndEscape# !maxLen =
  (P.effect (PM.newByteArray maxLen))
  `P.bindFromLiftedToByteArrayIntInt` \dst ->
  let goCopyAndEscape :: Int -> Parser @('TupleRep '[ 'BoxedRep 'Unlifted, 'IntRep, 'IntRep ]) SyntaxException s (# ByteArray#, Int#, Int# #)
      goCopyAndEscape !ix@(I# ix#) =
        P.any IncompleteString `P.bindFromLiftedToByteArrayIntInt` \theCharW -> case theCharW of
          -- Backslash
          0x5C -> Latin.any IncompleteEscapeSequence `P.bindFromLiftedToByteArrayIntInt` \escapedChar -> case escapedChar of
            '"' ->
              (P.effect (PM.writeByteArray dst ix (c2w '"')))
              `P.bindFromLiftedToByteArrayIntInt` \_ ->
              goCopyAndEscape (ix + 1)
            '\\' ->
              (P.effect (PM.writeByteArray dst ix (c2w '\\')))
              `P.bindFromLiftedToByteArrayIntInt` \_ ->
              goCopyAndEscape (ix + 1)
            't' ->
              (P.effect (PM.writeByteArray dst ix (c2w '\t')))
              `P.bindFromLiftedToByteArrayIntInt` \_ ->
              goCopyAndEscape (ix + 1)
            'n' ->
              (P.effect (PM.writeByteArray dst ix (c2w '\n')))
              `P.bindFromLiftedToByteArrayIntInt` \_ ->
              goCopyAndEscape (ix + 1)
            'r' ->
              (P.effect (PM.writeByteArray dst ix (c2w '\r')))
              `P.bindFromLiftedToByteArrayIntInt` \_ ->
              goCopyAndEscape (ix + 1)
            '/' ->
              (P.effect (PM.writeByteArray dst ix (c2w '/')))
              `P.bindFromLiftedToByteArrayIntInt` \_ ->
              goCopyAndEscape (ix + 1)
            'b' ->
              (P.effect (PM.writeByteArray dst ix (c2w '\b')))
              `P.bindFromLiftedToByteArrayIntInt` \_ ->
              goCopyAndEscape (ix + 1)
            'f' ->
              (P.effect (PM.writeByteArray dst ix (c2w '\f')))
              `P.bindFromLiftedToByteArrayIntInt` \_ ->
              goCopyAndEscape (ix + 1)
            'u' ->
              Latin.hexFixedWord16# InvalidEscapeSequence
              `P.bindFromWordToByteArrayIntInt` \w ->
              ( case (w `and#` 0b1111_1000_0000_0000## ) `xor#` 0b1101_1000_0000_0000## of
                  0## -> P.bindFromLiftedToByteArrayIntInt
                    ( P.effect $ do
                        -- We replace anything in the range U+D800-U+DFFF with U+FFFD
                        -- Note: UTF8 encoding of character U+FFFD is: 0xEF 0xBF 0xBD,
                        -- so we just inline it directly.
                        PM.writeByteArray dst ix (0xEF :: Word8)
                        PM.writeByteArray dst (ix + 1) (0xBF :: Word8)
                        PM.writeByteArray dst (ix + 2) (0xBD :: Word8)
                        pure (ix + 3)
                    ) goCopyAndEscape
                  _ -> P.bindFromLiftedToByteArrayIntInt (P.effect (encodeUtf8CharBmp dst ix (W# w))) goCopyAndEscape
              )
            _ -> P.failByteArrayIntInt InvalidEscapeSequence
          -- Double quote (terminates string)
          0x22 ->
            (P.effect (PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray dst ix))
            `P.bindFromLiftedToByteArrayIntInt` \str ->
            P.pureByteArrayIntInt
              ( case byteArrayToShortByteString str of
                  BSS.SBS str' -> (# str', 0#, ix# #)
              )
          _ | theCharW >= 0x20, theCharW < 127 -> 
                (P.effect (PM.writeByteArray dst ix theCharW))
                `P.bindFromLiftedToByteArrayIntInt` \_ ->
                (goCopyAndEscape (ix + 1))
          _ ->
            (Unsafe.unconsume 1)
            `P.bindFromLiftedToByteArrayIntInt` \_ ->
            (Utf8.any# IncompleteString)
            `P.bindFromCharToByteArrayIntInt` \c -> 
            (P.bindFromLiftedToByteArrayIntInt (P.effect (encodeUtf8Char dst ix (fromIntegral (ord (C# c)) :: Word))) goCopyAndEscape)
   in goCopyAndEscape 0

-- This is copy of encodeUtf8Char that only expects characters in the basic
-- multilingual plane.
encodeUtf8CharBmp :: MutableByteArray s -> Int -> Word -> ST s Int
encodeUtf8CharBmp !marr !ix !c
  | c < 128 = do
      PM.writeByteArray marr ix (fromIntegral c :: Word8)
      pure (ix + 1)
  | c < 0x0800 = do
      PM.writeByteArray
        marr
        ix
        (fromIntegral @Word @Word8 (unsafeShiftR c 6 .|. 0b11000000))
      PM.writeByteArray
        marr
        (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Word @Word8 c)))
      pure (ix + 2)
  | otherwise = do
      PM.writeByteArray
        marr
        ix
        (fromIntegral @Word @Word8 (unsafeShiftR c 12 .|. 0b11100000))
      PM.writeByteArray
        marr
        (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Word @Word8 (unsafeShiftR c 6))))
      PM.writeByteArray
        marr
        (ix + 2)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Word @Word8 c)))
      pure (ix + 3)

-- Note: the third argument is actually a character. This function used to
-- accept a Char argument, but it was rewritten to help get rid of chr#
-- and ord# in GHC core.
encodeUtf8Char :: MutableByteArray s -> Int -> Word -> ST s Int
encodeUtf8Char !marr !ix !c
  | c < 128 = do
      PM.writeByteArray marr ix (fromIntegral c :: Word8)
      pure (ix + 1)
  | c < 0x0800 = do
      PM.writeByteArray
        marr
        ix
        (fromIntegral @Word @Word8 (unsafeShiftR c 6 .|. 0b11000000))
      PM.writeByteArray
        marr
        (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Word @Word8 c)))
      pure (ix + 2)
  | c <= 0xffff = do
      PM.writeByteArray
        marr
        ix
        (fromIntegral @Word @Word8 (unsafeShiftR c 12 .|. 0b11100000))
      PM.writeByteArray
        marr
        (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Word @Word8 (unsafeShiftR c 6))))
      PM.writeByteArray
        marr
        (ix + 2)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Word @Word8 c)))
      pure (ix + 3)
  | otherwise = do
      PM.writeByteArray
        marr
        ix
        (fromIntegral @Word @Word8 (unsafeShiftR c 18 .|. 0b11110000))
      PM.writeByteArray
        marr
        (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Word @Word8 (unsafeShiftR c 12))))
      PM.writeByteArray
        marr
        (ix + 2)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Word @Word8 (unsafeShiftR c 6))))
      PM.writeByteArray
        marr
        (ix + 3)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Word @Word8 c)))
      pure (ix + 4)

byteArrayToShortByteString :: ByteArray -> BSS.ShortByteString
{-# inline byteArrayToShortByteString #-}
byteArrayToShortByteString (PM.ByteArray x) = BSS.SBS x

-- | Infix pattern synonym for 'Member'.
pattern (:->) :: Text -> Value -> Member
pattern key :-> value = Member {key, value}

{- | Construct a JSON array from a list of JSON values.

Unlike 'objectFromList', this is not currently equipped with a
rewrite rule.
-}
arrayFromList :: [Value] -> Value
arrayFromList ms = Array $ PM.smallArrayFromList ms

{- | Construct a JSON object from a list of members.

Note: When the argument is a list literal with 16 or fewer elements,
a rewrite rule transforms this into the appropriate @objectN@ function.
When the argument is not a list literal, this function just calls
@smallArrayFromList@ on the members, which has poor performance.
-}
objectFromList :: [Member] -> Value
objectFromList ms = Object $ PM.smallArrayFromList ms
{-# NOINLINE objectFromList #-}

{-# RULES
"objectFromList/1" forall a.
  objectFromList (a : []) =
    object1 a
  #-}
{-# RULES
"objectFromList/2" forall a b.
  objectFromList (a : b : []) =
    object2 a b
  #-}
{-# RULES
"objectFromList/3" forall a b c.
  objectFromList (a : b : c : []) =
    object3 a b c
  #-}
{-# RULES
"objectFromList/4" forall a b c d.
  objectFromList (a : b : c : d : []) =
    object4 a b c d
  #-}
{-# RULES
"objectFromList/5" forall a b c d e.
  objectFromList (a : b : c : d : e : []) =
    object5 a b c d e
  #-}
{-# RULES
"objectFromList/6" forall a b c d e f.
  objectFromList (a : b : c : d : e : f : []) =
    object6 a b c d e f
  #-}
{-# RULES
"objectFromList/7" forall a b c d e f g.
  objectFromList (a : b : c : d : e : f : g : []) =
    object7 a b c d e f g
  #-}
{-# RULES
"objectFromList/8" forall a b c d e f g h.
  objectFromList (a : b : c : d : e : f : g : h : []) =
    object8 a b c d e f g h
  #-}
{-# RULES
"objectFromList/9" forall a b c d e f g h i.
  objectFromList (a : b : c : d : e : f : g : h : i : []) =
    object9 a b c d e f g h i
  #-}
{-# RULES
"objectFromList/10" forall a b c d e f g h i j.
  objectFromList (a : b : c : d : e : f : g : h : i : j : []) =
    object10 a b c d e f g h i j
  #-}
{-# RULES
"objectFromList/11" forall a b c d e f g h i j k.
  objectFromList (a : b : c : d : e : f : g : h : i : j : k : []) =
    object11 a b c d e f g h i j k
  #-}
{-# RULES
"objectFromList/12" forall a b c d e f g h i j k l.
  objectFromList (a : b : c : d : e : f : g : h : i : j : k : l : []) =
    object12 a b c d e f g h i j k l
  #-}
{-# RULES
"objectFromList/13" forall a b c d e f g h i j k l m.
  objectFromList (a : b : c : d : e : f : g : h : i : j : k : l : m : []) =
    object13 a b c d e f g h i j k l m
  #-}
{-# RULES
"objectFromList/14" forall a b c d e f g h i j k l m n.
  objectFromList (a : b : c : d : e : f : g : h : i : j : k : l : m : n : []) =
    object14 a b c d e f g h i j k l m n
  #-}
{-# RULES
"objectFromList/15" forall a b c d e f g h i j k l m n o.
  objectFromList (a : b : c : d : e : f : g : h : i : j : k : l : m : n : o : []) =
    object15 a b c d e f g h i j k l m n o
  #-}
{-# RULES
"objectFromList/16" forall a b c d e f g h i j k l m n o p.
  objectFromList (a : b : c : d : e : f : g : h : i : j : k : l : m : n : o : p : []) =
    object16 a b c d e f g h i j k l m n o p
  #-}

-- | Construct a JSON object with one member.
object1 :: Member -> Value
{-# INLINE object1 #-}
object1 a = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 1 a
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with two members.
object2 :: Member -> Member -> Value
{-# INLINE object2 #-}
object2 a b = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 2 a
  PM.writeSmallArray dst 1 b
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with three members.
object3 :: Member -> Member -> Member -> Value
{-# INLINE object3 #-}
object3 a b c = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 3 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with four members.
object4 :: Member -> Member -> Member -> Member -> Value
{-# INLINE object4 #-}
object4 a b c d = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 4 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with five members.
object5 :: Member -> Member -> Member -> Member -> Member -> Value
{-# INLINE object5 #-}
object5 a b c d e = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 5 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with six members.
object6 :: Member -> Member -> Member -> Member -> Member -> Member -> Value
{-# INLINE object6 #-}
object6 a b c d e f = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 6 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.writeSmallArray dst 5 f
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with seven members.
object7 :: Member -> Member -> Member -> Member -> Member -> Member -> Member -> Value
{-# INLINE object7 #-}
object7 a b c d e f g = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 7 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.writeSmallArray dst 5 f
  PM.writeSmallArray dst 6 g
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with nine members.
object8 :: Member -> Member -> Member -> Member -> Member -> Member -> Member -> Member -> Value
{-# INLINE object8 #-}
object8 a b c d e f g h = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 8 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.writeSmallArray dst 5 f
  PM.writeSmallArray dst 6 g
  PM.writeSmallArray dst 7 h
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with nine members.
object9 ::
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Value
{-# INLINE object9 #-}
object9 a b c d e f g h i = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 9 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.writeSmallArray dst 5 f
  PM.writeSmallArray dst 6 g
  PM.writeSmallArray dst 7 h
  PM.writeSmallArray dst 8 i
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with ten members.
object10 ::
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Value
{-# INLINE object10 #-}
object10 a b c d e f g h i j = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 10 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.writeSmallArray dst 5 f
  PM.writeSmallArray dst 6 g
  PM.writeSmallArray dst 7 h
  PM.writeSmallArray dst 8 i
  PM.writeSmallArray dst 9 j
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with eleven members.
object11 ::
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Value
{-# INLINE object11 #-}
object11 a b c d e f g h i j k = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 11 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.writeSmallArray dst 5 f
  PM.writeSmallArray dst 6 g
  PM.writeSmallArray dst 7 h
  PM.writeSmallArray dst 8 i
  PM.writeSmallArray dst 9 j
  PM.writeSmallArray dst 10 k
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with twelve members.
object12 ::
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Value
{-# INLINE object12 #-}
object12 a b c d e f g h i j k l = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 12 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.writeSmallArray dst 5 f
  PM.writeSmallArray dst 6 g
  PM.writeSmallArray dst 7 h
  PM.writeSmallArray dst 8 i
  PM.writeSmallArray dst 9 j
  PM.writeSmallArray dst 10 k
  PM.writeSmallArray dst 11 l
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with thirteen members.
object13 ::
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Value
{-# INLINE object13 #-}
object13 a b c d e f g h i j k l m = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 13 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.writeSmallArray dst 5 f
  PM.writeSmallArray dst 6 g
  PM.writeSmallArray dst 7 h
  PM.writeSmallArray dst 8 i
  PM.writeSmallArray dst 9 j
  PM.writeSmallArray dst 10 k
  PM.writeSmallArray dst 11 l
  PM.writeSmallArray dst 12 m
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with fourteen members.
object14 ::
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Value
{-# INLINE object14 #-}
object14 a b c d e f g h i j k l m n = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 14 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.writeSmallArray dst 5 f
  PM.writeSmallArray dst 6 g
  PM.writeSmallArray dst 7 h
  PM.writeSmallArray dst 8 i
  PM.writeSmallArray dst 9 j
  PM.writeSmallArray dst 10 k
  PM.writeSmallArray dst 11 l
  PM.writeSmallArray dst 12 m
  PM.writeSmallArray dst 13 n
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with fifteen members.
object15 ::
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Value
{-# INLINE object15 #-}
object15 a b c d e f g h i j k l m n o = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 15 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.writeSmallArray dst 5 f
  PM.writeSmallArray dst 6 g
  PM.writeSmallArray dst 7 h
  PM.writeSmallArray dst 8 i
  PM.writeSmallArray dst 9 j
  PM.writeSmallArray dst 10 k
  PM.writeSmallArray dst 11 l
  PM.writeSmallArray dst 12 m
  PM.writeSmallArray dst 13 n
  PM.writeSmallArray dst 14 o
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with sixteen members.
object16 ::
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Member ->
  Value
{-# INLINE object16 #-}
object16 a b c d e f g h i j k l m n o p = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 16 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.writeSmallArray dst 5 f
  PM.writeSmallArray dst 6 g
  PM.writeSmallArray dst 7 h
  PM.writeSmallArray dst 8 i
  PM.writeSmallArray dst 9 j
  PM.writeSmallArray dst 10 k
  PM.writeSmallArray dst 11 l
  PM.writeSmallArray dst 12 m
  PM.writeSmallArray dst 13 n
  PM.writeSmallArray dst 14 o
  PM.writeSmallArray dst 15 p
  PM.unsafeFreezeSmallArray dst

word8 :: Word8 -> Json.Value
{-# INLINE word8 #-}
word8 = Json.Number . SCI.fromWord8

word16 :: Word16 -> Json.Value
{-# INLINE word16 #-}
word16 = Json.Number . SCI.fromWord16

word32 :: Word32 -> Json.Value
{-# INLINE word32 #-}
word32 = Json.Number . SCI.fromWord32

word64 :: Word64 -> Json.Value
{-# INLINE word64 #-}
word64 = Json.Number . SCI.fromWord64

int8 :: Int8 -> Json.Value
{-# INLINE int8 #-}
int8 = Json.Number . SCI.fromInt8

int16 :: Int16 -> Json.Value
{-# INLINE int16 #-}
int16 = Json.Number . SCI.fromInt16

int32 :: Int32 -> Json.Value
{-# INLINE int32 #-}
int32 = Json.Number . SCI.fromInt32

int64 :: Int64 -> Json.Value
{-# INLINE int64 #-}
int64 = Json.Number . SCI.fromInt64

int :: Int -> Json.Value
{-# INLINE int #-}
int = Json.Number . SCI.fromInt

text :: Text -> Json.Value
{-# INLINE text #-}
text = Json.String

shortText :: ShortText -> Json.Value
{-# INLINE shortText #-}
shortText = String . TS.toText

bool :: Prelude.Bool -> Json.Value
{-# INLINE bool #-}
bool Prelude.True = True
bool _ = False

-- | Typeclass for types that can be encoded as JSON.
class ToValue a where
  toValue :: a -> Value

-- | Encodes the unit value as JSON @null@.
instance ToValue () where toValue _ = Null

instance ToValue Value where toValue = id
instance ToValue Scientific where toValue = Number
instance ToValue Int where toValue = int
instance ToValue Int8 where toValue = int8
instance ToValue Int16 where toValue = int16
instance ToValue Int32 where toValue = int32
instance ToValue Int64 where toValue = int64
instance ToValue Word8 where toValue = word8
instance ToValue Word16 where toValue = word16
instance ToValue Word32 where toValue = word32
instance ToValue Word64 where toValue = word64
instance ToValue ShortText where toValue = shortText
instance ToValue Text where toValue = text
instance ToValue Prelude.Bool where toValue = bool
instance ToValue Word where
  toValue = word64 . fromIntegral @Word @Word64

listToJsonValue :: ToValue a => [a] -> Value
{-# noinline listToJsonValue #-}
listToJsonValue xs = runST $ do
  let len = List.length xs
  dst <- PM.newSmallArray len Null
  let goListToJsonValue !ix ys = case ys of
        [] -> do
          dst' <- PM.unsafeFreezeSmallArray dst
          pure (Array dst')
        z : zs -> do
          PM.writeSmallArray dst ix $! toValue z
          goListToJsonValue (ix + 1) zs
  goListToJsonValue 0 xs

instance (ToValue a) => ToValue [a] where
  {-# inline toValue #-}
  toValue = listToJsonValue

instance (ToValue a) => ToValue (SmallArray a) where
  toValue !xs = Json.Array $! Contiguous.map' toValue xs

instance (ToValue a) => ToValue (Array a) where
  toValue !xs = Json.Array $! Contiguous.map' toValue xs

instance (Prim a, ToValue a) => ToValue (PrimArray a) where
  toValue !xs = Json.Array $! Contiguous.map' toValue xs
