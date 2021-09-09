{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language PatternSynonyms #-}
{-# language TypeApplications #-}
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}

module Json
  ( -- * Types
    Value(..)
  , Member(..)
  , SyntaxException(..)
    -- * Functions
  , decode
  , encode
    -- * Infix Synonyms 
  , pattern (:->)
    -- * Constants
  , emptyArray
  , emptyObject
    -- * Construction
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
  ) where

import Prelude hiding (Bool(True,False))

import Control.Exception (Exception)
import Control.Monad.ST.Run (runSmallArrayST)
import Data.Builder.ST (Builder)
import Data.Bytes.Parser (Parser)
import Data.Bytes.Types (Bytes(..))
import Data.Char (ord)
import Data.Number.Scientific (Scientific)
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word8)
import GHC.Exts (Int(I#))
import Json.Internal.String (c2w,byteArrayToShortByteString)

import qualified Data.Builder.ST as B
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as BLDR
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser.Unsafe as Unsafe
import qualified Data.Bytes.Types as BytesType
import qualified Data.Chunks as Chunks
import qualified Data.Number.Scientific as SCI
import qualified Data.Primitive as PM
import qualified Data.Text.Short.Unsafe as TS
import qualified Json.Internal.String as StrParse
import qualified Prelude

-- | The JSON syntax tree described by the ABNF in RFC 7159. Notable
-- design decisions include:
--
-- * @True@ and @False@ are their own data constructors rather than
--   being lumped together under a data constructor for boolean values.
--   This improves performance when decoding the syntax tree to a @Bool@.
-- * @Object@ uses an association list rather than a hash map. This is
--   the data type that key-value pairs can be parsed into most cheaply.
-- * @Object@ and @Array@ both use 'Chunks' rather than using @SmallArray@
--   or cons-list directly. This a middle ground between those two types. We
--   get the efficent use of cache lines that @SmallArray@ offers, and we get
--   the worst-case @O(1)@ appends that cons-list offers. Users will typically
--   fold over the elements with the @Foldable@ instance of 'Chunks', although
--   there are functions in @Data.Chunks@ that efficently perform other
--   operations.
data Value
  = Object !(SmallArray Member)
  | Array !(SmallArray Value)
  | String {-# UNPACK #-} !ShortText
  | Number {-# UNPACK #-} !Scientific
  | Null
  | True
  | False
  deriving stock (Eq,Show)

-- | Exceptions that can happen while parsing JSON. Do not pattern
-- match on values of this type. New data constructors may be added
-- at any time without a major version bump.
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
  deriving stock (Eq,Show)
  deriving anyclass (Exception)

-- | A key-value pair in a JSON object. The name of this type is
-- taken from section 4 of RFC 7159.
data Member = Member
  { key :: {-# UNPACK #-} !ShortText
  , value :: !Value
  } deriving stock (Eq,Show)

-- | An array with no elements (i.e. @[]@)
emptyArray :: Value
{-# noinline emptyArray #-}
emptyArray = Array mempty

-- | An object with no members (i.e. @{}@)
emptyObject :: Value
{-# noinline emptyObject #-}
emptyObject = Object mempty

isSpace :: Word8 -> Prelude.Bool
{-# inline isSpace #-}
isSpace w =
     w == c2w ' '
  || w == c2w '\t'
  || w == c2w '\r'
  || w == c2w '\n'

-- | Decode a JSON syntax tree from a byte sequence.
decode :: Bytes -> Either SyntaxException Value
decode = P.parseBytesEither do
  P.skipWhile isSpace
  result <- Latin.any EmptyInput >>= parser
  P.skipWhile isSpace
  P.endOfInput UnexpectedLeftovers
  pure result

-- | Encode a JSON syntax tree.
encode :: Value -> BLDR.Builder
encode = \case
  True -> BLDR.ascii4 't' 'r' 'u' 'e'
  False -> BLDR.ascii5 'f' 'a' 'l' 's' 'e'
  Null -> BLDR.ascii4 'n' 'u' 'l' 'l'
  String s -> BLDR.shortTextJsonString s
  Number n -> SCI.builderUtf8 n
  Array ys -> case PM.sizeofSmallArray ys of
    0 -> BLDR.ascii2 '[' ']'
    _ ->
      let !(# z #) = PM.indexSmallArray## ys 0
       in BLDR.ascii '['
          <>
          encode z
          <>
          foldrTail
            ( \v b -> BLDR.ascii ',' <> encode v <> b
            ) (BLDR.ascii ']') ys
  Object ys -> case PM.sizeofSmallArray ys of
    0 -> BLDR.ascii2 '{' '}'
    _ ->
      let !(# z #) = PM.indexSmallArray## ys 0
       in BLDR.ascii '{'
          <>
          encodeMember z
          <>
          foldrTail
            ( \v b -> BLDR.ascii ',' <> encodeMember v <> b
            ) (BLDR.ascii '}') ys

encodeMember :: Member -> BLDR.Builder
encodeMember Member{key,value} =
  BLDR.shortTextJsonString key
  <>
  BLDR.ascii ':'
  <>
  encode value

foldrTail :: (a -> b -> b) -> b -> PM.SmallArray a -> b
{-# inline foldrTail #-}
foldrTail f z !ary = go 1 where
  !sz = PM.sizeofSmallArray ary
  go i
    | i == sz = z
    | (# x #) <- PM.indexSmallArray## ary i
    = f x (go (i+1))

-- Precondition: skip over all space before calling this.
-- It will not skip leading space for you. It does
parser :: Char -> Parser SyntaxException s Value
parser = \case
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
    string String start
  '-' -> fmap Number (SCI.parserNegatedUtf8Bytes InvalidNumber)
  '0' -> Latin.trySatisfy (\c -> c >= '0' && c <= '9') >>= \case
    Prelude.True -> P.fail LeadingZero
    Prelude.False -> fmap Number (SCI.parserTrailingUtf8Bytes InvalidNumber 0)
  c | c >= '1' && c <= '9' ->
        fmap Number (SCI.parserTrailingUtf8Bytes InvalidNumber (ord c - 48))
  _ -> P.fail InvalidLeader

objectTrailedByBrace :: Parser SyntaxException s Value
{-# inline objectTrailedByBrace #-}
objectTrailedByBrace = do
  P.skipWhile isSpace
  Latin.any IncompleteObject >>= \case
    '}' -> pure emptyObject
    '"' -> do
      start <- Unsafe.cursor
      !theKey <- string id start
      P.skipWhile isSpace
      Latin.char ExpectedColon ':'
      P.skipWhile isSpace
      val <- Latin.any IncompleteObject >>= parser
      let !mbr = Member theKey val
      !b0 <- P.effect B.new
      b1 <- P.effect (B.push mbr b0)
      objectStep b1
    _ -> P.fail ExpectedQuoteOrRightBrace

objectStep :: Builder s Member -> Parser SyntaxException s Value
objectStep !b = do
  P.skipWhile isSpace
  Latin.any IncompleteObject >>= \case
    ',' -> do
      P.skipWhile isSpace
      Latin.char ExpectedQuote '"'
      start <- Unsafe.cursor
      !theKey <- string id start
      P.skipWhile isSpace
      Latin.char ExpectedColon ':'
      P.skipWhile isSpace
      val <- Latin.any IncompleteObject >>= parser
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
{-# inline arrayTrailedByBracket #-}
arrayTrailedByBracket = do
  P.skipWhile isSpace
  Latin.any IncompleteArray >>= \case
    ']' -> pure emptyArray
    c -> do
      !b0 <- P.effect B.new
      val <- parser c
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
  P.skipWhile isSpace
  Latin.any IncompleteArray >>= \case
    ',' -> do
      P.skipWhile isSpace
      val <- Latin.any IncompleteArray >>= parser
      P.effect (B.push val b) >>= arrayStep
    ']' -> do
      !r <- P.effect (B.freeze b)
      let !arr = Chunks.concat r
      pure (Array arr)
    _ -> P.fail ExpectedCommaOrRightBracket

-- This is adapted from the function bearing the same name
-- in json-tokens. If you find a problem with it, then
-- something if wrong in json-tokens as well.
--
-- TODO: Quit doing this CPS and inline nonsense. We should
-- be able to unbox the resulting ShortText as ByteArray# and
-- mark the function as NOINLINE. This would prevent the generated
-- code from being needlessly duplicated in three different places.
string :: (ShortText -> a) -> Int -> Parser SyntaxException s a
{-# inline string #-}
string wrap !start = do
  (canMemcpy, raw) <- Unsafe.uneffectful $ \bs ->
    case StrParse.advance bs of
      StrParse.Finish# canMemcpy# end# rest# ->
        let rest = Bytes.lift rest#
            end = I# end#
            canMemcpy = StrParse.boxCanMemcpy canMemcpy#
            raw = Bytes.unsafeTake (end - start) bs
            bytesOffset = BytesType.offset :: Bytes -> Int
         in Unsafe.Success (canMemcpy, raw) (bytesOffset rest) (Bytes.length rest)
      StrParse.EndOfInput# -> Unsafe.Failure IncompleteString
      StrParse.IsolatedEscape# -> Unsafe.Failure InvalidEscapeSequence
      StrParse.Continue# _ _ -> errorWithoutStackTrace "json string parsing terminated early"
  case canMemcpy of
    StrParse.YesMemcpy -> do
      ba <- P.effect $ do
        dst <- PM.newByteArray (Bytes.length raw)
        Bytes.unsafeCopy dst 0 raw
        PM.unsafeFreezeByteArray dst
      pure . wrap $ TS.fromShortByteStringUnsafe (byteArrayToShortByteString ba)
    _ -> case StrParse.copyAndUnescape raw of
      Left _ -> P.fail IncompleteString
      Right str -> pure $ wrap str


-- | Infix pattern synonym for 'Member'.
pattern (:->) :: ShortText -> Value -> Member
pattern key :-> value = Member{key,value}

-- | Construct a JSON object with one member.
object1 :: Member -> Value
{-# inline object1 #-}
object1 a = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 1 a
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with two members.
object2 :: Member -> Member -> Value
{-# inline object2 #-}
object2 a b = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 2 a
  PM.writeSmallArray dst 1 b
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with three members.
object3 :: Member -> Member -> Member -> Value
{-# inline object3 #-}
object3 a b c = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 3 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with four members.
object4 :: Member -> Member -> Member -> Member -> Value
{-# inline object4 #-}
object4 a b c d = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 4 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with five members.
object5 :: Member -> Member -> Member -> Member -> Member -> Value
{-# inline object5 #-}
object5 a b c d e = Object $ runSmallArrayST $ do
  dst <- PM.newSmallArray 5 a
  PM.writeSmallArray dst 1 b
  PM.writeSmallArray dst 2 c
  PM.writeSmallArray dst 3 d
  PM.writeSmallArray dst 4 e
  PM.unsafeFreezeSmallArray dst

-- | Construct a JSON object with six members.
object6 :: Member -> Member -> Member -> Member -> Member -> Member -> Value
{-# inline object6 #-}
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
{-# inline object7 #-}
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
{-# inline object8 #-}
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
object9 :: Member -> Member -> Member -> Member -> Member -> Member -> Member -> Member -> Member
        -> Value
{-# inline object9 #-}
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
object10 :: Member -> Member -> Member -> Member -> Member -> Member -> Member -> Member 
         -> Member -> Member -> Value
{-# inline object10 #-}
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
object11 :: Member -> Member -> Member -> Member -> Member -> Member -> Member -> Member
         -> Member -> Member -> Member -> Value
{-# inline object11 #-}
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

-- | Construct a JSON object with eleven members.
object12 :: Member -> Member -> Member -> Member -> Member -> Member -> Member -> Member
         -> Member -> Member -> Member -> Member -> Value
{-# inline object12 #-}
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
