{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language BlockArguments #-}
{-# language DerivingStrategies #-}
{-# language DeriveAnyClass #-}
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
    -- * Classes
  , ToValue(..)
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

import Prelude hiding (Bool(True,False))

import Control.Exception (Exception)
import Control.Monad.ST (ST,runST)
import Control.Monad.ST.Run (runSmallArrayST)
import Data.Bits ((.&.),(.|.),unsafeShiftR)
import Data.Builder.ST (Builder)
import Data.Bytes.Parser (Parser)
import Data.Bytes.Types (Bytes(..))
import Data.Char (ord)
import Data.Number.Scientific (Scientific)
import Data.Primitive (ByteArray(ByteArray),MutableByteArray,SmallArray,Array,PrimArray,Prim)
import Data.Text.Short (ShortText)
import GHC.Exts (Char(C#),Int(I#),gtWord#,ltWord#,word2Int#,chr#)
import GHC.Word (Word8,Word16,Word32,Word64)
import GHC.Int (Int8,Int16,Int32,Int64)
import Data.Text (Text)
import Data.Foldable (foldlM)
import Control.Monad.Trans.Except (runExceptT,except)
import Control.Monad.Trans.Class (lift)
import Data.Bytes.Chunks (Chunks)

import qualified Prelude
import qualified Data.Builder.ST as B
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as ByteChunks
import qualified Data.Bytes.Builder as BLDR
import qualified Data.Bytes.Parser as P
import qualified Data.Chunks as Chunks
import qualified Data.Text.Short.Unsafe as TS
import qualified Data.Number.Scientific as SCI
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as Contiguous
import qualified Data.Bytes.Parser.Utf8 as Utf8
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.Bytes.Parser.Unsafe as Unsafe
import qualified Data.Text.Short as TS
import qualified Data.List as List
import qualified GHC.Word.Compat

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
  | PossibleOverflow
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
{-# noinline decode #-}
decode = P.parseBytesEither parser

parser :: Parser SyntaxException s Value
{-# inline parser #-}
parser = do
  P.skipWhile isSpace
  result <- Latin.any EmptyInput >>= parserStep
  P.skipWhile isSpace
  P.endOfInput UnexpectedLeftovers
  pure result

-- | Decode newline-delimited JSON. Both the LF and the CRLF conventions
-- are supported. The newline character (or character sequence) following
-- the final object may be omitted. This also allows blanks lines consisting
-- of only whitespace.
--
-- It's not strictly necessary for this to be a part of this library, but
-- newline-delimited JSON is somewhat common in practice. It's nice to have
-- this here instead of having to reimplement it in a bunch of different
-- applications.
--
-- Note: To protect against malicious input, this reject byte sequences with
-- more than 10 million newlines. If this is causing a problem for you, open
-- an issue.
--
-- Other note: in the future, this function might be changed transparently
-- to parallelize the decoding of large input (at least 1000 lines) with
-- GHC sparks.
decodeNewlineDelimited :: Bytes -> Either SyntaxException (SmallArray Value)
{-# noinline decodeNewlineDelimited #-}
decodeNewlineDelimited !everything =
  let maxVals = Bytes.count 0x0A everything + 1
   in if maxVals > 10000000
        then Left PossibleOverflow
        else runST $ runExceptT $ do
          !dst <- PM.newSmallArray maxVals Null
          !total <- foldlM
            (\ !ix b ->
              let clean = Bytes.dropWhile isSpace (Bytes.dropWhileEnd isSpace b)
               in if Bytes.null clean
                    then pure ix
                    else do
                      v <- except (decode clean)
                      lift (PM.writeSmallArray dst ix v)
                      pure (ix + 1)
            ) 0 (Bytes.split 0x0A everything)
          lift $ PM.shrinkSmallMutableArray dst total
          dst' <- lift $ PM.unsafeFreezeSmallArray dst
          pure dst'

toChunks :: Value -> Chunks
{-# inline toChunks #-}
toChunks = BLDR.run 512 . encode

toBytes :: Value -> Bytes
{-# inline toBytes #-}
toBytes = ByteChunks.concat . toChunks

toByteArray :: Value -> ByteArray
{-# inline toByteArray #-}
toByteArray = ByteChunks.concatU . toChunks

toShortText :: Value -> ShortText
{-# inline toShortText #-}
toShortText v = case toByteArray v of
  ByteArray x -> TS.fromShortByteStringUnsafe (BSS.SBS x)

toText :: Value -> Text
{-# inline toText #-}
toText = TS.toText . toShortText

-- | Encode a JSON syntax tree.
encode :: Value -> BLDR.Builder
{-# noinline encode #-}
encode v0 = BLDR.rebuild $ case v0 of
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
-- It will not skip leading space for you. It does not skip
-- over trailing space either.
parserStep :: Char -> Parser SyntaxException s Value
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
      val <- Latin.any IncompleteObject >>= parserStep
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
{-# inline arrayTrailedByBracket #-}
arrayTrailedByBracket = do
  P.skipWhile isSpace
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
  P.skipWhile isSpace
  Latin.any IncompleteArray >>= \case
    ',' -> do
      P.skipWhile isSpace
      val <- Latin.any IncompleteArray >>= parserStep
      P.effect (B.push val b) >>= arrayStep
    ']' -> do
      !r <- P.effect (B.freeze b)
      let !arr = Chunks.concat r
      pure (Array arr)
    _ -> P.fail ExpectedCommaOrRightBracket

c2w :: Char -> Word8
c2w = fromIntegral . ord

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
string wrap !start = go 1 where
  go !canMemcpy = do
    P.any IncompleteString >>= \case
      92 -> P.any InvalidEscapeSequence *> go 0 -- backslash
      34 -> do -- double quote
        !pos <- Unsafe.cursor
        case canMemcpy of
          1 -> do
            src <- Unsafe.expose
            str <- P.effect $ do
              let end = pos - 1
              let len = end - start
              dst <- PM.newByteArray len
              PM.copyByteArray dst 0 src start len
              PM.unsafeFreezeByteArray dst
            pure (wrap (TS.fromShortByteStringUnsafe (byteArrayToShortByteString str)))
          _ -> do
            Unsafe.unconsume (pos - start)
            let end = pos - 1
            let maxLen = end - start
            copyAndEscape wrap maxLen
      GHC.Word.Compat.W8# w -> go (canMemcpy .&. I# (ltWord# w 128##) .&. I# (gtWord# w 31##))

copyAndEscape :: (ShortText -> a) -> Int -> Parser SyntaxException s a
{-# inline copyAndEscape #-}
copyAndEscape wrap !maxLen = do
  !dst <- P.effect (PM.newByteArray maxLen)
  let go !ix = Utf8.any# IncompleteString `P.bindFromCharToLifted` \c -> case c of
        '\\'# -> Latin.any IncompleteEscapeSequence >>= \case
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
            w <- Latin.hexFixedWord16 InvalidEscapeSequence
            if w >= 0xD800 && w < 0xDFFF
              then go =<< P.effect (encodeUtf8Char dst ix '\xFFFD')
              else go =<< P.effect (encodeUtf8Char dst ix (w16ToChar w))
          _ -> P.fail InvalidEscapeSequence
        '"'# -> do
          str <- P.effect
            (PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray dst ix)
          pure (wrap (TS.fromShortByteStringUnsafe (byteArrayToShortByteString str)))
        _ -> go =<< P.effect (encodeUtf8Char dst ix (C# c))
  go 0

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

byteArrayToShortByteString :: ByteArray -> BSS.ShortByteString
byteArrayToShortByteString (PM.ByteArray x) = BSS.SBS x

-- Precondition: Not in the range [U+D800 .. U+DFFF]
w16ToChar :: Word16 -> Char
w16ToChar (GHC.Word.Compat.W16# w) = C# (chr# (word2Int# w))

-- | Infix pattern synonym for 'Member'.
pattern (:->) :: ShortText -> Value -> Member
pattern key :-> value = Member{key,value}

-- | Construct a JSON array from a list of JSON values.
--
-- Unlike 'objectFromList', this is not currently equipped with a
-- rewrite rule.
arrayFromList :: [Value] -> Value
arrayFromList ms = Array $ PM.smallArrayFromList ms

-- | Construct a JSON object from a list of members.
--
-- Note: When the argument is a list literal with 16 or fewer elements,
-- a rewrite rule transforms this into the appropriate @objectN@ function.
-- When the argument is not a list literal, this function just calls
-- @smallArrayFromList@ on the members, which has poor performance.
objectFromList :: [Member] -> Value
objectFromList ms = Object $ PM.smallArrayFromList ms

{-# NOINLINE objectFromList #-}
{-# RULES "objectFromList/1" forall a.
      objectFromList (a : []) =
      object1 a
#-}
{-# RULES "objectFromList/2" forall a b.
      objectFromList (a : b : []) =
      object2 a b
#-}
{-# RULES "objectFromList/3" forall a b c.
      objectFromList (a : b : c : []) =
      object3 a b c
#-}
{-# RULES "objectFromList/4" forall a b c d.
      objectFromList (a : b : c : d : []) =
      object4 a b c d
#-}
{-# RULES "objectFromList/5" forall a b c d e.
      objectFromList (a : b : c : d : e : []) =
      object5 a b c d e
#-}
{-# RULES "objectFromList/6" forall a b c d e f.
      objectFromList (a : b : c : d : e : f : []) =
      object6 a b c d e f
#-}
{-# RULES "objectFromList/7" forall a b c d e f g.
      objectFromList (a : b : c : d : e : f : g : []) =
      object7 a b c d e f g
#-}
{-# RULES "objectFromList/8" forall a b c d e f g h.
      objectFromList (a : b : c : d : e : f : g : h : []) =
      object8 a b c d e f g h
#-}
{-# RULES "objectFromList/9" forall a b c d e f g h i.
      objectFromList (a : b : c : d : e : f : g : h : i : []) =
      object9 a b c d e f g h i
#-}
{-# RULES "objectFromList/10" forall a b c d e f g h i j.
      objectFromList (a : b : c : d : e : f : g : h : i : j : []) =
      object10 a b c d e f g h i j
#-}
{-# RULES "objectFromList/11" forall a b c d e f g h i j k.
      objectFromList (a : b : c : d : e : f : g : h : i : j : k : []) =
      object11 a b c d e f g h i j k
#-}
{-# RULES "objectFromList/12" forall a b c d e f g h i j k l.
      objectFromList (a : b : c : d : e : f : g : h : i : j : k : l : []) =
      object12 a b c d e f g h i j k l
#-}
{-# RULES "objectFromList/13" forall a b c d e f g h i j k l m.
      objectFromList (a : b : c : d : e : f : g : h : i : j : k : l : m : []) =
      object13 a b c d e f g h i j k l m
#-}
{-# RULES "objectFromList/14" forall a b c d e f g h i j k l m n.
      objectFromList (a : b : c : d : e : f : g : h : i : j : k : l : m : n : []) =
      object14 a b c d e f g h i j k l m n
#-}
{-# RULES "objectFromList/15" forall a b c d e f g h i j k l m n o.
      objectFromList (a : b : c : d : e : f : g : h : i : j : k : l : m : n : o : []) =
      object15 a b c d e f g h i j k l m n o
#-}
{-# RULES "objectFromList/16" forall a b c d e f g h i j k l m n o p.
      objectFromList (a : b : c : d : e : f : g : h : i : j : k : l : m : n : o : p : []) =
      object16 a b c d e f g h i j k l m n o p
#-}

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

-- | Construct a JSON object with twelve members.
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

-- | Construct a JSON object with thirteen members.
object13 :: Member -> Member -> Member -> Member -> Member -> Member -> Member -> Member
         -> Member -> Member -> Member -> Member -> Member -> Value
{-# inline object13 #-}
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
object14 :: Member -> Member -> Member -> Member -> Member -> Member -> Member -> Member
         -> Member -> Member -> Member -> Member -> Member -> Member -> Value
{-# inline object14 #-}
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
object15 :: Member -> Member -> Member -> Member -> Member -> Member -> Member -> Member
         -> Member -> Member -> Member -> Member -> Member -> Member -> Member -> Value
{-# inline object15 #-}
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
object16 :: Member -> Member -> Member -> Member -> Member -> Member -> Member -> Member
         -> Member -> Member -> Member -> Member -> Member -> Member -> Member -> Member
         -> Value
{-# inline object16 #-}
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
{-# inline word8 #-}
word8 = Json.Number . SCI.fromWord8

word16 :: Word16 -> Json.Value
{-# inline word16 #-}
word16 = Json.Number . SCI.fromWord16

word32 :: Word32 -> Json.Value
{-# inline word32 #-}
word32 = Json.Number . SCI.fromWord32

word64 :: Word64 -> Json.Value
{-# inline word64 #-}
word64 = Json.Number . SCI.fromWord64

int8 :: Int8 -> Json.Value
{-# inline int8 #-}
int8 = Json.Number . SCI.fromInt8

int16 :: Int16 -> Json.Value
{-# inline int16 #-}
int16 = Json.Number . SCI.fromInt16

int32 :: Int32 -> Json.Value
{-# inline int32 #-}
int32 = Json.Number . SCI.fromInt32

int64 :: Int64 -> Json.Value
{-# inline int64 #-}
int64 = Json.Number . SCI.fromInt64

int :: Int -> Json.Value
{-# inline int #-}
int = Json.Number . SCI.fromInt

text :: Text -> Json.Value
{-# inline text #-}
text = Json.String . TS.fromText

shortText :: ShortText -> Json.Value
{-# inline shortText #-}
shortText = String

bool :: Prelude.Bool -> Json.Value
{-# inline bool #-}
bool Prelude.True = True
bool _ = False

-- | Typeclass for types that can be encoded as JSON.
class ToValue a where
  toValue :: a -> Value

-- | Encodes the unit value as JSON @null@.
instance ToValue () where {toValue _ = Null}
instance ToValue Value where {toValue = id}
instance ToValue Scientific where {toValue = Number}
instance ToValue Int where {toValue = int}
instance ToValue Int8 where {toValue = int8}
instance ToValue Int16 where {toValue = int16}
instance ToValue Int32 where {toValue = int32}
instance ToValue Int64 where {toValue = int64}
instance ToValue Word8 where {toValue = word8}
instance ToValue Word16 where {toValue = word16}
instance ToValue Word32 where {toValue = word32}
instance ToValue Word64 where {toValue = word64}
instance ToValue ShortText where {toValue = shortText}
instance ToValue Text where {toValue = text}
instance ToValue Prelude.Bool where {toValue = bool}
instance ToValue Word where
 toValue = word64 . fromIntegral @Word @Word64

instance ToValue a => ToValue [a] where
  toValue !xs = runST $ do
    let len = List.length xs
    dst <- PM.newSmallArray len Null
    let go !ix ys = case ys of
          [] -> do
            dst' <- PM.unsafeFreezeSmallArray dst
            pure (Array dst')
          z : zs -> do
            PM.writeSmallArray dst ix $! toValue z
            go (ix + 1) zs
    go 0 xs

instance ToValue a => ToValue (SmallArray a) where
  toValue !xs = Json.Array $! Contiguous.map' toValue xs

instance ToValue a => ToValue (Array a) where
  toValue !xs = Json.Array $! Contiguous.map' toValue xs

instance (Prim a, ToValue a) => ToValue (PrimArray a) where
  toValue !xs = Json.Array $! Contiguous.map' toValue xs
