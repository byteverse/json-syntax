{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | Flatten nested JSON objects into a single JSON object in which the keys
have been joined by the separator.
-}
module Json.Flatten
  ( flatten
  ) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Builder.Catenable (Builder)
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.Primitive (ByteArray (ByteArray), MutableByteArray, SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word8)
import Json (Member (Member))
import Data.Text.Internal (Text(Text))

import qualified Data.Builder.Catenable as Builder
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Text.Utf8 as Utf8
import qualified Data.Chunks as Chunks
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as C
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS
import qualified Json

{- | Flatten a json value, recursively descending into objects and joining
keys with the separator. For example:

> { "name": "bilbo"
> , "occupation":
>   { "name": "burglar"
>   , "start": "2022-05-30"
>   }
> , "height": 124
> , "favorites": ["adventures","lunch"]
> }

Becomes:

> { "name": "bilbo"
> , "occupation.name": "burglar"
> , "occupation.start": "2022-05-30"
> , "height": 124
> , "favorites": ["adventures","lunch"]
> }

Currently, the implementation of this function throws an exception if
any separator other than period is used. This may be corrected in a future
release.
-}
flatten :: Char -> Json.Value -> Json.Value
flatten c v = case c of
  '.' -> flattenPeriod v
  _ -> errorWithoutStackTrace "Json.Flatten.flatten: only period is supported"

-- built backwards
data Texts
  = TextsCons !Text !Texts
  | TextsBase !Text

flattenPeriod :: Json.Value -> Json.Value
flattenPeriod x = case x of
  Json.Object mbrs ->
    let bldr = foldMap (\Member {key, value} -> flattenPrefix (TextsBase key) value) mbrs
        chunks = Builder.run bldr
        result = Chunks.concat chunks
     in Json.Object result
  Json.Array ys -> Json.Array $! C.map' flattenPeriod ys
  _ -> x

flattenPrefix ::
  Texts -> -- context accumulator
  Json.Value ->
  Builder Json.Member
flattenPrefix !pre x = case x of
  Json.Object mbrs -> flattenObject pre mbrs
  _ ->
    let !a = flattenPeriod x
        !k = TS.toText $! runTexts pre
        !mbr = Json.Member {key = k, value = a}
     in Builder.Cons mbr Builder.Empty

flattenObject :: Texts -> SmallArray Json.Member -> Builder Json.Member
flattenObject !pre !mbrs =
  foldMap
    ( \Member {key, value} -> flattenPrefix (TextsCons key pre) value
    )
    mbrs

runTexts :: Texts -> ShortText
runTexts !ts0 = go 0 ts0
 where
  paste :: MutableByteArray s -> Int -> Texts -> ST s ByteArray
  paste !dst !ix (TextsBase (Text arr off len)) = case ix - len of
    0 -> do
      PM.copyByteArray dst 0 arr off len
      PM.unsafeFreezeByteArray dst
    _ -> errorWithoutStackTrace "Json.Flatten.runTexts: implementation mistake"
  paste !dst !ix (TextsCons (Text arr off len) ts) = do
    let !ixNext = ix - len
    PM.copyByteArray dst ixNext arr off len
    let !ixPred = ixNext - 1
    PM.writeByteArray dst ixPred (0x2E :: Word8)
    paste dst ixPred ts
  go :: Int -> Texts -> ShortText
  go !byteLenAcc (TextsCons t ts) =
    go (Bytes.length (Utf8.fromText t) + byteLenAcc + 1) ts
  go !byteLenAcc (TextsBase t) =
    let !(ByteArray r) = runByteArrayST $ do
          let totalLen = Bytes.length (Utf8.fromText t) + byteLenAcc
          dst <- PM.newByteArray totalLen
          paste dst totalLen ts0
     in TS.fromShortByteStringUnsafe (SBS r)
