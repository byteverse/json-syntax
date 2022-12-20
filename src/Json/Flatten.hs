{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}

-- | Flatten nested JSON objects into a single JSON object in which the keys
-- have been joined by the separator.
module Json.Flatten
  ( flatten
  ) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Builder.Catenable (Builder)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Text.Short (ShortText)
import Data.Primitive (SmallArray,ByteArray(ByteArray),MutableByteArray)
import Data.Word (Word8)
import Json (Member(Member))
import qualified Json
import qualified Data.Chunks as Chunks
import qualified Data.Primitive.Contiguous as C
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Text.Utf8 as Utf8
import qualified Data.Primitive as PM
import qualified Data.Builder.Catenable as Builder
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS

-- | Flatten a json value, recursively descending into objects and joining
-- keys with the separator. For example:
--
-- > { "name": "bilbo"
-- > , "occupation":
-- >   { "name": "burglar"
-- >   , "start": "2022-05-30"
-- >   }
-- > , "height": 124
-- > , "favorites": ["adventures","lunch"]
-- > }
--
-- Becomes:
--
-- > { "name": "bilbo"
-- > , "occupation.name": "burglar"
-- > , "occupation.start": "2022-05-30"
-- > , "height": 124
-- > , "favorites": ["adventures","lunch"]
-- > }
--
-- Currently, the implementation of this function throws an exception if
-- any separator other than period is used. This may be corrected in a future
-- release.
flatten :: Char -> Json.Value -> Json.Value
flatten c v = case c of
  '.' -> flattenPeriod v
  _ -> errorWithoutStackTrace "Json.Flatten.flatten: only period is supported"

-- built backwards
data ShortTexts
  = ShortTextsCons !ShortText !ShortTexts
  | ShortTextsBase !ShortText

flattenPeriod :: Json.Value -> Json.Value
flattenPeriod x = case x of
  Json.Object mbrs ->
    let bldr = foldMap (\Member{key,value} -> flattenPrefix (ShortTextsBase key) value) mbrs
        chunks = Builder.run bldr
        result = Chunks.concat chunks
     in Json.Object result
  Json.Array ys -> Json.Array $! C.map' flattenPeriod ys
  _ -> x

flattenPrefix ::
     ShortTexts -- context accumulator
  -> Json.Value
  -> Builder Json.Member
flattenPrefix !pre x = case x of
  Json.Object mbrs -> flattenObject pre mbrs
  _ ->
    let !a = flattenPeriod x
        !k = runShortTexts pre
        !mbr = Json.Member{key=k,value=a}
     in Builder.Cons mbr Builder.Empty

flattenObject :: ShortTexts -> SmallArray Json.Member -> Builder Json.Member
flattenObject !pre !mbrs = foldMap
  (\Member{key,value} -> flattenPrefix (ShortTextsCons key pre) value
  ) mbrs

runShortTexts :: ShortTexts -> ShortText
runShortTexts !ts0 = go 0 ts0
  where
  paste :: MutableByteArray s -> Int -> ShortTexts -> ST s ByteArray
  paste !dst !ix (ShortTextsBase t) =
    let len = Bytes.length (Utf8.fromShortText t)
     in case ix - len of
          0 -> do
            PM.copyByteArray dst 0 (st2ba t) 0 len
            PM.unsafeFreezeByteArray dst
          _ -> errorWithoutStackTrace "Json.Flatten.runShortTexts: implementation mistake"
  paste !dst !ix (ShortTextsCons t ts) = do
    let !len = Bytes.length (Utf8.fromShortText t)
    let !ixNext = ix - len
    PM.copyByteArray dst ixNext (st2ba t) 0 len
    let !ixPred = ixNext - 1
    PM.writeByteArray dst ixPred (0x2E :: Word8)
    paste dst ixPred ts
  go :: Int -> ShortTexts -> ShortText
  go !byteLenAcc (ShortTextsCons t ts) =
    go (Bytes.length (Utf8.fromShortText t) + byteLenAcc + 1) ts
  go !byteLenAcc (ShortTextsBase t) =
    let !(ByteArray r) = runByteArrayST $ do
          let totalLen = Bytes.length (Utf8.fromShortText t) + byteLenAcc
          dst <- PM.newByteArray totalLen
          paste dst totalLen ts0
     in TS.fromShortByteStringUnsafe (SBS r)

st2ba :: ShortText -> ByteArray
{-# inline st2ba #-}
st2ba t = case TS.toShortByteString t of
  SBS x -> ByteArray x
