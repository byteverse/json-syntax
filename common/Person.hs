{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Person
  ( encodedPerson
  , encodedFlattenedPerson
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString,toShort)
import Data.Primitive (ByteArray)
import Data.Text.Encoding (encodeUtf8)
import NeatInterpolation (text)

import qualified Data.Primitive as PM
import qualified Data.ByteString.Short.Internal as BSS

shortByteStringToByteArray :: ShortByteString -> ByteArray 
shortByteStringToByteArray (BSS.SBS x) = PM.ByteArray x

encodedPerson :: ByteArray
encodedPerson =
  shortByteStringToByteArray (toShort byteStringPerson)

encodedFlattenedPerson :: ByteArray
encodedFlattenedPerson =
  shortByteStringToByteArray (toShort byteStringFlattenedPerson)

byteStringPerson :: ByteString
byteStringPerson = encodeUtf8
  [text|
    { "name": "bilbo"
    , "occupation":
      { "name": "burglar"
      , "start": "2022-05-30"
      }
    , "height": 124
    , "favorites": ["adventures","lunch"]
    }
  |]

byteStringFlattenedPerson :: ByteString
byteStringFlattenedPerson = encodeUtf8
  [text|
    { "name": "bilbo"
    , "occupation.name": "burglar"
    , "occupation.start": "2022-05-30"
    , "height": 124
    , "favorites": ["adventures","lunch"]
    }
  |]

