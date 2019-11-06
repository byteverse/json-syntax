{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Gauge.Main (defaultMain,bgroup,bench,whnf)

import Twitter100 (encodedTwitter100,byteStringTwitter100)

import qualified Data.Bytes as Bytes
import qualified Json as J
import qualified Data.Aeson as Aeson

main :: IO ()
main = defaultMain
  [ bgroup "json"
    [ bgroup "twitter"
      [ bench "100" $ whnf
          (\b -> J.decode (Bytes.fromByteArray b))
          encodedTwitter100
      ]
    ]
  , bgroup "aeson"
    [ bgroup "twitter"
      [ bench "100"
          (whnf (Aeson.decodeStrict' @Aeson.Value) byteStringTwitter100)
      ]
    ]
  ]
