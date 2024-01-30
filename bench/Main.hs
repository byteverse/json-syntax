{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Gauge.Main (bench, bgroup, defaultMain, whnf)

import Metrics1024 (encodedMetrics1024)
import Twitter100 (byteStringTwitter100, encodedTwitter100)
import Url100 (byteStringUrl100, encodedUrl100)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as BLDR
import qualified Data.Bytes.Chunks as Chunks
import qualified Json as J
import qualified Json.Smile as Smile

main :: IO ()
main = do
  valueTwitter100 <- case J.decode (Bytes.fromByteArray encodedTwitter100) of
    Left _ -> fail "json-syntax failed to decode twitter-100"
    Right v -> pure v
  valueUrl100 <- case J.decode (Bytes.fromByteArray encodedUrl100) of
    Left _ -> fail "json-syntax failed to decode url-100"
    Right v -> pure v
  valueMetrics1024 <- case J.decode (Bytes.fromByteArray encodedMetrics1024) of
    Left _ -> fail "json-syntax failed to decode metrics-1024"
    Right v -> pure v
  aesonValueTwitter100 <- case Aeson.decodeStrict' byteStringTwitter100 of
    Nothing -> fail "aeson failed to decode twitter-100"
    Just (v :: Aeson.Value) -> pure v
  defaultMain
    [ bgroup
        "json"
        [ bgroup
            "twitter"
            [ bgroup
                "100"
                [ bench "decode" $
                    whnf
                      (\b -> J.decode (Bytes.fromByteArray b))
                      encodedTwitter100
                , bench "encode" $
                    whnf
                      (\v -> Chunks.length (BLDR.run 128 (J.encode v)))
                      valueTwitter100
                , bench "encode-smile" $
                    whnf
                      (\v -> Chunks.length (BLDR.run 128 (Smile.encode v)))
                      valueTwitter100
                ]
            ]
        , bgroup
            "url"
            [ bgroup
                "100"
                [ bench "decode" $
                    whnf
                      (\b -> J.decode (Bytes.fromByteArray b))
                      encodedUrl100
                , bench "encode" $
                    whnf
                      (\v -> Chunks.length (BLDR.run 128 (J.encode v)))
                      valueUrl100
                , bench "encode-smile" $
                    whnf
                      (\v -> Chunks.length (BLDR.run 128 (Smile.encode v)))
                      valueUrl100
                ]
            ]
        , bgroup
            "metrics"
            [ bgroup
                "1024"
                [ bench "encode" $
                    whnf
                      (\v -> Chunks.length (BLDR.run 128 (J.encode v)))
                      valueMetrics1024
                , bench "encode-smile" $
                    whnf
                      (\v -> Chunks.length (BLDR.run 128 (Smile.encode v)))
                      valueMetrics1024
                ]
            ]
        ]
    , bgroup
        "aeson"
        [ bgroup
            "twitter"
            [ bgroup
                "100"
                [ bench
                    "decode"
                    (whnf (Aeson.decodeStrict' @Aeson.Value) byteStringTwitter100)
                , bench "encode" $
                    whnf
                      (\v -> LBS.length (Aeson.encode v))
                      aesonValueTwitter100
                ]
            ]
        , bgroup
            "url"
            [ bgroup
                "100"
                [ bench "decode" (whnf (Aeson.decodeStrict' @Aeson.Value) byteStringUrl100)
                ]
            ]
        ]
    ]
