{-# language BangPatterns #-}
{-# language LambdaCase #-}

import Data.Primitive (ByteArray)
import Data.ByteString (ByteString)
import Data.Bool (bool)
import Control.Exception
import Foreign.C.Types (CChar)

import qualified Json
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified System.IO as IO

main :: IO ()
main = do
  input <- Chunks.hGetContents IO.stdin
  case Json.decode (Chunks.concat input) of
    Left err -> fail (show err)
    Right v -> print v
