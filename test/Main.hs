{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

import Control.Monad (when)
import Data.Bytes (Bytes)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Primitive (ByteArray(ByteArray))
import Data.Scientific (Scientific,scientific)
import Data.Text.Short (ShortText)
import System.IO (withFile,IOMode(..))
import Test.QuickCheck ((===))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.HUnit ((@=?))
import Twitter100 (encodedTwitter100,byteStringTwitter100)

import qualified Data.Aeson as AE
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes.Chunks as BChunks
import qualified Data.Bytes.Text.Ascii as Ascii
import qualified Data.Bytes.Text.Latin1 as Latin1
import qualified Data.ByteString as BS
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as M
import qualified Data.Number.Scientific as SCI
import qualified Data.Text.Short as TS
import qualified GHC.Exts as Exts
import qualified Json as J
import qualified Json.Smile as Smile
import qualified Test.QuickCheck as QC
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as TQC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "poorly-named tests"
    [ THU.testCase "A" $
        Right (J.Object mempty)
        @=?
        J.decode (Ascii.fromString "{}")
    , THU.testCase "B" $
        Right (J.Object (Exts.fromList [J.Member "foo" J.True]))
        @=?
        J.decode (Ascii.fromString "{\"foo\" : true}")
    , THU.testCase "C" $
        Right (J.Array (Exts.fromList [J.String "bar"]))
        @=?
        J.decode (Ascii.fromString "[\"bar\"]")
    , THU.testCase "D" $
        Right (J.Object (Exts.fromList [J.Member "foo" J.True, J.Member "bar" J.False]))
        @=?
        J.decode (Ascii.fromString "{\"foo\" : true, \"bar\": false }")
    , THU.testCase "E" $
        Right (J.String "Smile: 😂")
        @=?
        J.decode (shortTextToBytes "\"Smile: 😂\"")
    , THU.testCase "F" $
        Right (J.Array (Exts.fromList [ J.Object mempty, J.Object mempty, J.Null ]))
        @=?
        J.decode (shortTextToBytes " [ {} , { } , null ] ")
    , THU.testCase "G" $ case J.decode (shortTextToBytes " [ 55e2 , 1 ] ") of
        Right (J.Array xs) -> case Exts.toList xs of
          [J.Number a, J.Number b] -> do
            SCI.toWord32 a @=? Just 5500
            SCI.toWord32 b @=? Just 1
          _ -> fail "no good y"
        _ -> fail "no good x"
    , THU.testCase "H" $ case J.decode (shortTextToBytes " [] x") of
        Left _ -> pure ()
        Right _ -> fail "this was not supposed parse"
    , THU.testCase "I" $
        BChunks.concat (Builder.run 1 (J.encode (J.Array mempty)))
        @=?
        Latin1.fromString "[]"
    , THU.testCase "J" $
        BChunks.concat (Builder.run 1 (J.encode (J.Array mempty)))
        @=?
        Latin1.fromString "[]"
    , THU.testCase "K" $
        BChunks.concat (Builder.run 1 (J.encode (J.String "Hello\DELWorld")))
        @=?
        Latin1.fromString "\"Hello\DELWorld\""
    , THU.testCase "L" $
        BChunks.concat (Builder.run 1 (J.encode (J.String "Hello\nWorld")))
        @=?
        Latin1.fromString "\"Hello\\nWorld\""
    , TQC.testProperty "M" $ QC.forAll (jsonFromPrintableStrings <$> QC.vectorOf 10 QC.arbitrary) $ \val0 -> do
        let enc = BChunks.concat (Builder.run 128 (J.encode val0))
        case J.decode enc of
          Left _ -> QC.property False
          Right val1 -> val0 === val1
    , TQC.testProperty "N" $ QC.forAll (jsonFromPrintableStrings <$> QC.vectorOf 400 QC.arbitrary) $ \val0 -> do
        let enc = BChunks.concat (Builder.run 128 (J.encode val0))
        case J.decode enc of
          Left e -> QC.counterexample (show e) False
          Right val1 -> val0 === val1
    , TQC.testProperty "O" $ QC.forAll (jsonFromAsciiStrings <$> QC.vectorOf 10 QC.arbitrary) $ \val0 -> do
        let enc = BChunks.concat (Builder.run 128 (J.encode val0))
        case J.decode enc of
          Left _ -> QC.property False
          Right val1 -> val0 === val1
    , TQC.testProperty "P" $ QC.forAll (jsonFromAsciiStrings <$> QC.vectorOf 400 QC.arbitrary) $ \val0 -> do
        let enc = BChunks.concat (Builder.run 128 (J.encode val0))
        case J.decode enc of
          Left e -> QC.counterexample (show e) False
          Right val1 -> val0 === val1
    , THU.testCase "Q" $
        -- Test that Unicode <right single quotation mark> is decoded correctly.
        Right (J.String "It\2019s over now")
        @=?
        J.decode (shortTextToBytes "\"It\2019s over now\"")
    , THU.testCase "R" $
        -- Test that Unicode <right single quotation mark> is encoded correctly.
        shortTextToBytes "\"It\2019s over now\""
        @=?
        BChunks.concat (Builder.run 4 (J.encode (J.String "It\2019s over now")))
    ]
  , testGroup "smile-fragment"
    [ THU.testCase "biginteger-65535" $
        let enc = BChunks.concat (Builder.run 128 (Smile.encodeBigInteger 65535))
         in enc @=? Exts.fromList [0x26,0x83,0x00,0x3f,0x7f,0x07]
    , THU.testCase "biginteger-30000" $
        let enc = BChunks.concat (Builder.run 128 (Smile.encodeBigInteger 30000))
         in enc @=? Exts.fromList [0x26,0x82,0x3a,0x4c,0x00]
    , THU.testCase "biginteger-neg-300" $
        let enc = BChunks.concat (Builder.run 128 (Smile.encodeBigInteger (-300)))
         in enc @=? Exts.fromList [0x26,0x82,0x7f,0x35,0x00]
    , THU.testCase "biginteger-neg-65534" $
        let enc = BChunks.concat (Builder.run 128 (Smile.encodeBigInteger (-65534)))
         in enc @=? Exts.fromList [0x26,0x83,0x7f,0x40,0x00,0x02]
    , THU.testCase "biginteger-neg-65535" $
        let enc = BChunks.concat (Builder.run 128 (Smile.encodeBigInteger (-65535)))
         in enc @=? Exts.fromList [0x26,0x83,0x7f,0x40,0x00,0x01]
    , THU.testCase "biginteger-neg-65536" $
        let enc = BChunks.concat (Builder.run 128 (Smile.encodeBigInteger (-65536)))
         in enc @=? Exts.fromList [0x26,0x83,0x7f,0x40,0x00,0x00]
    , THU.testCase "biginteger-neg-65537" $
        let enc = BChunks.concat (Builder.run 128 (Smile.encodeBigInteger (-65537)))
         in enc @=? Exts.fromList [0x26,0x83,0x7f,0x3f,0x7f,0x07]
    , THU.testCase "biginteger-neg-9223372036854775808" $
        let enc = BChunks.concat (Builder.run 128 (Smile.encodeBigInteger (-9223372036854775808)))
         in enc @=? Exts.fromList
              [0x26,0x88,0x40,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00]
    , THU.testCase "biginteger-9223372036854775807" $
        let enc = BChunks.concat (Builder.run 128 (Smile.encodeBigInteger 9223372036854775807))
         in enc @=? Exts.fromList
              [0x26,0x88,0x3f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x01]
    , THU.testCase "biginteger-1208925819614629174706175" $
        -- Here, we are checking to make sure that we pad the beginning
        -- of a Integer (the Jp# case) when the leading bit is 1.
        let enc = BChunks.concat (Builder.run 128 (Smile.encodeBigInteger 1208925819614629174706175))
         in enc @=? Exts.fromList
              [0x26,0x8b,0x00,0x3f,0x7f,0x7f,0x7f,0x7f
              ,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x0f
              ]
    , THU.testCase "biginteger-neg-1208925819614629174706175" $
        let enc = BChunks.concat (Builder.run 128 (Smile.encodeBigInteger (-1208925819614629174706175)))
         in enc @=? Exts.fromList
              [0x26,0x8b,0x7f,0x40,0x00,0x00,0x00,0x00
              ,0x00,0x00,0x00,0x00,0x00,0x00,0x01
              ]
    ]
  , testGroup "smile-encode" $
    let mkTest name n = goldenVsFile name ("test/golden/"++n++"/expect.bin")
                                          ("test/golden/"++n++"/out.bin")
                                          (generate n)
        generate n = do
          contents <- Bytes.fromByteString <$> BS.readFile ("test/golden/"++n++"/in.json")
          let json = case J.decode contents of
                Left err -> error $ show err
                Right it -> it
              out = Smile.encode json
          withFile ("test/golden/"++n++"/out.bin") WriteMode $ \fp ->
            BChunks.hPut fp (Builder.run 2048 out)
     in
      [ mkTest "hello-world" "001"
      , mkTest "konnichiwa-minnasan" "002"
      , mkTest "konnichiwa-minnasan" "003"
      , mkTest "sixtyish-char unicode string" "003"
      , mkTest "sixtyish-char unicode keyname" "004"
      , mkTest "small numbers" "005"
      , mkTest "long positive decimal" "006"
      , mkTest "long negative decimal" "007"
      , mkTest "long positive integer" "008"
      , mkTest "long negative integer" "009"
      , mkTest "scientific-notation medium-pos-int-repr" "010"
      , mkTest "scientific-notation small-neg-int-repr" "011"
      , mkTest "scientific-notation small-pos-int-repr" "012"
      , mkTest "long-ascii" "013"
      , mkTest "empty-key" "014"
      , mkTest "number-100" "number/100"
      , mkTest "number-1e31" "number/1e31"
      , mkTest "number-1e32" "number/1e32"
      , mkTest "number-1e50" "number/1e50"
      , mkTest "number-1e62" "number/1e62"
      , mkTest "number-1e63" "number/1e63"
      , mkTest "number-1e64" "number/1e64"
      , mkTest "number-1e126" "number/1e126"
      , mkTest "number-1e127" "number/1e127"
      , mkTest "number-1e128" "number/1e128"
      , mkTest "number-1e129" "number/1e129"
      ]
  , THU.testCase "Twitter100" $
      case J.decode (Bytes.fromByteArray encodedTwitter100) of
        Left _ -> fail "nope"
        Right j -> case AE.decodeStrict byteStringTwitter100 of
          Nothing -> fail "aeson is messed up"
          Just ae -> ae @=? toAesonValue j
  , THU.testCase "Twitter100-roundtrip" $
      case J.decode (Bytes.fromByteArray encodedTwitter100) of
        Left _ -> fail "nope, Twitter100 test will be failing too"
        Right j -> case J.decode (BChunks.concat (Builder.run 1 (J.encode j))) of
          Left _ -> fail "encode did not produce a document that could be decoded"
          Right j' -> when (j /= j') (fail "document was not the same after roundtrip")
  ]

jsonFromPrintableStrings :: [QC.PrintableString] -> J.Value
jsonFromPrintableStrings xs = J.Array (Exts.fromList (map (J.String . TS.pack . QC.getPrintableString) xs))

jsonFromAsciiStrings :: [QC.ASCIIString] -> J.Value
jsonFromAsciiStrings xs = J.Array (Exts.fromList (map (J.String . TS.pack . QC.getASCIIString) xs))

toBadSci :: SCI.Scientific -> Scientific
toBadSci = SCI.withExposed
  (\a b -> scientific (fromIntegral a) b)
  (\a b -> scientific a (fromIntegral b))

toAesonValue :: J.Value -> AE.Value
toAesonValue = \case
  J.True -> AE.Bool True
  J.False -> AE.Bool False
  J.Null -> AE.Null
  J.String t -> AE.String (TS.toText t)
  J.Number n -> AE.Number (toBadSci n)
  J.Object mbrs -> AE.Object $ foldr
    (\(J.Member key val) hm -> M.insert (Key.fromShortText key) (toAesonValue val) hm)
    M.empty mbrs
  J.Array vals -> AE.Array $ Exts.fromList $ foldr
    (\x xs -> toAesonValue x : xs) [] vals

shortTextToBytes :: ShortText -> Bytes
shortTextToBytes str = case TS.toShortByteString str of
  SBS x -> let y = ByteArray x in Bytes.fromByteArray y
