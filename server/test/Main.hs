{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Api (app, applyArgs, blake2bHash, estimateTxFees, hashData, hashScript)
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Kind (Type)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status (Status))
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Plutus.V1.Ledger.Api qualified as Ledger
import Servant.Client (
  BaseUrl (baseUrlPort),
  ClientEnv,
  ClientError (FailureResponse),
  ClientM,
  ResponseF (Response),
  mkClientEnv,
  parseBaseUrl,
  runClientM,
 )
import System.Exit (die)
import Test.Hspec (
  ActionWith,
  Spec,
  around,
  context,
  describe,
  hspec,
  it,
  runIO,
  shouldBe,
  shouldSatisfy,
 )
import Test.Hspec.Core.Spec (SpecM)
import Types (
  AppliedScript (AppliedScript),
  ApplyArgsRequest (ApplyArgsRequest, args, script),
  Blake2bHash (Blake2bHash),
  BytesToHash (BytesToHash),
  Cbor (Cbor),
  Env,
  Fee (Fee),
  HashDataRequest (HashDataRequest),
  HashScriptRequest (HashScriptRequest),
  HashedData (HashedData),
  HashedScript (HashedScript),
  WitnessCount (WitnessCount),
  newEnvIO,
  unsafeDecode,
 )
import Plutus.V1.Ledger.Api (CurrencySymbol (..), unsafeFromBuiltinData, fromCompiledCode)
import Plutus.V1.Ledger.Scripts(Script, mkValidatorScript, getValidator)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinData)
import PlutusTx.Prelude qualified as P

main :: IO ()
main = hspec serverSpec

serverSpec :: Spec
serverSpec = do
  describe "Api.Handlers.applyArgs" applyArgsSpec
  describe "Api.Handlers.estimateTxFees" feeEstimateSpec
  describe "Api.Handlers.hashScript" hashScriptSpec
  describe "Api.Handlers.blake2bHash" blake2bHashSpec
  describe "Api.Handlers.hashData" hashDataSpec

applyArgsSpec :: Spec
applyArgsSpec = around withTestApp $ do
  clientEnv <- setupClientEnv

  context "POST apply-args" $ do
    it "returns the same script when called without args" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          applyArgs unappliedRequestFixture
      result `shouldBe` Right (AppliedScript unappliedScript)

    it "returns the correct partially applied Plutus script" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          applyArgs partiallyAppliedRequestFixture
      result `shouldBe` Right (AppliedScript $ partiallyAppliedScript 32)

    it "returns the correct fully applied Plutus script" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          applyArgs fullyAppliedRequestFixture
      result `shouldBe` Right (AppliedScript $ fullyAppliedScript 32 (CurrencySymbol "test"))

feeEstimateSpec :: Spec
feeEstimateSpec = around withTestApp $ do
  clientEnv <- setupClientEnv

  context "GET fees" $ do
    it "estimates the correct fee" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          estimateTxFees (WitnessCount 1) cborTxFixture
      -- This is probably incorrect. See:
      -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/123
      result `shouldBe` Right (Fee 168449)

    it "catches invalid hex strings" $ \port -> do
      result <-
        runClientM' (clientEnv port)
          . estimateTxFees (WitnessCount 1)
          $ Cbor "deadbeefq"
      result `shouldSatisfy` expectError 400 "invalid bytestring size"

    it "catches invalid CBOR-encoded transactions" $ \port -> do
      result <-
        runClientM' (clientEnv port)
          . estimateTxFees (WitnessCount 1)
          $ Cbor "deadbeef"
      result
        `shouldSatisfy` expectError
          400
          "DecoderErrorDeserialiseFailure \"Shelley Tx\" \
          \(DeserialiseFailure 0 \"expected list len or indef\")"
  where
    expectError :: Int -> LC8.ByteString -> Either ClientError Fee -> Bool
    expectError code body = \case
      Left (FailureResponse _ (Response (Status scode _) _ _ sbody))
        | scode == code && sbody == body -> True
      _ -> False

hashDataSpec :: Spec
hashDataSpec = around withTestApp $ do
  clientEnv <- setupClientEnv
  context "POST hash-data" $ do
    it "hashes the data" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          hashData cborDatumFixture
      result `shouldBe` Right hashedDatumFixture

hashScriptSpec :: Spec
hashScriptSpec = around withTestApp $ do
  clientEnv <- setupClientEnv

  context "POST hash-script" $ do
    it "hashes the script" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          hashScript hashScriptRequestFixture
      result `shouldBe` Right hashedScriptFixture

blake2bHashSpec :: Spec
blake2bHashSpec = around withTestApp $ do
  clientEnv <- setupClientEnv

  context "POST blake2b" $ do
    it "gets the blake2b_256 hash" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          blake2bHash (BytesToHash "foo")
      result `shouldBe` Right blake2bRes
  where
    -- obtained from `fromBuiltin . blake2b_256 $ toBuiltin @ByteString "foo"`
    blake2bRes :: Blake2bHash
    blake2bRes =
      Blake2bHash
        "\184\254\159\DELbU\166\250\b\246h\171c*\141\b\SUB\216y\131\199|\210t\228\140\228P\240\179I\253"

setupClientEnv :: SpecM Port (Port -> ClientEnv)
setupClientEnv = do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  pure $
    let clientEnv port = mkClientEnv manager $ baseUrl {baseUrlPort = port}
     in clientEnv

withTestApp :: ActionWith (Port -> IO ())
withTestApp = Warp.testWithApplication $ app <$> newEnvIO'
  where
    newEnvIO' :: IO Env
    newEnvIO' = either die pure =<< newEnvIO

runClientM' ::
  forall (a :: Type).
  ClientEnv ->
  ClientM a ->
  IO (Either ClientError a)
runClientM' = flip runClientM

cborDatumFixture :: HashDataRequest
cborDatumFixture =
  HashDataRequest $
    Cbor $
      mconcat
        [ "d8799f581c7040636730e73aea054c0b2dd0b734bec3ecaaca1e3cbe48b482ca145820d"
        , "850d7a70fd5ff97ab104b127d4c9630c64d8ac158a14cdcc4f65157b79af0baff"
        ]

hashedDatumFixture :: HashedData
hashedDatumFixture =
  HashedData
    "\150\GS~\189\&8;\239\DC1n\249\188\181'\241\188\SUBE\135\248\EM\212\236\SOr?\239Z\178(\173\GS\143"

-- This is a known-good 'Tx AlonzoEra'
cborTxFixture :: Cbor
cborTxFixture =
  Cbor $
    mconcat
      [ "84a500818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65a"
      , "d959996000d818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b75"
      , "8f65ad95999600018282581d600f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723a"
      , "c2d49f975461a0023e8fa82581d60981fc565bcf0c95c0cfa6ee6693875b60d529d87ed"
      , "7082e9bf03c6a41a000f4240021a0002b5690e81581c0f45aaf1b2959db6e5ff94dbb1f"
      , "823bf257680c3c723ac2d49f97546a10081825820096092b8515d75c2a2f75d6aa7c519"
      , "1996755840e81deaa403dba5b690f091b6584063721fd9360d569968defac287e76cfaa"
      , "767366ecf6709b8354e02e2df6c35d78453adb04ec76f8a3d1287468b8c244ff051dcd0"
      , "f29dbcac1f7baf3e2d06ce06f5f6"
      ]

hashScriptRequestFixture :: HashScriptRequest
hashScriptRequestFixture =
  HashScriptRequest $
    unsafeDecode "Script" "\"4d01000033222220051200120011\""

hashedScriptFixture :: HashedScript
hashedScriptFixture =
  HashedScript $
    unsafeDecode
      "ScriptHash"
      "{\"getScriptHash\":\
      \\"67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656\"}"

unappliedRequestFixture :: ApplyArgsRequest
unappliedRequestFixture =
  ApplyArgsRequest
    { script = unappliedScript
    , args = []
    }

partiallyAppliedRequestFixture :: ApplyArgsRequest
partiallyAppliedRequestFixture = unappliedRequestFixture {args = [Ledger.I 32]}

fullyAppliedRequestFixture :: ApplyArgsRequest
fullyAppliedRequestFixture =
  unappliedRequestFixture
    { args =
        [ Ledger.I 32
        , Ledger.B "test"
        ]
    }

mkTestValidator :: Integer -> CurrencySymbol -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkTestValidator i (CurrencySymbol cs) _ _ _ = 
  if i P.== 1 && cs P.== "" then () else P.error ()

mkTestValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkTestValidatorUntyped p1 p2 =
  mkTestValidator
    (unsafeFromBuiltinData p1)
    (unsafeFromBuiltinData p2)

unappliedScript :: Script
unappliedScript =
  fromCompiledCode
    $$(PlutusTx.compile [|| mkTestValidatorUntyped ||])

partiallyAppliedScript :: Integer -> Script
partiallyAppliedScript i =
  fromCompiledCode
    ($$(PlutusTx.compile [|| mkTestValidatorUntyped ||])
    `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData i))

fullyAppliedScript :: Integer -> CurrencySymbol -> Script
fullyAppliedScript i b =
  getValidator $ mkValidatorScript
    ($$(PlutusTx.compile [|| mkTestValidatorUntyped ||]) 
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData i)
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData b))
