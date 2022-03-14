module Main (main) where

import Api (app, applyArgs, estimateTxFees)
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Kind (Type)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status (Status))
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
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
  ApplyArgsRequest (ApplyArgsRequest, args, script),
  Cbor (Cbor),
  Env,
  Fee (Fee),
  newEnvIO, AppliedScript (AppliedScript)
 )

main :: IO ()
main = hspec serverSpec

serverSpec :: Spec
serverSpec = do
  describe "Api.Handlers.applyArgs" applyArgsSpec
  describe "Api.Handlers.estimateTxFees" feeEstimateSpec

applyArgsSpec :: Spec
applyArgsSpec = around withTestApp $ do
  clientEnv <- setupClientEnv

  context "POST /apply-args" $ do
    it "returns the correct fully saturated Plutus script" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          applyArgs argsRequestFixture
      result `shouldBe` Right appliedScript
  where
    appliedScript :: AppliedScript
    appliedScript = AppliedScript undefined -- TODO

feeEstimateSpec :: Spec
feeEstimateSpec = around withTestApp $ do
  clientEnv <- setupClientEnv

  context "GET /fees" $ do
    it "estimates the correct fee" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          estimateTxFees cborTxFixture
      -- This is probably incorrect. See:
      -- https://github.com/Plutonomicon/cardano-browser-tx/issues/123
      result `shouldBe` Right (Fee 168449)

    it "catches invalid hex strings" $ \port -> do
      result <-
        runClientM' (clientEnv port)
          . estimateTxFees
          $ Cbor "deadbeefq"
      result `shouldSatisfy` expectError 400 "invalid bytestring size"

    it "catches invalid CBOR-encoded transactions" $ \port -> do
      result <-
        runClientM' (clientEnv port)
          . estimateTxFees
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

argsRequestFixture :: ApplyArgsRequest
argsRequestFixture =
  ApplyArgsRequest
    { script = undefined -- TODO
    , args = undefined -- TODO
    }

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
