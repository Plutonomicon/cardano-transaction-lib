{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Api (app, getTransactionFeeEstimate)
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
import Types (Cbor (Cbor), Env, Fee (Fee), newEnvIO)

main :: IO ()
main = hspec serverSpec

serverSpec :: Spec
serverSpec = do
  describe "Api.Fees" feeEstimateSpec

feeEstimateSpec :: Spec
feeEstimateSpec = around withFeeEstimate $ do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv baseUrlPort = mkClientEnv manager $ baseUrl {baseUrlPort}

  context "GET /fees" $ do
    it "estimates the correct fee" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          getTransactionFeeEstimate cborTxFixture
      -- TODO verify with another tool that this is the correct fee estimate
      result `shouldBe` Right (Fee 160265)

    it "catches invalid hex strings" $ \port -> do
      result <-
        runClientM' (clientEnv port)
          . getTransactionFeeEstimate
          $ Cbor "deadbeefq"
      result `shouldSatisfy` expectError 400 "invalid bytestring size"

    it "catches invalid CBOR-encoded transactions" $ \port -> do
      result <-
        runClientM' (clientEnv port)
          . getTransactionFeeEstimate
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

withFeeEstimate :: ActionWith (Port -> IO ())
withFeeEstimate = Warp.testWithApplication $ app <$> newEnvIO'
  where
    newEnvIO' :: IO Env
    newEnvIO' = either die pure =<< newEnvIO

runClientM' ::
  forall (a :: Type).
  ClientEnv ->
  ClientM a ->
  IO (Either ClientError a)
runClientM' = flip runClientM

-- This is a known-good 'Tx AlonzoEra'
cborTxFixture :: Cbor
cborTxFixture =
  Cbor $
    mconcat
      [ "84a300818258205d677265fa5bb21ce6d8c7502aca70b93"
      , "16d10e958611f3c6b758f65ad9599960001818258390030"
      , "fb3b8539951e26f034910a5a37f22cb99d94d1d409f69dd"
      , "baea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba"
      , "3e96550504d5336100021a0002b569a0f5f6"
      ]
