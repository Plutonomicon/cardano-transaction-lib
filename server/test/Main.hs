module Main (main) where

import Api (app, applyArgs)
import Cardano.Api.Shelley (
  EpochNo (EpochNo),
  ExecutionUnitPrices (
    ExecutionUnitPrices,
    priceExecutionMemory,
    priceExecutionSteps
  ),
  ExecutionUnits (ExecutionUnits, executionMemory, executionSteps),
  Lovelace (Lovelace),
  ProtocolParameters (
    ProtocolParameters,
    protocolParamCollateralPercent,
    protocolParamCostModels,
    protocolParamDecentralization,
    protocolParamExtraPraosEntropy,
    protocolParamMaxBlockBodySize,
    protocolParamMaxBlockExUnits,
    protocolParamMaxBlockHeaderSize,
    protocolParamMaxCollateralInputs,
    protocolParamMaxTxExUnits,
    protocolParamMaxTxSize,
    protocolParamMaxValueSize,
    protocolParamMinPoolCost,
    protocolParamMinUTxOValue,
    protocolParamMonetaryExpansion,
    protocolParamPoolPledgeInfluence,
    protocolParamPoolRetireMaxEpoch,
    protocolParamPrices,
    protocolParamProtocolVersion,
    protocolParamStakeAddressDeposit,
    protocolParamStakePoolDeposit,
    protocolParamStakePoolTargetNum,
    protocolParamTreasuryCut,
    protocolParamTxFeeFixed,
    protocolParamTxFeePerByte,
    protocolParamUTxOCostPerWord
  ),
 )
import Data.Bifunctor (second)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Ratio ((%))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Ogmios.Parser (decodeProtocolParameters)
import Plutus.V1.Ledger.Api qualified as Ledger
import Servant.Client (
  BaseUrl (baseUrlPort),
  ClientEnv,
  ClientError,
  ClientM,
  mkClientEnv,
  parseBaseUrl,
  runClientM,
 )
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
  Env (Env),
  unsafeDecode,
 )

main :: IO ()
main = hspec serverSpec

serverSpec :: Spec
serverSpec = do
  describe "Api.Handlers.applyArgs" applyArgsSpec
  describe "Ogmios.Parser " testParser

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
      result `shouldBe` Right (AppliedScript partiallyAppliedScript)

    it "returns the correct fully applied Plutus script" $ \port -> do
      result <-
        runClientM' (clientEnv port) $
          applyArgs fullyAppliedRequestFixture
      result `shouldBe` Right (AppliedScript fullyAppliedScript)

setupClientEnv :: SpecM Port (Port -> ClientEnv)
setupClientEnv = do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  pure $
    let clientEnv port = mkClientEnv manager $ baseUrl {baseUrlPort = port}
     in clientEnv

withTestApp :: ActionWith (Port -> IO ())
withTestApp =
  Warp.testWithApplication $
    pure . app $
      Env fixedProtocolParameters

runClientM' ::
  forall (a :: Type).
  ClientEnv ->
  ClientM a ->
  IO (Either ClientError a)
runClientM' = flip runClientM

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

-- mkTestValidator :: Integer -> CurrencySymbol -> BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mkTestValidator i (CurrencySymbol cs) _ _ _ =
--   if i P.== 1 && cs P.== "" then () else P.error ()

-- mkTestValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mkTestValidatorUntyped p1 p2 =
--   mkTestValidator
--     (unsafeFromBuiltinData p1)
--     (unsafeFromBuiltinData p2)

-- unappliedScript :: Script
-- unappliedScript =
--   fromCompiledCode
--     $$(PlutusTx.compile [||mkTestValidatorUntyped||])
unappliedScript :: Ledger.Script
unappliedScript =
  unsafeDecode
    "Script"
    "\"586f010000333222323233222233222225335300c33225335300e0021001100f333500b\
    \22333573466e1c00800404003c0152002333500b22333573466e3c00800404003c0112201\
    \0010091326353008009498cd4015d680119a802bae0011200120011200112001122002122\
    \001200101\""

-- partiallyAppliedScript :: Integer -> Script
-- partiallyAppliedScript i =
--   fromCompiledCode
--     ( $$(PlutusTx.compile [||mkTestValidatorUntyped||])
--         `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData i)
--     )
-- Script applied with (I 32)
partiallyAppliedScript :: Ledger.Script
partiallyAppliedScript =
  unsafeDecode
    "Script"
    "\"58750100003333222323233222233222225335300c33225335300e0021001100f333500\
    \b22333573466e1c00800404003c0152002333500b22333573466e3c00800404003c011221\
    \0010091326353008009498cd4015d680119a802bae0011200120011200112001122002122\
    \00120014c010218200001\""

-- fullyAppliedScript :: Integer -> CurrencySymbol -> Script
-- fullyAppliedScript i b =
--   getValidator $
--     mkValidatorScript
--       ( $$(PlutusTx.compile [||mkTestValidatorUntyped||])
--           `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData i)
--           `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData b)
--       )
-- Script applied with (I 32) (B "test")
fullyAppliedScript :: Ledger.Script
fullyAppliedScript =
  unsafeDecode
    "Script"
    "\"587f01000033333222323233222233222225335300c33225335300e0021001100f33350\
    \0b22333573466e1c00800404003c0152002333500b22333573466e3c00800404003c01122\
    \010010091326353008009498cd4015d680119a802bae00112001200112001120011220021\
    \2200120014c01021820004c010544746573740001\""

fixedProtocolParameters :: ProtocolParameters
fixedProtocolParameters =
  ProtocolParameters
    { protocolParamProtocolVersion = (7, 0)
    , protocolParamDecentralization = 0 % 1
    , protocolParamExtraPraosEntropy = Nothing
    , protocolParamMaxBlockHeaderSize = 1100
    , protocolParamMaxBlockBodySize = 65536
    , protocolParamMaxTxSize = 16384
    , protocolParamTxFeeFixed = 155381
    , protocolParamTxFeePerByte = 44
    , protocolParamMinUTxOValue = Nothing
    , protocolParamStakeAddressDeposit = Lovelace 400000
    , protocolParamStakePoolDeposit = Lovelace 500000000
    , protocolParamMinPoolCost = Lovelace 0
    , protocolParamPoolRetireMaxEpoch = EpochNo 18
    , protocolParamStakePoolTargetNum = 50
    , protocolParamPoolPledgeInfluence = 1 % 10
    , protocolParamMonetaryExpansion = 178650067 % 100000000000
    , protocolParamTreasuryCut = 1 % 10
    , protocolParamUTxOCostPerWord = Just (Lovelace 34480)
    , protocolParamCostModels = Map.empty
    , protocolParamPrices =
        Just
          ( ExecutionUnitPrices
              { priceExecutionSteps = 721 % 10000000
              , priceExecutionMemory = 577 % 10000
              }
          )
    , protocolParamMaxTxExUnits =
        Just
          ( ExecutionUnits
              { executionSteps =
                  10000000000
              , executionMemory = 10000000
              }
          )
    , protocolParamMaxBlockExUnits =
        Just
          ( ExecutionUnits
              { executionSteps = 40000000000
              , executionMemory = 50000000
              }
          )
    , protocolParamMaxValueSize = Just 5000
    , protocolParamCollateralPercent = Just 150
    , protocolParamMaxCollateralInputs = Just 3
    }

loadParametersFile :: IO (Either String ProtocolParameters)
loadParametersFile =
  LBS.readFile "test/ogmios.json"
    <&> decodeProtocolParameters

testParser :: Spec
testParser =
  it "Testing parser of ogmios parameters" $ do
    value <- loadParametersFile
    makeNullCostModels value `shouldBe` Right fixedProtocolParameters
    value `shouldSatisfy` isNotNullCostModels
  where
    makeNullCostModels ::
      Either String ProtocolParameters -> Either String ProtocolParameters
    makeNullCostModels =
      second
        (\v -> v {protocolParamCostModels = mempty})

    isNotNullCostModels ::
      Either String ProtocolParameters -> Bool
    isNotNullCostModels (Right parameters) =
      not . Map.null . protocolParamCostModels $ parameters
    isNotNullCostModels _ = False
