module Test.OneShot
  ( main
  ) where

import Prelude

import Cardano.Types.Value (getNonAdaAsset)
import Contract.Address (ownPaymentPubKeyHash, getWalletAddress, ownStakePubKeyHash)
import Contract.Extra (mintContract, mintedValue, oneshotPolicy)
import Contract.Log (logInfo')
import Contract.Log (logTrace')
import Contract.Monad (Contract, liftContractAffM, liftContractM, liftedE, liftedM)
import Contract.PlutusData (PlutusData(Integer), Redeemer(Redeemer), getDatumByHash, getDatumsByHashes)
import Contract.Prelude (mconcat)
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, validatorHash)
import Contract.Test.Plutip (InitialUTxO, runContractInEnv, runPlutipContract, withPlutipContractEnv)
import Contract.Transaction (BalancedSignedTransaction, DataHash, awaitTxConfirmed, balanceAndSignTx, balanceAndSignTxE, submit, withBalancedAndSignedTxs)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (withResource)
import Control.Monad.Reader (asks)
import Control.Parallel (parallel, sequential)
import Data.Array (uncons)
import Data.BigInt as BigInt
import Data.Foldable (any, foldMap)
import Data.Function (on)
import Data.Log.Level (LogLevel(Trace))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (throw)
import Effect.Ref as Ref
import Examples.AlwaysMints (alwaysMintsPolicy)
import Examples.AlwaysSucceeds as AlwaysSucceeds
import Examples.MintsMultipleTokens (mintingPolicyRdmrInt1, mintingPolicyRdmrInt2, mintingPolicyRdmrInt3)
import Mote (group, test)
import Plutip.Types (PlutipConfig, StartClusterResponse(ClusterStartupSuccess), StopClusterResponse(StopClusterSuccess))
import Plutus.Conversion.Value (fromPlutusValue)
import Plutus.Types.Address (pubKeyHashAddress)
import Test.Spec.Assertions (expectError)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Runner (defaultConfig)
import Test.Utils as Utils
import TestM (TestPlanM)
import Types.UsedTxOuts (TxOutRefCache)

-- Run with `spago test --main Test.Plutip`
main :: Effect Unit
main = launchAff_ do
  Utils.interpretWithConfig
    -- we don't want to exit because we need to clean up after failure by
    -- timeout
    defaultConfig { timeout = Just $ wrap 30_000.0, exit = true }
    suite

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }

assertContract :: String -> Boolean -> Contract () Unit
assertContract msg cond = if cond then pure unit else liftEffect $ throw msg

compareWithoutAda :: Value.Value -> Value.Value -> Boolean
compareWithoutAda = (==) `on` (getNonAdaAsset <<< fromPlutusValue)

suite :: TestPlanM Unit
suite = do
  group "OneShot" do
    test "runPlutipContract: Mints correctly" do
      let
        distribution :: InitialUTxO
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice do
          tn <- liftContractM "Cannot make token name"
            $ Value.mkTokenName
                =<< byteArrayFromAscii "TheToken"
          oneShot <- mintContract [tn /\ BigInt.fromInt 1]
          addr <- liftedM "Couldn't get own address" getWalletAddress
          utxos <- unwrap <$> liftedM "Failed to fetch UTxOs" (utxosAt addr)
          let val = mintedValue oneShot
          assertContract "Couldn't find value in utxos" $
            any (\txOut -> (unwrap txOut).amount `compareWithoutAda` val) utxos
    test "runPlutipContract: Can't mint without txOutRef" do
      let
        distribution :: InitialUTxO
        distribution =
          [ BigInt.fromInt 10_000_000
          , BigInt.fromInt 10_000_000
          ]
      runPlutipContract config distribution \alice -> do
        withKeyWallet alice do
          tn <- liftContractM "Cannot make token name"
            $ Value.mkTokenName
                =<< byteArrayFromAscii "TheToken"
          expectError $ noRefContract [tn /\ BigInt.fromInt 1]

noRefContract :: Array (TokenName /\ BigInt.BigInt) -> Contract () Unit
noRefContract amts = do
  pkh <- liftedM "Failed to get fetch own pubkeyhash" ownPaymentPubKeyHash
  stk <- ownStakePubKeyHash
  utxos <- unwrap <$> liftedM "Failed to fetch UTxOs"
    (utxosAt $ pubKeyHashAddress pkh stk)
  let utxosList = Map.toUnfoldable utxos
  { head: (firstTxIn /\ _), tail: t} <- liftContractM "Not enough utxos" $ uncons utxosList
  { head: (secondTxIn /\ _), tail: t} <- liftContractM "Not enough utxos" $ uncons t

  mp <- oneshotPolicy secondTxIn

  cs <- liftContractAffM "Cannot get cs" $ Value.scriptCurrencySymbol mp

  let
    toValue :: TokenName /\ BigInt.BigInt -> Value.Value
    toValue (tn /\ amt) = Value.singleton cs tn amt

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValue (foldMap toValue amts)
        <> Constraints.mustSpendPubKeyOutput firstTxIn

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs utxos

  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <-
    liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId

  awaitTxConfirmed txId
  logInfo' $ "Tx submitted successfully!"

submitAndLog
  :: forall (r :: Row Type). BalancedSignedTransaction -> Contract r Unit
submitAndLog bsTx = do
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId

getLockedInputs :: forall (r :: Row Type). Contract r TxOutRefCache
getLockedInputs = do
  cache <- asks (_.usedTxOuts <<< _.runtime <<< unwrap)
  liftEffect $ Ref.read $ unwrap cache

mkTokenName :: forall (r :: Row Type). String -> Contract r TokenName
mkTokenName =
  liftContractM "Cannot make token name"
    <<< (Value.mkTokenName <=< byteArrayFromAscii)

