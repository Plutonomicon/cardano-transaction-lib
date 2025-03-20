-- | This module demonstrates how `applyArgs` from `Cardano.Plutus.ApplyArgs`
-- | (from https://github.com/mlabs-haskell/purescript-uplc-apply-args) can be
-- | used to build scripts with the provided arguments applied. It creates a
-- | transaction that mints an NFT using the one-shot minting policy.
module Ctl.Examples.OneShotMinting
  ( contract
  , example
  , main
  , mkContractWithAssertions
  , mkOneShotMintingPolicy
  , oneShotMintingPolicyScript
  ) where

import Contract.Prelude

import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(SpendOutput, MintAsset)
  )
import Cardano.Types (_body, _fee, _input)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionUnspentOutput (fromUtxoMap)
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractE
  , liftContractM
  , liftedM
  , runContract
  )
import Contract.PlutusData (PlutusData, toData)
import Contract.Scripts (PlutusScript)
import Contract.Test.Assert
  ( ContractCheck
  , checkLossInWallet
  , checkTokenGainInWallet'
  , runChecks
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , submitTxFromBuildPlan
  )
import Contract.Value (AssetName, ScriptHash)
import Contract.Wallet (getWalletUtxos)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Trans.Class (lift)
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Data.Array (head, singleton) as Array
import Data.Lens ((^.))
import Data.Map (empty) as Map
import Effect.Exception (error, throw)
import JS.BigInt (BigInt)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

mkChecks
  :: (ScriptHash /\ AssetName /\ BigInt)
  -> Array (ContractCheck { txFinalFee :: BigInt })
mkChecks nft =
  [ checkTokenGainInWallet' nft
  , checkLossInWallet
      case _ of
        Nothing -> liftEffect $ throw $
          "Unable to estimate expected loss in wallet"
        Just { txFinalFee } -> pure txFinalFee
  ]

contract :: Contract Unit
contract =
  mkContractWithAssertions "Examples.OneShotMinting" oneShotMintingPolicyScript

mkContractWithAssertions
  :: String
  -> (TransactionInput -> Contract PlutusScript)
  -> Contract Unit
mkContractWithAssertions exampleName mkMintingPolicy = do
  logInfo' ("Running " <> exampleName)
  utxos <- liftedM "Failed to get UTxOs from wallet" $ getWalletUtxos <#> map
    fromUtxoMap
  oref <-
    liftContractM "Utxo set is empty"
      (Array.head utxos)

  ps <- mkMintingPolicy (oref ^. _input)
  let cs = PlutusScript.hash ps
  tn <- Helpers.mkAssetName "CTLNFT"

  let
    plan =
      [ MintAsset cs tn (Int.fromInt one)
          (PlutusScriptCredential (ScriptValue ps) RedeemerDatum.unit)
      , SpendOutput (oref) Nothing
      ]

  let checks = mkChecks (cs /\ tn /\ one)
  void $ runChecks checks $ lift do
    tx <- submitTxFromBuildPlan Map.empty mempty plan
    let
      txHash = Transaction.hash tx
      txFinalFee = tx ^. _body <<< _fee
    logInfo' $ "Tx ID: " <> show txHash
    awaitTxConfirmed txHash
    logInfo' "Tx submitted successfully!"
    pure { txFinalFee: BigNum.toBigInt $ unwrap txFinalFee }

oneShotMintingPolicyScript :: TransactionInput -> Contract PlutusScript
oneShotMintingPolicyScript txInput = do
  script <- liftMaybe (error "Error decoding oneShotMinting") do
    envelope <- decodeTextEnvelope oneShotMinting
    plutusScriptFromEnvelope envelope
  liftContractE $ mkOneShotMintingPolicy script txInput

mkOneShotMintingPolicy
  :: PlutusScript
  -> TransactionInput
  -> Either String PlutusScript
mkOneShotMintingPolicy unappliedMintingPolicy oref =
  let
    mintingPolicyArgs :: Array PlutusData
    mintingPolicyArgs = Array.singleton (toData oref)
  in
    applyArgs unappliedMintingPolicy mintingPolicyArgs

foreign import oneShotMinting :: String
