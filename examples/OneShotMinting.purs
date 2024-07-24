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
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Config
  ( ContractParams
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
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
import Contract.ScriptLookups as Lookups
import Contract.Scripts (PlutusScript)
import Contract.Test.Assert
  ( ContractCheck
  , checkLossInWallet
  , checkTokenGainInWallet'
  , runChecks
  )
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptFromEnvelope
  )
import Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraintsReturningFee
  )
import Contract.TxConstraints as Constraints
import Contract.Value (AssetName, ScriptHash)
import Contract.Wallet (getWalletUtxos)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Trans.Class (lift)
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Data.Array (head, singleton) as Array
import Data.Map (toUnfoldable) as Map
import Effect.Exception (error, throw)
import JS.BigInt (BigInt)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec = Just $ ConnectToGenericCip30 "nami" { cip95: false }
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
  utxos <- liftedM "Failed to get UTxOs from wallet" getWalletUtxos
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (Map.toUnfoldable utxos :: Array _))

  ps <- mkMintingPolicy oref
  let cs = PlutusScript.hash ps
  tn <- Helpers.mkAssetName "CTLNFT"

  let
    constraints :: Constraints.TxConstraints
    constraints =
      Constraints.mustMintValue (Mint.singleton cs tn $ Int.fromInt one)
        <> Constraints.mustSpendPubKeyOutput oref

    lookups :: Lookups.ScriptLookups
    lookups =
      Lookups.plutusMintingPolicy ps
        <> Lookups.unspentOutputs utxos

  let checks = mkChecks (cs /\ tn /\ one)
  void $ runChecks checks $ lift do
    { txHash, txFinalFee } <-
      submitTxFromConstraintsReturningFee lookups constraints
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
