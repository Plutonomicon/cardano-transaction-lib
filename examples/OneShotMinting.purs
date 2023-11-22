-- | This module demonstrates how `applyArgs` from `Contract.Scripts` can be
-- | used to build scripts with the provided arguments applied. It creates a
-- | transaction that mints an NFT using the one-shot minting policy.
module Ctl.Examples.OneShotMinting
  ( contract
  , example
  , main
  , mkContractWithAssertions
  , mkOneShotMintingPolicy
  , oneShotMintingPolicy
  , oneShotMintingPolicyScript
  ) where

import Contract.Prelude

import Contract.Config (ContractParams, testnetNamiConfig)
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
import Contract.Scripts
  ( ApplyArgsError
  , MintingPolicy(PlutusMintingPolicy)
  , PlutusScript
  , applyArgs
  )
import Contract.Test.Assert
  ( ContractCheck
  , checkLossInWallet
  , checkTokenGainInWallet'
  , runChecks
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV1FromEnvelope)
import Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraintsReturningFee
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value (singleton) as Value
import Contract.Wallet (getWalletUtxos)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Trans.Class (lift)
import Ctl.Examples.Helpers (mkCurrencySymbol, mkTokenName) as Helpers
import Data.Array (head, singleton) as Array
import Data.Map (toUnfoldable) as Map
import Effect.Exception (error, throw)
import JS.BigInt (BigInt)

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

mkChecks
  :: (CurrencySymbol /\ TokenName /\ BigInt)
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
  mkContractWithAssertions "Examples.OneShotMinting" oneShotMintingPolicy

mkContractWithAssertions
  :: String
  -> (TransactionInput -> Contract MintingPolicy)
  -> Contract Unit
mkContractWithAssertions exampleName mkMintingPolicy = do
  logInfo' ("Running " <> exampleName)
  utxos <- liftedM "Failed to get UTxOs from wallet" getWalletUtxos
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (Map.toUnfoldable utxos :: Array _))

  mp /\ cs <- Helpers.mkCurrencySymbol (mkMintingPolicy oref)
  tn <- Helpers.mkTokenName "CTLNFT"

  let
    constraints :: Constraints.TxConstraints
    constraints =
      Constraints.mustMintValue (Value.singleton cs tn one)
        <> Constraints.mustSpendPubKeyOutput oref

    lookups :: Lookups.ScriptLookups
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs utxos

  let checks = mkChecks (cs /\ tn /\ one)
  void $ runChecks checks $ lift do
    { txHash, txFinalFee } <-
      submitTxFromConstraintsReturningFee lookups constraints
    logInfo' $ "Tx ID: " <> show txHash
    awaitTxConfirmed txHash
    logInfo' "Tx submitted successfully!"
    pure { txFinalFee }

oneShotMintingPolicy :: TransactionInput -> Contract MintingPolicy
oneShotMintingPolicy =
  map PlutusMintingPolicy <<< oneShotMintingPolicyScript

oneShotMintingPolicyScript :: TransactionInput -> Contract PlutusScript
oneShotMintingPolicyScript txInput = do
  script <- liftMaybe (error "Error decoding oneShotMinting") do
    envelope <- decodeTextEnvelope oneShotMinting
    plutusScriptV1FromEnvelope envelope
  liftContractE $ mkOneShotMintingPolicy script txInput

mkOneShotMintingPolicy
  :: PlutusScript
  -> TransactionInput
  -> Either ApplyArgsError PlutusScript
mkOneShotMintingPolicy unappliedMintingPolicy oref =
  let
    mintingPolicyArgs :: Array PlutusData
    mintingPolicyArgs = Array.singleton (toData oref)
  in
    applyArgs unappliedMintingPolicy mintingPolicyArgs

foreign import oneShotMinting :: String
