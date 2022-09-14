-- | This module demonstrates how `applyArgs` from `Contract.Scripts` can be 
-- | used to build scripts with the provided arguments applied. It creates a 
-- | transaction that mints an NFT using the one-shot minting policy.
module CTL.Examples.OneShotMinting (main, example, contract) where

import CTL.Contract.Prelude

import CTL.Contract.Address (Address, getWalletAddress)
import CTL.Contract.Config (ConfigParams, testnetNamiConfig)
import CTL.Contract.Log (logInfo')
import CTL.Contract.Monad
  ( Contract
  , launchAff_
  , liftContractM
  , liftedE
  , liftedM
  , runContract
  )
import CTL.Contract.PlutusData (PlutusData, toData)
import CTL.Contract.ScriptLookups as Lookups
import CTL.Contract.Scripts (MintingPolicy, applyArgs)
import CTL.Contract.Test.E2E (publishTestFeedback)
import CTL.Contract.Test.Utils (ContractWrapAssertion, Labeled, label)
import CTL.Contract.Test.Utils as TestUtils
import CTL.Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import CTL.Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , plutusV1Script
  )
import CTL.Contract.TxConstraints as Constraints
import CTL.Contract.Utxos (utxosAt)
import CTL.Contract.Value (CurrencySymbol, TokenName)
import CTL.Contract.Value (singleton) as Value
import CTL.Examples.Helpers
  ( buildBalanceSignAndSubmitTx'
  , mkCurrencySymbol
  , mkTokenName
  ) as Helpers
import Data.Array (head, singleton) as Array
import Data.BigInt (BigInt)
import Data.Map (toUnfoldable) as Map

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true

mkAssertions
  :: Address
  -> (CurrencySymbol /\ TokenName /\ BigInt)
  -> Array (ContractWrapAssertion () { txFinalFee :: BigInt })
mkAssertions ownAddress nft =
  let
    labeledOwnAddress :: Labeled Address
    labeledOwnAddress = label ownAddress "ownAddress"
  in
    [ TestUtils.assertTokenGainAtAddress' labeledOwnAddress nft

    , TestUtils.assertLossAtAddress labeledOwnAddress
        \{ txFinalFee } -> pure txFinalFee
    ]

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.OneShotMinting"

  ownAddress <- liftedM "Failed to get own address" getWalletAddress
  utxos <- liftedM "Failed to get utxo set" $ utxosAt ownAddress
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (Map.toUnfoldable utxos :: Array _))

  mp /\ cs <- Helpers.mkCurrencySymbol (oneShotMintingPolicy oref)
  tn <- Helpers.mkTokenName "CTLNFT"

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValue (Value.singleton cs tn one)
        <> Constraints.mustSpendPubKeyOutput oref

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs utxos

  let assertions = mkAssertions ownAddress (cs /\ tn /\ one)
  void $ TestUtils.withAssertions assertions do
    { txHash, txFinalFee } <-
      Helpers.buildBalanceSignAndSubmitTx' lookups constraints

    awaitTxConfirmed txHash
    logInfo' "Tx submitted successfully!"
    pure { txFinalFee }

foreign import oneShotMinting :: String

oneShotMintingPolicy :: TransactionInput -> Contract () MintingPolicy
oneShotMintingPolicy oref = do
  unappliedMintingPolicy <-
    map (wrap <<< plutusV1Script)
      (textEnvelopeBytes oneShotMinting PlutusScriptV1)
  let
    mintingPolicyArgs :: Array PlutusData
    mintingPolicyArgs = Array.singleton (toData oref)

  liftedE $ applyArgs unappliedMintingPolicy mintingPolicyArgs

