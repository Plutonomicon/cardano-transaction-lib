-- | This module demonstrates how `applyArgs` from `Contract.Scripts` can be 
-- | used to build scripts with the provided arguments applied. It creates a 
-- | transaction that mints an NFT using the one-shot minting policy.
module Ctl.Examples.OneShotMinting
  ( contract
  , example
  , main
  , mkContractWithAssertions
  , mkOneShotMintingPolicy
  ) where

import Contract.Prelude

import Contract.Address (Address, getWalletAddress)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractM
  , liftedE
  , liftedM
  , runContract
  )
import Contract.PlutusData (PlutusData, toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , PlutusScript
  , applyArgs
  )
import Contract.Test.E2E (publishTestFeedback)
import Contract.Test.Utils (ContractWrapAssertion, Labeled, label)
import Contract.Test.Utils as TestUtils
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , plutusV1Script
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value (singleton) as Value
import Ctl.Examples.Helpers
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
contract =
  mkContractWithAssertions "Examples.OneShotMinting" oneShotMintingPolicy

mkContractWithAssertions
  :: String
  -> (TransactionInput -> Contract () MintingPolicy)
  -> Contract () Unit
mkContractWithAssertions exampleName mkMintingPolicy = do
  logInfo' ("Running " <> exampleName)

  ownAddress <- liftedM "Failed to get own address" getWalletAddress
  utxos <- liftedM "Failed to get utxo set" $ utxosAt ownAddress
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (Map.toUnfoldable utxos :: Array _))

  mp /\ cs <- Helpers.mkCurrencySymbol (mkMintingPolicy oref)
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
oneShotMintingPolicy =
  mkOneShotMintingPolicy oneShotMinting PlutusScriptV1 plutusV1Script

mkOneShotMintingPolicy
  :: String
  -> TextEnvelopeType
  -> (ByteArray -> PlutusScript)
  -> TransactionInput
  -> Contract () MintingPolicy
mkOneShotMintingPolicy json ty mkPlutusScript oref = do
  unappliedMintingPolicy <-
    map mkPlutusScript (textEnvelopeBytes json ty)
  let
    mintingPolicyArgs :: Array PlutusData
    mintingPolicyArgs = Array.singleton (toData oref)

  liftedE $ map PlutusMintingPolicy <$> applyArgs unappliedMintingPolicy
    mintingPolicyArgs

