-- | This module creates a transaction which produces outputs including:
-- |   1. an output containing an inline datum
-- |   2. an output containing a datum hash
-- |
-- | Then checks are performed to verify that these outputs have actually been
-- | created and that they contain given datums.
module Ctl.Examples.PaysWithDatum (contract, example, main) where

import Contract.Prelude

import Cardano.Types (TransactionOutput)
import Cardano.Types.BigNum as BigNum
import Contract.Address (Address)
import Contract.Config
  ( ContractParams
  , KnownWallet(Nami)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Hashing (datumHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.PlutusData
  ( DataHash
  , OutputDatum(OutputDatum, OutputDatumHash)
  , PlutusData(Integer)
  )
import Contract.ScriptLookups as Lookups
import Contract.Test.Assert
  ( ContractAssertionFailure(CustomFailure)
  , ContractCheck
  , assertContract
  , assertNewUtxosAtAddress
  , assertionToCheck
  , label
  , runChecks
  )
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(DatumInline, DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Value (Value)
import Contract.Value (lovelaceValueOf) as Value
import Contract.Wallet
  ( getWalletAddresses
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  )
import Control.Monad.Trans.Class (lift)
import Ctl.Examples.Helpers (mustPayToPubKeyStakeAddressWithDatum)
import Data.Array (head)
import JS.BigInt (fromInt) as BigInt

type ContractResult =
  { address :: Address
  , txHash :: TransactionHash
  , datum :: PlutusData
  , datumHash :: DataHash
  }

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Nami) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.PaysWithDatum"

  pkh <- liftedM "Could not get own PKH" (head <$> ownPaymentPubKeyHashes)
  skh <- join <<< head <$> ownStakePubKeyHashes
  address <- liftedM "Could not get own address" (head <$> getWalletAddresses)

  let
    datum = Integer $ BigInt.fromInt 42
    datumHash' = datumHash datum

    value :: Value
    value = Value.lovelaceValueOf (BigNum.fromInt 2_000_000)

    constraints :: Constraints.TxConstraints
    constraints =
      mustPayToPubKeyStakeAddressWithDatum pkh skh datum DatumWitness value
        <> mustPayToPubKeyStakeAddressWithDatum pkh skh datum DatumInline value

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  void $ runChecks checks $ lift do
    txHash <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txHash
    logInfo' "Tx submitted successfully!"
    pure { address, txHash, datum, datumHash: datumHash' }

checks :: Array (ContractCheck ContractResult)
checks =
  [ assertTxCreatesOutputWithInlineDatum, assertTxCreatesOutputWithDatumHash ]

assertTxCreatesOutputWithInlineDatum
  :: ContractCheck ContractResult
assertTxCreatesOutputWithInlineDatum = assertionToCheck
  "Contains an output with inline datum"
  \{ address, txHash, datum } -> do
    let
      assertionFailure :: ContractAssertionFailure
      assertionFailure =
        CustomFailure "Could not find output with given inline datum"
    assertNewUtxosAtAddress (label address "ownAddress") txHash
      \outputs ->
        assertContract assertionFailure $
          hasOutputWithOutputDatum (OutputDatum datum) outputs

assertTxCreatesOutputWithDatumHash
  :: ContractCheck ContractResult
assertTxCreatesOutputWithDatumHash = assertionToCheck
  "Contains an output with a given datum hash"
  \{ address, txHash, datumHash } -> do
    let
      assertionFailure :: ContractAssertionFailure
      assertionFailure =
        CustomFailure "Could not find output with given datum hash"
    assertNewUtxosAtAddress (label address "ownAddress") txHash
      \outputs ->
        assertContract assertionFailure $
          hasOutputWithOutputDatum (OutputDatumHash datumHash) outputs

hasOutputWithOutputDatum
  :: OutputDatum -> Array TransactionOutput -> Boolean
hasOutputWithOutputDatum datum =
  any (eq (Just datum) <<< _.datum <<< unwrap)
