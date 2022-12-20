-- | This module creates a transaction which produces outputs including:
-- |   1. an output containing an inline datum
-- |   2. an output containing a datum hash
-- |
-- | Then checks are performed to verify that these outputs have actually been
-- | created and that they contain given datums.
module Ctl.Examples.PaysWithDatum (contract, example, main) where

import Contract.Prelude

import Contract.Address
  ( Address
  , getWalletAddresses
  , ownPaymentPubKeysHashes
  , ownStakePubKeysHashes
  )
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Hashing (datumHash)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftedM
  , runContract
  )
import Contract.PlutusData
  ( DataHash
  , Datum(Datum)
  , OutputDatum(OutputDatum, OutputDatumHash)
  , PlutusData(Integer)
  )
import Contract.ScriptLookups as Lookups
import Contract.Test.Utils
  ( ContractAssertionFailure(CustomFailure)
  , ContractBasicAssertion
  , label
  )
import Contract.Test.Utils
  ( assertContract
  , checkNewUtxosAtAddress
  , runContractAssertionM'
  , withAssertions
  ) as TestUtils
import Contract.Transaction
  ( TransactionHash
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(DatumInline, DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Value (Value)
import Contract.Value (lovelaceValueOf) as Value
import Ctl.Examples.Helpers (mustPayToPubKeyStakeAddressWithDatum)
import Data.Array (head)
import Data.BigInt (fromInt) as BigInt

type ContractResult =
  { address :: Address
  , txHash :: TransactionHash
  , datum :: Datum
  , datumHash :: DataHash
  }

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.PaysWithDatum"

  pkh <- liftedM "Could not get own PKH" (head <$> ownPaymentPubKeysHashes)
  skh <- join <<< head <$> ownStakePubKeysHashes
  address <- liftedM "Could not get own address" (head <$> getWalletAddresses)

  let
    datum = Datum $ Integer $ BigInt.fromInt 42
    datumHash' = datumHash datum

    value :: Value
    value = Value.lovelaceValueOf (BigInt.fromInt 2_000_000)

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      mustPayToPubKeyStakeAddressWithDatum pkh skh datum DatumWitness value
        <> mustPayToPubKeyStakeAddressWithDatum pkh skh datum DatumInline value

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  void $ TestUtils.withAssertions assertions do
    txHash <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txHash
    logInfo' "Tx submitted successfully!"
    pure { address, txHash, datum, datumHash: datumHash' }

assertions :: Array (ContractBasicAssertion () ContractResult Unit)
assertions =
  [ assertTxCreatesOutputWithInlineDatum, assertTxCreatesOutputWithDatumHash ]

assertTxCreatesOutputWithInlineDatum
  :: ContractBasicAssertion () ContractResult Unit
assertTxCreatesOutputWithInlineDatum { address, txHash, datum } =
  let
    assertionFailure :: ContractAssertionFailure
    assertionFailure =
      CustomFailure "Could not find output with given inline datum"
  in
    TestUtils.runContractAssertionM' $
      TestUtils.checkNewUtxosAtAddress (label address "ownAddress") txHash
        \outputs ->
          TestUtils.assertContract assertionFailure $
            hasOutputWithOutputDatum (OutputDatum datum) outputs

assertTxCreatesOutputWithDatumHash
  :: ContractBasicAssertion () ContractResult Unit
assertTxCreatesOutputWithDatumHash { address, txHash, datumHash } =
  let
    assertionFailure :: ContractAssertionFailure
    assertionFailure =
      CustomFailure "Could not find output with given datum hash"
  in
    TestUtils.runContractAssertionM' $
      TestUtils.checkNewUtxosAtAddress (label address "ownAddress") txHash
        \outputs ->
          TestUtils.assertContract assertionFailure $
            hasOutputWithOutputDatum (OutputDatumHash datumHash) outputs

hasOutputWithOutputDatum
  :: OutputDatum -> Array TransactionOutputWithRefScript -> Boolean
hasOutputWithOutputDatum datum =
  any (eq datum <<< _.datum <<< unwrap <<< _.output <<< unwrap)
