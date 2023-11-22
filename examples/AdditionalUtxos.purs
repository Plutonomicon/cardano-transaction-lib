module Ctl.Examples.AdditionalUtxos
  ( contract
  , main
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder)
import Contract.BalanceTxConstraints (mustUseAdditionalUtxos) as BalancerConstraints
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (Datum, PlutusData(Integer), unitRedeemer)
import Contract.ScriptLookups (ScriptLookups, UnbalancedTx)
import Contract.ScriptLookups (datum, unspentOutputs, validator) as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Sync (withoutSync)
import Contract.Transaction
  ( ScriptRef(NativeScriptRef)
  , TransactionInput
  , awaitTxConfirmed
  , balanceTxWithConstraints
  , createAdditionalUtxos
  , signTransaction
  , submit
  , withBalancedTx
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline, DatumWitness)
  , TxConstraints
  )
import Contract.TxConstraints
  ( mustPayToScript
  , mustPayToScriptWithScriptRef
  , mustSpendPubKeyOutput
  , mustSpendScriptOutput
  ) as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (UtxoMap)
import Contract.Value (Value)
import Contract.Value (lovelaceValueOf) as Value
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Data.Array (fromFoldable) as Array
import Data.Map (difference, filter, keys) as Map
import JS.BigInt (fromInt) as BigInt
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example contractParams =
  launchAff_ $ runContract contractParams $ contract false

contract :: Boolean -> Contract Unit
contract testAdditionalUtxoOverlap = withoutSync do
  logInfo' "Running Examples.AdditionalUtxos"
  validator <- alwaysSucceedsScriptV2
  let vhash = validatorHash validator
  { unbalancedTx, datum } <- payToValidator vhash
  withBalancedTx unbalancedTx \balancedTx -> do
    balancedSignedTx <- signTransaction balancedTx
    txHash <- submit balancedSignedTx
    when testAdditionalUtxoOverlap $ awaitTxConfirmed txHash
    logInfo' "Successfully locked two outputs at the validator address."

    additionalUtxos <- createAdditionalUtxos balancedSignedTx
    spendFromValidator validator additionalUtxos datum

payToValidator
  :: ValidatorHash -> Contract { unbalancedTx :: UnbalancedTx, datum :: Datum }
payToValidator vhash = do
  scriptRef <- liftEffect (NativeScriptRef <$> randomSampleOne arbitrary)
  let
    value :: Value
    value = Value.lovelaceValueOf $ BigInt.fromInt 2_000_000

    datum :: Datum
    datum = wrap $ Integer $ BigInt.fromInt 42

    constraints :: TxConstraints
    constraints =
      Constraints.mustPayToScript vhash datum DatumWitness value
        <> Constraints.mustPayToScriptWithScriptRef vhash datum DatumInline
          scriptRef
          value

    lookups :: ScriptLookups
    lookups = Lookups.datum datum

  unbalancedTx <- mkUnbalancedTx lookups constraints
  pure { unbalancedTx, datum }

spendFromValidator :: Validator -> UtxoMap -> Datum -> Contract Unit
spendFromValidator validator additionalUtxos datum = do
  let
    scriptUtxos :: UtxoMap
    scriptUtxos =
      additionalUtxos # Map.filter \out ->
        (unwrap (unwrap out).output).address
          == scriptHashAddress (validatorHash validator) Nothing

    scriptOrefs :: Array TransactionInput
    scriptOrefs = Array.fromFoldable $ Map.keys scriptUtxos

    pubKeyOrefs :: Array TransactionInput
    pubKeyOrefs =
      Array.fromFoldable $ Map.keys $ Map.difference additionalUtxos scriptUtxos

    constraints :: TxConstraints
    constraints =
      foldMap (flip Constraints.mustSpendScriptOutput unitRedeemer) scriptOrefs
        <> foldMap Constraints.mustSpendPubKeyOutput pubKeyOrefs

    lookups :: ScriptLookups
    lookups =
      Lookups.validator validator
        <> Lookups.unspentOutputs additionalUtxos
        <> Lookups.datum datum

    balancerConstraints :: BalanceTxConstraintsBuilder
    balancerConstraints =
      BalancerConstraints.mustUseAdditionalUtxos additionalUtxos

  unbalancedTx <- mkUnbalancedTx lookups constraints
  balancedTx <- balanceTxWithConstraints unbalancedTx balancerConstraints
  balancedSignedTx <- signTransaction balancedTx
  txHash <- submit balancedSignedTx

  awaitTxConfirmed txHash
  logInfo' "Successfully spent additional utxos from the validator address."
