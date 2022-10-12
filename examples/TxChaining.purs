module Ctl.Examples.TxChaining
  ( main
  , example
  , createAdditionalUtxos
  , contract
  ) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.BalanceTxConstraints
  ( BalanceTxConstraintsBuilder
  , mustUseAdditionalUtxos
  ) as BalanceTxConstraints
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
import Contract.PlutusData (PlutusData)
import Contract.ScriptLookups as Lookups
import Contract.Test.E2E (publishTestFeedback)
import Contract.Transaction
  ( Transaction
  , TransactionInput(TransactionInput)
  , awaitTxConfirmed
  , balanceAndSignTx
  , balanceAndSignTxWithConstraints
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap)
import Contract.Value as Value
import Ctl.Internal.Hashing (transactionHash)
import Ctl.Internal.Plutus.Conversion (toPlutusTxOutputWithRefScript)
import Ctl.Internal.Serialization (convertTransaction)
import Data.BigInt as BigInt
import Data.Map as Map

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true

contract :: Contract () Unit
contract = do
  pkh <- liftedM "Failed to get PKH" $ ownPaymentPubKeyHash
  let
    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToPubKey pkh
        (Value.lovelaceValueOf $ BigInt.fromInt 1_000_000)

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  unbalancedTx0 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  balancedSignedTx0 <- balanceAndSignTx unbalancedTx0

  additionalUtxos <- createAdditionalUtxos (unwrap balancedSignedTx0)
  logInfo' $ "Additional utxos: " <> show additionalUtxos

  let
    balanceTxConstraints :: BalanceTxConstraints.BalanceTxConstraintsBuilder
    balanceTxConstraints =
      BalanceTxConstraints.mustUseAdditionalUtxos additionalUtxos

  unbalancedTx1 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  balancedSignedTx1 <-
    balanceAndSignTxWithConstraints (unbalancedTx1 /\ balanceTxConstraints)

  txId0 <- submit balancedSignedTx0
  txId1 <- submit balancedSignedTx1

  awaitTxConfirmed txId0
  awaitTxConfirmed txId1

createAdditionalUtxos
  :: forall r
   . Transaction
  -> Contract r UtxoMap
createAdditionalUtxos tx = do
  cslTx <- liftEffect $ convertTransaction tx

  let
    transactionId = transactionHash cslTx
    txBody = tx # unwrap # _.body # unwrap
    txin index = TransactionInput { transactionId, index }

  plutusOutputs <-
    liftContractM "Unable to convert to Plutus outputs"
      (traverse toPlutusTxOutputWithRefScript txBody.outputs)

  pure $ plutusOutputs #
    foldl (\utxo txout -> Map.insert (txin $ length utxo) txout utxo) Map.empty

