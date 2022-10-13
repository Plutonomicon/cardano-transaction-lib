module Ctl.Examples.TxChaining
  ( main
  , example
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
import Contract.Monad (Contract, launchAff_, liftedE, liftedM, runContract)
import Contract.PlutusData (PlutusData)
import Contract.ScriptLookups as Lookups
import Contract.Test.E2E (publishTestFeedback)
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceAndSignTx
  , balanceAndSignTxWithConstraints
  , createAdditionalUtxos
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt as BigInt

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

  additionalUtxos <- createAdditionalUtxos balancedSignedTx0
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

