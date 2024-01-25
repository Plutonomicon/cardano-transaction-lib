-- | This module demonstrates how to enable transaction chaining when submitting
-- | multiple transactions in the correct order; and how subsequent transactions
-- | can use utxos created in the previous ones without waiting for them to
-- | leave the mempool and be included in the block.
module Ctl.Examples.TxChaining
  ( main
  , example
  , contract
  ) where

import Contract.Prelude

import Contract.BalanceTxConstraints
  ( BalanceTxConstraintsBuilder
  , mustUseAdditionalUtxos
  ) as BalanceTxConstraints
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTxWithConstraints
  , createAdditionalUtxos
  , signTransaction
  , submit
  , withBalancedTx
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value as Value
import Contract.Wallet (ownPaymentPubKeyHashes)
import Data.Array (head)
import JS.BigInt as BigInt

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

contract :: Contract Unit
contract = do
  pkh <- liftedM "Failed to get PKH" $ head <$> ownPaymentPubKeyHashes
  let
    constraints :: TxConstraints
    constraints =
      Constraints.mustPayToPubKey pkh
        (Value.lovelaceValueOf $ BigInt.fromInt 1_000_000)

    lookups0 :: Lookups.ScriptLookups
    lookups0 = mempty

  unbalancedTx0 <- mkUnbalancedTx lookups0 constraints

  withBalancedTx unbalancedTx0 \balancedTx0 -> do
    balancedSignedTx0 <- signTransaction balancedTx0

    additionalUtxos <- createAdditionalUtxos balancedSignedTx0
    logInfo' $ "Additional utxos: " <> show additionalUtxos

    let
      lookups1 :: Lookups.ScriptLookups
      lookups1 = Lookups.unspentOutputs additionalUtxos

      balanceTxConstraints :: BalanceTxConstraints.BalanceTxConstraintsBuilder
      balanceTxConstraints =
        BalanceTxConstraints.mustUseAdditionalUtxos additionalUtxos

    unbalancedTx1 <- mkUnbalancedTx lookups1 constraints
    balancedTx1 <- balanceTxWithConstraints unbalancedTx1 balanceTxConstraints
    balancedSignedTx1 <- signTransaction balancedTx1

    txId0 <- submit balancedSignedTx0
    txId1 <- submit balancedSignedTx1

    awaitTxConfirmed txId0
    awaitTxConfirmed txId1
