-- | This module balances and signs two transactions at once and demonstrates
-- | the `withBalancedandSignedTxs` bracket. The point is that two different
-- | Utxos will be used for these transactions.
module Ctl.Examples.SignMultiple (example, contract, main) where

import Contract.Prelude

import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo', logWarn')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftedM
  , runContract
  , throwContractError
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( BalancedSignedTransaction
  , TransactionHash
  , awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , signTransaction
  , submit
  , submitTxFromConstraints
  , withBalancedTxs
  )
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value (leq)
import Contract.Value as Value
import Contract.Wallet
  ( getWalletUtxos
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  )
import Control.Monad.Reader (asks)
import Data.Array (head)
import Data.Map (Map, filter)
import Data.Set (Set)
import Data.UInt (UInt)
import Effect.Ref as Ref
import JS.BigInt as BigInt

getLockedInputs
  :: Contract (Map TransactionHash (Set UInt))
getLockedInputs = do
  cache <- asks _.usedTxOuts
  liftEffect $ Ref.read $ unwrap cache

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.SignMultiple"
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  skh <- liftedM "Failed to get own SKH" $ join <<< head <$>
    ownStakePubKeyHashes

  -- Early fail if not enough utxos present for 2 transactions
  unlessM hasSufficientUtxos do
    logWarn' "Insufficient Utxos for 2 transactions"
    createAdditionalUtxos

  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustPayToPubKeyAddress pkh skh
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  unbalancedTx0 <- mkUnbalancedTx lookups constraints
  unbalancedTx1 <- mkUnbalancedTx lookups constraints

  txIds <- withBalancedTxs [ unbalancedTx0, unbalancedTx1 ] $ \balancedTxs -> do
    locked <- getLockedInputs
    logInfo' $ "Locked inputs inside bracket (should be nonempty): "
      <> show locked
    traverse (submitAndLog <=< signTransaction) balancedTxs

  locked <- getLockedInputs
  logInfo' $ "Locked inputs after bracket (should be empty): " <> show locked

  case txIds of
    [ txId0, txId1 ] -> do
      awaitTxConfirmed txId0
      logInfo' $ "Tx 0 submitted successfully!"
      awaitTxConfirmed txId1
      logInfo' $ "Tx 1 submitted successfully!"
    _ -> throwContractError "Unexpected error - no transaction IDs"

  where
  submitAndLog
    :: BalancedSignedTransaction
    -> Contract TransactionHash
  submitAndLog bsTx = do
    txId <- submit bsTx
    logInfo' $ "Tx ID: " <> show txId
    pure txId

  hasSufficientUtxos :: Contract Boolean
  hasSufficientUtxos = do
    let
      -- 4 Ada: enough to cover 2 Ada transfer and fees
      isUtxoValid u = (Value.lovelaceValueOf $ BigInt.fromInt 4_000_000) `leq`
        (unwrap (unwrap u).output).amount

    walletValidUtxos <- liftedM "Failed to get wallet Utxos"
      $ map (filter isUtxoValid)
      <$> getWalletUtxos

    pure $ length walletValidUtxos >= 2 -- 2 transactions

createAdditionalUtxos :: Contract Unit
createAdditionalUtxos = do
  logInfo' "Creating additional UTxOs for SignMultiple example"
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  skh <- liftedM "Failed to get own SKH" $ join <<< head <$>
    ownStakePubKeyHashes

  let
    constraints :: Constraints.TxConstraints
    constraints =
      Constraints.mustPayToPubKeyAddress pkh skh
        ( Value.lovelaceValueOf
            $ BigInt.fromInt 2_000_000
        ) <>
        Constraints.mustPayToPubKeyAddress pkh skh
          ( Value.lovelaceValueOf
              $ BigInt.fromInt 2_000_000
          )

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmedWithTimeout (wrap 100.0) txId
  logInfo' $ "Tx submitted successfully!"

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
