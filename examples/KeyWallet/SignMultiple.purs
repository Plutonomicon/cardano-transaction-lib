module Examples.KeyWallet.SignMultiple where

import Contract.Prelude

import Contract.Monad (Contract, liftedE, logInfo', logError')
import Control.Monad.Reader (asks)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( BalancedSignedTransaction
  , TransactionHash
  , awaitTxConfirmedWithTimeout
  , submit
  , withBalancedAndSignedTxs
  )
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf) as Value
import Effect.Ref (read) as Ref
import Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)
import Types.UsedTxOuts (TxOutRefCache)

getLockedInputs :: forall (r :: Row Type). Contract r TxOutRefCache
getLockedInputs = do
  cache <- asks (_.usedTxOuts <<< unwrap)
  liftEffect $ Ref.read $ unwrap cache

main :: Effect Unit
main = runKeyWalletContract_ \pkh lovelace unlock -> do
  logInfo' "Running Examples.KeyWallet.SignMultiple"

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToPubKey pkh $
      Value.lovelaceValueOf lovelace

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  ubTx1 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  ubTx2 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints

  txIds <- withBalancedAndSignedTxs [ ubTx1, ubTx2 ] $ \txs -> do
    locked <- getLockedInputs
    logInfo' $ "Locked inputs inside bracket (should be nonempty): "
      <> show locked
    traverse submitAndLog txs

  locked <- getLockedInputs
  logInfo' $ "Locked inputs after bracket (should be empty): " <> show locked

  case txIds of
    [ txId1, txId2 ] -> do
      awaitTxConfirmedWithTimeout (wrap 120.0) txId1
      logInfo' $ "Tx 1 submitted successfully!"
      awaitTxConfirmedWithTimeout (wrap 120.0) txId2
      logInfo' $ "Tx 2 submitted successfully!"
    _ -> logError' "Unexpected error - no transaction IDs"

  liftEffect unlock
  where
  submitAndLog
    :: forall (r :: Row Type)
     . BalancedSignedTransaction
    -> Contract r TransactionHash
  submitAndLog bsTx = do
    txId <- submit bsTx
    logInfo' $ "Tx ID: " <> show txId
    pure txId
