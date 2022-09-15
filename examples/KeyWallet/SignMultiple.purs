module CTL.Examples.KeyWallet.SignMultiple where

import CTL.Contract.Prelude

import CTL.Contract.Log (logInfo')
import CTL.Contract.Monad (Contract, liftedE, throwContractError)
import CTL.Contract.ScriptLookups as Lookups
import CTL.Contract.Transaction
  ( BalancedSignedTransaction
  , TransactionHash
  , awaitTxConfirmed
  , submit
  , withBalancedAndSignedTxs
  )
import CTL.Contract.TxConstraints as Constraints
import CTL.Contract.Value (lovelaceValueOf) as Value
import CTL.Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)
-- TODO Re-export into Contract or drop the usage
import CTL.Internal.Types.UsedTxOuts (TxOutRefCache)
import Control.Monad.Reader (asks)
import Data.Newtype (unwrap)
import Effect.Ref (read) as Ref

getLockedInputs :: forall (r :: Row Type). Contract r TxOutRefCache
getLockedInputs = do
  cache <- asks (_.usedTxOuts <<< _.runtime <<< unwrap)
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
      awaitTxConfirmed txId1
      logInfo' $ "Tx 1 submitted successfully!"
      awaitTxConfirmed txId2
      logInfo' $ "Tx 2 submitted successfully!"
    _ -> throwContractError "Unexpected error - no transaction IDs"

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
