module Ctl.Examples.KeyWallet.SignMultiple where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, throwContractError)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( BalancedSignedTransaction
  , TransactionHash
  , TxOutRefCache
  , awaitTxConfirmed
  , signTransaction
  , submit
  , withBalancedTxs
  )
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf) as Value
import Control.Monad.Reader (asks)
import Ctl.Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)
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

  unbalancedTx0 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  unbalancedTx1 <- liftedE $ Lookups.mkUnbalancedTx lookups constraints

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
