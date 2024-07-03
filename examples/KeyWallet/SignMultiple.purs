module Ctl.Examples.KeyWallet.SignMultiple where

import Contract.Prelude

import Cardano.Types (Transaction)
import Contract.Log (logInfo')
import Contract.Monad (Contract, throwContractError)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , signTransaction
  , submit
  , withBalancedTxs
  )
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value (lovelaceValueOf) as Value
import Control.Monad.Reader (asks)
import Ctl.Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)
import Data.Map (Map)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.UInt (UInt)
import Effect.Ref (read) as Ref

getLockedInputs
  :: Contract (Map TransactionHash (Set UInt))
getLockedInputs = do
  cache <- asks _.usedTxOuts
  liftEffect $ Ref.read $ unwrap cache

main :: Effect Unit
main = runKeyWalletContract_ \pkh lovelace unlock -> do
  logInfo' "Running Examples.KeyWallet.SignMultiple"

  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustPayToPubKey pkh $
      Value.lovelaceValueOf lovelace

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  unbalancedTx0 /\ usedUtxos0 <- mkUnbalancedTx lookups constraints
  unbalancedTx1 /\ usedUtxos1 <- mkUnbalancedTx lookups constraints

  txIds <-
    withBalancedTxs
      [ { transaction: unbalancedTx0
        , usedUtxos: usedUtxos0
        , balancerConstraints: mempty
        }
      , { transaction: unbalancedTx1
        , usedUtxos: usedUtxos1
        , balancerConstraints: mempty
        }
      ] $ \balancedTxs -> do
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
    :: Transaction
    -> Contract TransactionHash
  submitAndLog bsTx = do
    txId <- submit bsTx
    logInfo' $ "Tx ID: " <> show txId
    pure txId
