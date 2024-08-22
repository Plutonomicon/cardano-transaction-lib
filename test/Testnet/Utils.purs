module Test.Ctl.Testnet.Utils
  ( submitAndLog
  , getLockedInputs
  ) where

import Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Transaction (Transaction, awaitTxConfirmed, submit)
import Control.Monad.Reader (asks)
import Ctl.Internal.Types.UsedTxOuts (TxOutRefCache)
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

-- TODO: Get everything we can about a tx and confirm them
-- eg. outputs, metadata, datums, scripts
submitAndLog
  :: Transaction -> Contract Unit
submitAndLog bsTx = do
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ "Confirmed Tx ID: " <> show txId

getLockedInputs :: Contract TxOutRefCache
getLockedInputs = do
  cache <- asks _.usedTxOuts
  liftEffect $ Ref.read $ unwrap cache
