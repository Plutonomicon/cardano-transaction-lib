module Test.Ctl.Plutip.Utils
  ( submitAndLog
  , getLockedInputs
  ) where

import Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Transaction
  ( BalancedSignedTransaction
  , awaitTxConfirmed
  , submit
  )
import Control.Monad.Reader (asks)
import Ctl.Internal.UsedTxOuts (UsedTxOuts)
import Ctl.Internal.UsedTxOuts as UsedTxOuts
import Effect.Class (liftEffect)

submitAndLog
  :: BalancedSignedTransaction -> Contract Unit
submitAndLog bsTx = do
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ "Confirmed Tx ID: " <> show txId

getLockedInputs :: Contract UsedTxOuts
getLockedInputs =
  liftEffect <<< UsedTxOuts.get =<< asks _.storage
