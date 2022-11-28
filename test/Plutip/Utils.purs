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
  , getTxByHash
  , submit
  )
import Control.Monad.Reader (asks)
import Ctl.Internal.Types.UsedTxOuts (TxOutRefCache)
import Data.Maybe (Maybe(Just), isNothing)
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref

submitAndLog
  :: BalancedSignedTransaction -> Contract Unit
submitAndLog bsTx = do
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  awaitTxConfirmed txId
  mbTransaction <- getTxByHash txId
  logInfo' $ "Tx: " <> show mbTransaction
  liftEffect $ when (isNothing mbTransaction) do
    void $ throw "Unable to get Tx contents"
    when (mbTransaction /= Just (unwrap bsTx)) do
      throw "Tx contents do not match"

getLockedInputs :: Contract TxOutRefCache
getLockedInputs = do
  cache <- asks _.usedTxOuts
  liftEffect $ Ref.read $ unwrap cache
