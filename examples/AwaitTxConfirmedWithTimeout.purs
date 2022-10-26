-- | This module tests that `awaitTxConfirmedWithTimeout` is actually
-- | interrupted after the given timeout elapses. To do that, it awaits
-- | a fake TX id that will never succeed and catches the resulting exception
-- | when the timeout elapses.
module Ctl.Examples.AwaitTxConfirmedWithTimeout
  ( contract
  , main
  , example
  ) where

import Contract.Prelude

import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract, throwContractError)
import Contract.Transaction (awaitTxConfirmedWithTimeout)
import Control.Monad.Error.Class (try)
-- TODO Re-export into Contract or drop the usage
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1042
import Ctl.Internal.Types.ByteArray (hexToByteArrayUnsafe)
import Ctl.Internal.Types.Transaction (TransactionHash(TransactionHash))

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

contract :: Contract () Unit
contract = do
  logInfo' "Running AwaitTxConfirmedWithTimeout"
  let
    fakeHash = TransactionHash $ hexToByteArrayUnsafe
      "ffffffffffff55555555555555555555a1af1b7534b51e60fad3fe9c164313e8"
  result <- try $ awaitTxConfirmedWithTimeout (wrap 1.0) fakeHash
  case result of
    Left _ -> pure unit
    Right _ -> throwContractError "expected awaitTxConfirmedWithTimeOut to fail"
