-- | This module demonstrates the use of the Cip30 functions 
-- | using the `KeyWallet` provided by CTL
module Ctl.Examples.KeyWallet.Cip30
  ( main
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Wallet (getChangeAddress, getNetworkId, getRewardAddresses, getUnusedAddresses, signData)
import Control.Monad.Error.Class (liftMaybe, try)
import Ctl.Examples.KeyWallet.Internal.Cip30Contract (runKeyWalletContract_)
import Ctl.Internal.Types.RawBytes (RawBytes)
import Data.Array (head)
import Effect.Exception (error)

main :: Effect Unit
main = runKeyWalletContract_ mkContract

mkContract :: RawBytes -> Contract () Unit
mkContract dat = do
  logInfo' "Running Examples.KeyWallet.Cip30"
  logInfo' "Funtions that depend on `Contract`"
  _ <- performAndLog "getNetworkId" getNetworkId
  _ <- performAndLog "getUnusedAddresses" getUnusedAddresses
  mChangeAddress <- performAndLog "getChangeAddress" getChangeAddress
  changeAddress <- liftMaybe (error "can't get change address") mChangeAddress
  _ <- performAndLog "signData changeAddress" $ try $ signData changeAddress dat
  mRewardAddress <- performAndLog "getRewardAddresses" getRewardAddresses
  rewardAddr <- liftMaybe (error "can't get change address")
    (mRewardAddress >>= head)
  _ <- performAndLog "signData rewardAddress" $ try $  signData rewardAddr dat
  pure unit
  where

  performAndLog
    :: forall (a :: Type)
     . Show a
    => String
    -> Contract () a
    -> Contract () a
  performAndLog logMsg cont = do
    result <- cont
    logInfo' $ logMsg <> ": " <> show result
    pure result

