-- | This module demonstrates the use of the Cip30 functions
-- | using the `KeyWallet` provided by CTL
module Ctl.Examples.KeyWallet.Cip30
  ( main
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Prim.ByteArray (RawBytes)
import Contract.Wallet
  ( getChangeAddress
  , getRewardAddresses
  , getUnusedAddresses
  , signData
  )
import Control.Monad.Error.Class (try)
import Ctl.Examples.KeyWallet.Internal.Cip30Contract (runKeyWalletContract_)
import Data.Array (head)

main :: Effect Unit
main = runKeyWalletContract_ mkContract

mkContract :: RawBytes -> Contract Unit
mkContract dat = do
  logInfo' "Running Examples.KeyWallet.Cip30"
  logInfo' "Functions that depend on `Contract`"
  _ <- performAndLog "getUnusedAddresses" getUnusedAddresses
  changeAddress <- performAndLog "getChangeAddress" getChangeAddress
  _ <- performAndLog "signData changeAddress" $ try $ signData changeAddress dat
  rewardAddress <- performAndLog "getRewardAddresses" $
    liftedM "Could not get reward address" (head <$> getRewardAddresses)
  _ <- performAndLog "signData rewardAddress" $ try $ signData rewardAddress dat
  pure unit
  where

  performAndLog
    :: forall (a :: Type)
     . Show a
    => String
    -> Contract a
    -> Contract a
  performAndLog logMsg cont = do
    result <- cont
    logInfo' $ logMsg <> ": " <> show result
    pure result
