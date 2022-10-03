-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address
module Ctl.Examples.Cip30
  ( main
  ) where

import Contract.Prelude

import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractAffM, runContract)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Wallet
  ( getChangeAddress
  , getNetworkId
  , getRewardAddresses
  , getUnusedAddresses
  , getWallet
  , isEnabled
  , signData
  , supportedWalletToName
  , walletToSupportedWallet
  )
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)
import Ctl.Internal.Types.RawBytes (rawBytesFromAscii)
import Effect.Aff (try)
import Effect.Exception (error)

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg (contract false)
  liftEffect $ runKeyWalletContract_ (\_ _ _ -> contract true)
  publishTestFeedback true

contract :: Boolean -> Contract () Unit
contract catch = do
  logInfo' "Running Examples.Cip30"
  mWallet <- getWallet
  let mSupportWallet =walletToSupportedWallet <$> mWallet 
  logInfo' $ show $ supportedWalletToName <$> mSupportWallet
  performAndLog catch "isEnabled" (liftAff $ (traverse isEnabled mSupportWallet))
  performAndLog catch "getNetworkId" getNetworkId
  performAndLog catch "getUnusedAddresses" getUnusedAddresses
  performAndLog false "getChangeAddress" getChangeAddress
  performAndLog false "getRewardAddresses" getRewardAddresses
  maddress <- getChangeAddress
  address <- liftMaybe (error "can't get change address") maddress
  dataBytes <- liftContractAffM
    ("can't convert : " <> msg <> " to RawBytes")
    (pure mDataBytes)
  performAndLog catch "signData" $ signData address dataBytes
  where
  msg = "hello world!"
  mDataBytes = rawBytesFromAscii msg

performAndLog
  :: forall (a :: Type)
   . Show a
  => Boolean
  -> String
  -> Contract () a
  -> Contract () Unit
performAndLog catch msg cont = do
  result <- if catch then try cont else pure <$> cont
  logInfo' $ msg <> ": " <> show result

