-- | This module demonstrates the use of the CIP-30 functions
-- | using an external wallet.
module Ctl.Examples.Cip30
  ( main
  , example
  , contract
  , noSignDataContract
  ) where

import Contract.Prelude

import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractAffM, runContract)
import Contract.Wallet
  ( apiVersion
  , getChangeAddress
  , getNetworkId
  , getRewardAddresses
  , getUnusedAddresses
  , getWallet
  , icon
  , isEnabled
  , isWalletAvailable
  , name
  , signData
  , walletToWalletExtension
  )
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Types.RawBytes (rawBytesFromAscii)
import Ctl.Internal.Wallet (WalletExtension)
import Data.Array (head)
import Effect.Exception (error)

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  mWallet <- runContract cfg getWallet
  let mSupportWallet = walletToWalletExtension =<< mWallet
  _ <- traverse nonConfigFunctions mSupportWallet
  runContract cfg contract

nonConfigFunctions :: WalletExtension -> Aff Unit
nonConfigFunctions extensionWallet = do
  log "Functions that don't depend on `Contract`"
  performAndLog "isWalletAvailable" (liftEffect <<< isWalletAvailable)
  performAndLog "isEnabled" isEnabled
  performAndLog "apiVersion" apiVersion
  performAndLog "name" name
  performAndLog "icon" icon
  where
  performAndLog
    :: forall (a :: Type)
     . Show a
    => String
    -> (WalletExtension -> Aff a)
    -> Aff Unit
  performAndLog msg f = do
    result <- f extensionWallet
    log $ msg <> ":" <> (show result)

-- a version without the `signData` call, which is not implemented for KeyWallet
noSignDataContract :: Contract () Unit
noSignDataContract = do
  logInfo' "Running Examples.Cip30"
  logInfo' "Funtions that depend on `Contract`"
  _ <- performAndLog "getNetworkId" getNetworkId
  _ <- performAndLog "getUnusedAddresses" getUnusedAddresses
  mRewardAddress <- performAndLog "getRewardAddresses" getRewardAddresses
  _ <- liftMaybe (error "can't get reward address") (mRewardAddress >>= head)
  mChangeAddress <- performAndLog "getChangeAddress" getChangeAddress
  _ <- liftMaybe (error "can't get change address") mChangeAddress
  void $ liftMaybe (error "can't get reward address") (mRewardAddress >>= head)
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

contract :: Contract () Unit
contract = do
  noSignDataContract
  dataBytes <- liftContractAffM
    ("can't convert : " <> msg <> " to RawBytes")
    (pure mDataBytes)
  mRewardAddress <- performAndLog "getRewardAddresses" getRewardAddresses
  rewardAddr <- liftMaybe (error "can't get reward address")
    (mRewardAddress >>= head)
  mChangeAddress <- performAndLog "getChangeAddress" getChangeAddress
  changeAddress <- liftMaybe (error "can't get change address") mChangeAddress
  _ <- performAndLog "signData changeAddress" $ signData changeAddress dataBytes
  void $ performAndLog "signData rewardAddress" $ signData rewardAddr dataBytes
  where
  msg = "hello world!"
  mDataBytes = rawBytesFromAscii msg

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
