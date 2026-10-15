-- | This module demonstrates the use of the CIP-30 functions
-- | using an external wallet. Uses `purescript-cip30`
module Ctl.Examples.Cip30
  ( main
  , example
  , contract
  ) where

import Contract.Prelude

import Cardano.Wallet.Cip30
  ( getApiVersion
  , getAvailableWallets
  , getIcon
  , getName
  )
import Cardano.Wallet.Cip30.TypeSafe as Cip30
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractAffM, runContract)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Wallet
  ( getChangeAddress
  , getRewardAddresses
  , getUnusedAddresses
  , signData
  )
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Effect.Exception (error)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  traverse_ nonConfigFunctions =<< liftEffect getAvailableWallets
  runContract cfg contract

nonConfigFunctions :: String -> Aff Unit
nonConfigFunctions extensionWallet = do
  log "Functions that don't depend on `Contract`"
  performAndLog "isEnabled" $ Cip30.isEnabled
  performAndLog "apiVersion" $ liftEffect <<< getApiVersion
  performAndLog "name" $ liftEffect <<< getName
  performAndLog "icon" $ liftEffect <<< getIcon
  where
  performAndLog
    :: forall (a :: Type)
     . Show a
    => String
    -> (String -> Aff a)
    -> Aff Unit
  performAndLog msg f = do
    result <- f extensionWallet
    log $ msg <> ":" <> (show result)

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.Cip30"
  logInfo' "Funtions that depend on `Contract`"
  _ <- performAndLog "getUnusedAddresses" getUnusedAddresses
  dataBytes <- liftContractAffM
    ("can't convert : " <> msg <> " to RawBytes")
    (pure $ wrap <$> mDataBytes)
  mRewardAddress <- performAndLog "getRewardAddresses" getRewardAddresses
  rewardAddr <- liftMaybe (error "can't get reward address")
    $ head mRewardAddress
  changeAddress <- performAndLog "getChangeAddress" getChangeAddress
  _ <- performAndLog "signData changeAddress" $ signData changeAddress dataBytes
  void $ performAndLog "signData rewardAddress" $ signData rewardAddr dataBytes
  where
  msg = "hello world!"
  mDataBytes = byteArrayFromAscii msg

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
