module Ctl.Internal.Wallet.Spec
  ( WalletSpec
      ( UseKeys
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToEternl
      , ConnectToLode
      , ConnectToNuFi
      )
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  , mkWalletBySpec
  ) where

import Prelude

import Ctl.Internal.Serialization.Address (NetworkId)
import Ctl.Internal.Wallet
  ( Wallet
  , WalletExtension
      ( NamiWallet
      , GeroWallet
      , FlintWallet
      , EternlWallet
      , LodeWallet
      , NuFiWallet
      )
  , mkKeyWallet
  , mkWalletAff
  )
import Ctl.Internal.Wallet.Key (PrivatePaymentKey, PrivateStakeKey)
import Ctl.Internal.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  )
import Data.Maybe (Maybe)
import Data.Traversable (for)
import Effect.Aff (Aff)
import Node.Path (FilePath)

data PrivatePaymentKeySource
  = PrivatePaymentKeyFile FilePath
  | PrivatePaymentKeyValue PrivatePaymentKey

data PrivateStakeKeySource
  = PrivateStakeKeyFile FilePath
  | PrivateStakeKeyValue PrivateStakeKey

-- | A data type to describe instructions on how to initialize a wallet.
data WalletSpec
  = UseKeys PrivatePaymentKeySource (Maybe PrivateStakeKeySource)
  | ConnectToNami
  | ConnectToGero
  | ConnectToFlint
  | ConnectToEternl
  | ConnectToLode
  | ConnectToNuFi

mkWalletBySpec :: NetworkId -> WalletSpec -> Aff Wallet
mkWalletBySpec networkId = case _ of
  UseKeys paymentKeySpec mbStakeKeySpec -> do
    privatePaymentKey <- case paymentKeySpec of
      PrivatePaymentKeyFile filePath ->
        privatePaymentKeyFromFile filePath
      PrivatePaymentKeyValue key -> pure key
    mbPrivateStakeKey <- for mbStakeKeySpec case _ of
      PrivateStakeKeyFile filePath -> privateStakeKeyFromFile filePath
      PrivateStakeKeyValue key -> pure key
    pure $ mkKeyWallet networkId privatePaymentKey mbPrivateStakeKey
  ConnectToNami -> mkWalletAff NamiWallet
  ConnectToGero -> mkWalletAff GeroWallet
  ConnectToFlint -> mkWalletAff FlintWallet
  ConnectToEternl -> mkWalletAff EternlWallet
  ConnectToLode -> mkWalletAff LodeWallet
  ConnectToNuFi -> mkWalletAff NuFiWallet
