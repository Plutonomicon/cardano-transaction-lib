module Ctl.Internal.Wallet
  ( Cip30Extensions
  , Wallet(KeyWallet, GenericCip30)
  , WalletExtension
  , mkKeyWallet
  , mkWalletAff
  , actionBasedOnWallet
  , isWalletAvailable
  ) where

import Prelude

import Cardano.Wallet.Cip30 (Api)
import Cardano.Wallet.Cip30 (enable) as Cip30
import Cardano.Wallet.Cip95 (enable) as Cip95
import Cardano.Wallet.Key
  ( KeyWallet
  , PrivateDrepKey
  , PrivatePaymentKey
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )
import Control.Monad.Error.Class (catchError, throwError)
import Ctl.Internal.Wallet.Cip30 (Cip30Wallet, mkCip30WalletAff)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console

foreign import isWalletAvailable :: String -> Effect Boolean

-- NOTE: this data type is defined like this on purpose, don't change it
-- to `(Cip30Wallet /\ WalletExtension)`. The motivation is to make it simpler
-- to special-case each wallet in the future, if needed.
data Wallet
  = GenericCip30 Cip30Wallet
  | KeyWallet KeyWallet

type WalletExtension =
  { name :: String
  , exts :: Cip30Extensions
  }

type Cip30Extensions =
  { cip95 :: Boolean
  }

mkKeyWallet
  :: PrivatePaymentKey
  -> Maybe PrivateStakeKey
  -> Maybe PrivateDrepKey
  -> Wallet
mkKeyWallet payKey mbStakeKey mbDrepKey =
  KeyWallet $ privateKeysToKeyWallet payKey mbStakeKey mbDrepKey

mkWalletAff :: WalletExtension -> Aff Wallet
mkWalletAff walletExtension = do
  retryNWithIntervalUntil 300 (toNumber 100) $
    liftEffect (isWalletAvailable walletExtension.name)
  GenericCip30 <$> do
    mkCip30WalletAff =<< do
      enableWallet walletExtension `catchError`
        \err -> do
          liftEffect $ Console.error $ "Wallet extension "
            <> walletExtension.name
            <> " is not available!"
          throwError err
  where
  retryNWithIntervalUntil n ms mBool =
    if n == zero then pure unit
    else mBool >>=
      if _ then pure unit
      else delay (wrap ms) *> retryNWithIntervalUntil (n - 1) ms mBool

enableWallet :: WalletExtension -> Aff Api
enableWallet { name, exts: { cip95 } }
  | cip95 = Cip95.enable name
  | otherwise = Cip30.enable name mempty

actionBasedOnWallet
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadAff m
  => (Cip30Wallet -> Aff a)
  -> (KeyWallet -> m a)
  -> Wallet
  -> m a
actionBasedOnWallet walletAction keyWalletAction =
  case _ of
    GenericCip30 wallet -> liftAff $ walletAction wallet
    KeyWallet kw -> keyWalletAction kw
