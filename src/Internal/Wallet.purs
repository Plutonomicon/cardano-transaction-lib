module Ctl.Internal.Wallet
  ( Wallet(KeyWallet, GenericCip30)
  , WalletExtension
      ( NamiWallet
      , LodeWallet
      , GeroWallet
      , FlintWallet
      , EternlWallet
      , NuFiWallet
      , LaceWallet
      , GenericCip30Wallet
      )
  , mkKeyWallet
  , mkWalletAff
  , actionBasedOnWallet
  , isWalletAvailable
  ) where

import Prelude

import Cardano.Wallet.Cip30 as Cip30
import Control.Monad.Error.Class (catchError, throwError)
import Ctl.Internal.Wallet.Cip30 (Cip30Wallet, mkCip30WalletAff)
import Ctl.Internal.Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console

-- NOTE: this data type is defined like this on purpose, don't change it
-- to `(Cip30Wallet /\ WalletExtension)`. The motivation is to make it simpler
-- to special-case each wallet in the future, if needed.
data Wallet
  = GenericCip30 Cip30Wallet
  | KeyWallet KeyWallet

data WalletExtension
  = NamiWallet
  | GeroWallet
  | FlintWallet
  | EternlWallet
  | LodeWallet
  | LaceWallet
  | NuFiWallet
  | GenericCip30Wallet String

mkKeyWallet :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Wallet
mkKeyWallet payKey mbStakeKey = KeyWallet $ privateKeysToKeyWallet
  payKey
  mbStakeKey

foreign import _isWalletAvailable :: String -> Effect Boolean

mkWalletAff :: WalletExtension -> Aff Wallet
mkWalletAff walletExtension = do
  retryNWithIntervalUntil 300 (toNumber 100)
    $ liftEffect (isWalletAvailable walletExtension)
  GenericCip30 <$> do
    mkCip30WalletAff =<< do
      Cip30.enable (walletExtensionToName walletExtension) [] `catchError`
        \err -> do
          liftEffect $ Console.error $ "Wallet extension "
            <> walletExtensionToName walletExtension
            <> " is not available!"
          throwError err
  where
  retryNWithIntervalUntil n ms mBool =
    if n == zero then pure unit
    else mBool >>=
      if _ then pure unit
      else delay (wrap ms) *> retryNWithIntervalUntil (n - 1) ms mBool

isWalletAvailable :: WalletExtension -> Effect Boolean
isWalletAvailable = _isWalletAvailable <<< walletExtensionToName

walletExtensionToName :: WalletExtension -> String
walletExtensionToName = case _ of
  NamiWallet -> "nami"
  GeroWallet -> "gerowallet"
  FlintWallet -> "flint"
  EternlWallet -> "eternl"
  LodeWallet -> "LodeWallet"
  NuFiWallet -> "nufi"
  LaceWallet -> "lace"
  GenericCip30Wallet name' -> name'

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
