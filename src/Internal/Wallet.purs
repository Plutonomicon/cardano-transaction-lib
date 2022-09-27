module Ctl.Internal.Wallet
  ( module KeyWallet
  , module Cip30Wallet
  , Wallet(Gero, Nami, Flint, Lode, Eternl, KeyWallet)
  , isEternlAvailable
  , isGeroAvailable
  , isNamiAvailable
  , isFlintAvailable
  , isLodeAvailable
  , mkEternlWalletAff
  , mkNamiWalletAff
  , mkGeroWalletAff
  , mkFlintWalletAff
  , mkLodeWalletAff
  , mkKeyWallet
  , cip30Wallet
  , dummySign
  ) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Promise (Promise)
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  , mkEd25519Signature
  , mkPubKey
  )
import Ctl.Internal.Helpers ((<<>>))
import Ctl.Internal.Types.Natural (fromInt', minus)
import Ctl.Internal.Wallet.Cip30 (Cip30Connection, Cip30Wallet) as Cip30Wallet
import Ctl.Internal.Wallet.Cip30
  ( Cip30Connection
  , Cip30Wallet
  , mkCip30WalletAff
  )
import Ctl.Internal.Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )
import Ctl.Internal.Wallet.Key (KeyWallet, privateKeysToKeyWallet) as KeyWallet
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (over, wrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, delay, error)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)

data Wallet
  = Nami Cip30Wallet
  | Gero Cip30Wallet
  | Flint Cip30Wallet
  | Eternl Cip30Wallet
  | Lode Cip30Wallet
  | KeyWallet KeyWallet

mkKeyWallet :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Wallet
mkKeyWallet payKey mbStakeKey = KeyWallet $ privateKeysToKeyWallet payKey
  mbStakeKey

foreign import _isNamiAvailable :: Effect Boolean

isNamiAvailable :: Effect Boolean
isNamiAvailable = _isNamiAvailable

foreign import _enableNami :: Effect (Promise Cip30Connection)

mkNamiWalletAff :: Aff Wallet
mkNamiWalletAff = Nami <$> mkCip30WalletAff "Nami" _enableNami

foreign import _isGeroAvailable :: Effect Boolean

isGeroAvailable :: Effect Boolean
isGeroAvailable = _isGeroAvailable

foreign import _enableGero :: Effect (Promise Cip30Connection)

mkGeroWalletAff :: Aff Wallet
mkGeroWalletAff = Gero <$> mkCip30WalletAff "Gero" _enableGero

foreign import _isFlintAvailable :: Effect Boolean

isFlintAvailable :: Effect Boolean
isFlintAvailable = _isFlintAvailable

foreign import _enableFlint :: Effect (Promise Cip30Connection)

mkFlintWalletAff :: Aff Wallet
mkFlintWalletAff = Flint <$> mkCip30WalletAff "Flint" _enableFlint

foreign import _isLodeAvailable :: Effect Boolean

isLodeAvailable :: Effect Boolean
isLodeAvailable = _isLodeAvailable

foreign import _enableLode :: Effect (Promise Cip30Connection)

isEternlAvailable :: Effect Boolean
isEternlAvailable = _isEternlAvailable

foreign import _isEternlAvailable :: Effect Boolean

mkEternlWalletAff :: Aff Wallet
mkEternlWalletAff = Eternl <$> mkCip30WalletAff "Eternl" _enableEternl

foreign import _enableEternl :: Effect (Promise Cip30Connection)

-- Lode does not inject on page load, so this function retries up to set
-- number of times, for Lode to be available.
mkLodeWalletAff :: Aff Wallet
mkLodeWalletAff = do
  retryNWithIntervalUntil (fromInt' 10) (toNumber 100)
    $ liftEffect isLodeAvailable

  catchError
    (Lode <$> mkCip30WalletAff "Lode" _enableLode)
    ( \e -> throwError <<< error $ (show e) <>
        " Note: LodeWallet is injected asynchronously and may be unreliable."
    )

  where
  retryNWithIntervalUntil n ms mBool =
    if n == zero then pure unit
    else mBool >>=
      if _ then pure unit
      else delay (wrap ms) *> retryNWithIntervalUntil (n `minus` one) ms mBool

cip30Wallet :: Wallet -> Maybe Cip30Wallet
cip30Wallet = case _ of
  Nami c30 -> Just c30
  Gero c30 -> Just c30
  Flint c30 -> Just c30
  Eternl c30 -> Just c30
  Lode c30 -> Just c30
  KeyWallet _ -> Nothing

-- Attach a dummy vkey witness to a transaction. Helpful for when we need to
-- know the number of witnesses (e.g. fee calculation) but the wallet hasn't
-- signed (or cannot sign) yet
dummySign :: Transaction -> Transaction
dummySign tx@(Transaction { witnessSet: tws@(TransactionWitnessSet ws) }) =
  over Transaction _
    { witnessSet = over TransactionWitnessSet
        _
          { vkeys = ws.vkeys <<>> Just [ vk ]
          }
        tws
    }
    $ tx
  where
  vk :: Vkeywitness
  vk = Vkeywitness
    ( Vkey
        -- This should not fail assuming the hardcoded bech32 key is valid.
        ( unsafePartial $ fromJust $ mkPubKey
            "ed25519_pk1eamrnx3pph58yr5l4z2wghjpu2dt2f0rp0zq9qquqa39p52ct0xsudjp4e"
        )
        /\
          ( unsafePartial $ fromJust $ mkEd25519Signature
              "ed25519_sig1ynufn5umzl746ekpjtzt2rf58ep0wg6mxpgyezh8vx0e8jpgm3kuu3tgm453wlz4rq5yjtth0fnj0ltxctaue0dgc2hwmysr9jvhjzswt86uk"
          )
    )
