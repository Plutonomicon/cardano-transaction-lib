module Ctl.Internal.Wallet
  ( module KeyWallet
  , module Cip30Wallet
  , Wallet(Gero, Nami, Flint, Lode, Eternl, KeyWallet)
  , WalletExtension
      ( ExtensionNami
      , ExtensionLode
      , ExtensionGero
      , ExtensionFlint
      , ExtensionEternl
      , ExtensionKeyWallet
      )
  , isEternlAvailable
  , isGeroAvailable
  , isNamiAvailable
  , isFlintAvailable
  , isLodeAvailable
  , isWalletAvailable
  , mkEternlWalletAff
  , mkNamiWalletAff
  , mkGeroWalletAff
  , mkFlintWalletAff
  , mkLodeWalletAff
  , mkKeyWallet
  , cip30Wallet
  , dummySign
  , isEnabled
  , walletToWalletExtension
  , apiVersion
  , name
  , icon
  ) where

import Prelude

import Contract.Prelude (fromMaybe)
import Control.Monad.Error.Class (catchError, liftMaybe, throwError)
import Control.Promise (Promise, toAffE)
import Ctl.Internal.Cardano.Types.Transaction
  ( Ed25519Signature(Ed25519Signature)
  , PublicKey(PublicKey)
  , Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
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
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (over, wrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, delay, error)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Prim.TypeError (class Warn, Text)

data Wallet
  = Nami Cip30Wallet
  | Gero Cip30Wallet
  | Flint Cip30Wallet
  | Eternl Cip30Wallet
  | Lode Cip30Wallet
  | KeyWallet KeyWallet

data WalletExtension
  = ExtensionNami
  | ExtensionGero
  | ExtensionFlint
  | ExtensionEternl
  | ExtensionLode
  | ExtensionKeyWallet

mkKeyWallet :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Wallet
mkKeyWallet payKey mbStakeKey = KeyWallet $ privateKeysToKeyWallet payKey
  mbStakeKey

foreign import _enableWallet :: String -> Effect (Promise Cip30Connection)
foreign import _isWalletAvailable :: String -> Effect Boolean
foreign import _isEnabled :: String -> Effect (Promise Boolean)
foreign import _apiVersion :: String -> Effect String
foreign import _name :: String -> Effect String
foreign import _icon :: String -> Effect String

mkWalletAff :: WalletExtension -> Aff Wallet
mkWalletAff walletExtension =
  case walletExtension of
    ExtensionNami -> Nami <$> mkCip30WalletAff "Nami" (_enableWallet walletName)
    ExtensionGero -> Gero <$> mkCip30WalletAff "Gero" (_enableWallet walletName)
    ExtensionEternl -> Eternl <$> mkCip30WalletAff "Eternl"
      (_enableWallet walletName)
    ExtensionFlint -> Flint <$> mkCip30WalletAff "Flint"
      (_enableWallet walletName)
    ExtensionLode -> _mkLodeWalletAff
    ExtensionKeyWallet -> liftEffect $ throw
      "Can't use makeWalletAff with a KeyWallet"
  where
  walletName = fromMaybe "" $ walletExtensionToName walletExtension

isNamiAvailable
  :: Warn (Text "Deprecated, please use `isWalletAvailable`")
  => Effect Boolean
isNamiAvailable = isWalletAvailable ExtensionNami

mkNamiWalletAff
  :: Warn (Text "Deprecated, please use `mkWalletAff`")
  => Aff Wallet
mkNamiWalletAff = mkWalletAff ExtensionNami

isGeroAvailable
  :: Warn (Text "Deprecated, please use `isWalletAvailable`")
  => Effect Boolean
isGeroAvailable = isWalletAvailable ExtensionGero

mkGeroWalletAff
  :: Warn (Text "Deprecated, please use `mkWalletAff`")
  => Aff Wallet
mkGeroWalletAff = mkWalletAff ExtensionGero

isFlintAvailable
  :: Warn (Text "Deprecated, please use `isWalletAvailable`")
  => Effect Boolean
isFlintAvailable = isWalletAvailable ExtensionFlint

mkFlintWalletAff
  :: Warn (Text "Deprecated, please use `mkWalletAff`")
  => Aff Wallet
mkFlintWalletAff = mkWalletAff ExtensionFlint

isLodeAvailable
  :: Warn (Text "Deprecated, please use `isWalletAvailable`")
  => Effect Boolean
isLodeAvailable = isWalletAvailable ExtensionLode

mkLodeWalletAff
  :: Warn (Text "Deprecated, please use `mkWalletAff`")
  => Aff Wallet
mkLodeWalletAff = _mkLodeWalletAff

-- Lode does not inject on page load, so this function retries up to set
-- number of times, for Lode to be available.
_mkLodeWalletAff :: Aff Wallet
_mkLodeWalletAff = do
  retryNWithIntervalUntil (fromInt' 10) (toNumber 100)
    $ liftEffect (isWalletAvailable ExtensionLode)
  catchError
    (Lode <$> mkCip30WalletAff "Lode" (_enableWallet "LodeWallet"))
    ( \e -> throwError <<< error $ (show e) <>
        " Note: LodeWallet is injected asynchronously and may be unreliable."
    )
  where
  retryNWithIntervalUntil n ms mBool =
    if n == zero then pure unit
    else mBool >>=
      if _ then pure unit
      else delay (wrap ms) *> retryNWithIntervalUntil (n `minus` one) ms mBool

isEternlAvailable :: Effect Boolean
isEternlAvailable = isWalletAvailable ExtensionEternl

mkEternlWalletAff :: Aff Wallet
mkEternlWalletAff = mkWalletAff ExtensionEternl

isWalletAvailable :: WalletExtension -> Effect Boolean
isWalletAvailable swallet =
  case walletExtensionToName swallet of
    Just walletName -> _isWalletAvailable walletName
    Nothing -> throw "Not implemented yet"

cip30Wallet :: Wallet -> Maybe Cip30Wallet
cip30Wallet = case _ of
  Nami c30 -> Just c30
  Gero c30 -> Just c30
  Flint c30 -> Just c30
  Eternl c30 -> Just c30
  Lode c30 -> Just c30
  KeyWallet _ -> Nothing

walletExtensionToName :: WalletExtension -> Maybe String
walletExtensionToName = case _ of
  ExtensionNami -> Just "nami"
  ExtensionGero -> Just "gerowallet"
  ExtensionFlint -> Just "flint"
  ExtensionEternl -> Just "eternl"
  ExtensionLode -> Just "LodeWallet"
  ExtensionKeyWallet -> Nothing

walletToWalletExtension :: Wallet -> WalletExtension
walletToWalletExtension = case _ of
  Nami _ -> ExtensionNami
  Gero _ -> ExtensionGero
  Flint _ -> ExtensionFlint
  Eternl _ -> ExtensionEternl
  Lode _ -> ExtensionLode
  KeyWallet _ -> ExtensionKeyWallet

isEnabled :: WalletExtension -> Aff Boolean
isEnabled wallet =
  case walletExtensionToName wallet of
    Just walletName -> toAffE $ _isEnabled walletName
    Nothing -> pure true

apiVersion :: WalletExtension -> Aff String
apiVersion wallet = do
  walletName <- liftMaybe
    (error "Can't get the name of the Wallet in apiVersion call")
    (walletExtensionToName wallet)
  liftEffect $ _apiVersion walletName

name :: WalletExtension -> Aff String
name wallet = do
  walletName <- liftMaybe
    (error "Can't get the name of the Wallet in `name` call")
    (walletExtensionToName wallet)
  liftEffect $ _name walletName

icon :: WalletExtension -> Aff String
icon wallet = do
  walletName <- liftMaybe
    (error "Can't get the name of the Wallet in `icon` call")
    (walletExtensionToName wallet)
  liftEffect $ _icon walletName

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
        ( PublicKey
            "ed25519_pk1eamrnx3pph58yr5l4z2wghjpu2dt2f0rp0zq9qquqa39p52ct0xsudjp4e"
        )
        /\ Ed25519Signature
          "ed25519_sig1ynufn5umzl746ekpjtzt2rf58ep0wg6mxpgyezh8vx0e8jpgm3kuu3tgm453wlz4rq5yjtth0fnj0ltxctaue0dgc2hwmysr9jvhjzswt86uk"
    )
