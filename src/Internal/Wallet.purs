module Ctl.Internal.Wallet
  ( module KeyWallet
  , module Cip30Wallet
  , Wallet(Gero, Nami, Flint, Lode, Eternl, KeyWallet)
  , WalletExtension
      ( NamiWallet
      , LodeWallet
      , GeroWallet
      , FlintWallet
      , EternlWallet
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
  , mkWalletAff
  , cip30Wallet
  , dummySign
  , isEnabled
  , walletToWalletExtension
  , apiVersion
  , name
  , icon
  , getNetworkId
  , getUnusedAddresses
  , getChangeAddress
  , getRewardAddresses
  , getWalletAddresses
  , actionBasedOnWalletAff
  , signData
  , ownPubKeyHashes
  , ownPaymentPubKeyHashes
  , ownStakePubKeysHashes
  ) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Promise (Promise, toAffE)
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  , mkEd25519Signature
  , mkPublicKey
  )
import Ctl.Internal.Helpers ((<<>>))
import Ctl.Internal.Types.Natural (fromInt', minus)
import Ctl.Internal.Wallet.Cip30 (Cip30Connection, Cip30Wallet) as Cip30Wallet
import Ctl.Internal.Wallet.Cip30
  ( Cip30Connection
  , Cip30Wallet
  , DataSignature
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
import Data.Newtype (over, wrap, unwrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, delay, error)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Prim.TypeError (class Warn, Text)
import Ctl.Internal.Helpers as Helpers
import Data.Array as Array
import Data.Foldable (fold)
import Data.Traversable (traverse)
import Effect.Exception (throw)
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId(TestnetId, MainnetId)
  , addressPaymentCred
  , baseAddressDelegationCred
  , baseAddressFromAddress
  , stakeCredentialToKeyHash
  )
import Ctl.Internal.Types.PubKeyHash
  ( PaymentPubKeyHash
  , PubKeyHash
  , StakePubKeyHash
  )
import Ctl.Internal.Types.RawBytes (RawBytes)

data Wallet
  = Nami Cip30Wallet
  | Gero Cip30Wallet
  | Flint Cip30Wallet
  | Eternl Cip30Wallet
  | Lode Cip30Wallet
  | KeyWallet KeyWallet

data WalletExtension
  = NamiWallet
  | GeroWallet
  | FlintWallet
  | EternlWallet
  | LodeWallet

mkKeyWallet :: NetworkId -> PrivatePaymentKey -> Maybe PrivateStakeKey -> Wallet
mkKeyWallet network payKey mbStakeKey = KeyWallet $ privateKeysToKeyWallet network payKey mbStakeKey

foreign import _enableWallet :: String -> Effect (Promise Cip30Connection)
foreign import _isWalletAvailable :: String -> Effect Boolean
foreign import _isEnabled :: String -> Effect (Promise Boolean)
foreign import _apiVersion :: String -> Effect String
foreign import _name :: String -> Effect String
foreign import _icon :: String -> Effect String

mkWalletAff :: WalletExtension -> Aff Wallet
mkWalletAff walletExtension =
  case walletExtension of
    NamiWallet -> Nami <$> mkCip30WalletAff "Nami" (_enableWallet walletName)
    GeroWallet -> Gero <$> mkCip30WalletAff "Gero" (_enableWallet walletName)
    EternlWallet -> Eternl <$> mkCip30WalletAff "Eternl"
      (_enableWallet walletName)
    FlintWallet -> Flint <$> mkCip30WalletAff "Flint"
      (_enableWallet walletName)
    LodeWallet -> _mkLodeWalletAff
  where
  walletName = walletExtensionToName walletExtension

isNamiAvailable
  :: Warn
       ( Text
           "`isNamiAvailable` is deprecated, please use `isWalletAvailable NamiWallet`"
       )
  => Effect Boolean
isNamiAvailable = isWalletAvailable NamiWallet

mkNamiWalletAff
  :: Warn
       ( Text
           "`mkNamiWalletAff` is deprecated, please use `mkWalletAff NamiWallet`"
       )
  => Aff Wallet
mkNamiWalletAff = mkWalletAff NamiWallet

isGeroAvailable
  :: Warn
       ( Text
           "`isGeroAvailable` is deprecated, please use `isWalletAvailable GeroWallet`"
       )
  => Effect Boolean
isGeroAvailable = isWalletAvailable GeroWallet

mkGeroWalletAff
  :: Warn
       ( Text
           "`mkGeroWalletAff` is deprecated, please use `mkWalletAff GeroWallet`"
       )
  => Aff Wallet
mkGeroWalletAff = mkWalletAff GeroWallet

isFlintAvailable
  :: Warn
       ( Text
           "`isFlintAvailable` is deprecated, please use `isWalletAvailable FlintWallet`"
       )
  => Effect Boolean
isFlintAvailable = isWalletAvailable FlintWallet

mkFlintWalletAff
  :: Warn
       ( Text
           "`mkFlintWalletAff` is deprecated, please use `mkWalletAff FlintWallet`"
       )
  => Aff Wallet
mkFlintWalletAff = mkWalletAff FlintWallet

isLodeAvailable
  :: Warn
       ( Text
           "`isLodeAvailable` is deprecated, please use `isWalletAvailable LodeWallet`"
       )
  => Effect Boolean
isLodeAvailable = isWalletAvailable LodeWallet

mkLodeWalletAff
  :: Warn
       ( Text
           "`mkLodeWalletAff` is deprecated, please use `mkWalletAff LodeWallet`"
       )
  => Aff Wallet
mkLodeWalletAff = mkWalletAff LodeWallet

isEternlAvailable
  :: Warn
       ( Text
           "`isEternlAvailable` is deprecated, please use `isWalletAvailable EternlWallet`"
       )
  => Effect Boolean
isEternlAvailable = isWalletAvailable EternlWallet

mkEternlWalletAff
  :: Warn
       ( Text
           "`mkEternlWalletAff` is deprecated, please use `mkWalletAff EternlWallet`"
       )
  => Aff Wallet
mkEternlWalletAff = mkWalletAff EternlWallet

-- Lode does not inject on page load, so this function retries up to set
-- number of times, for Lode to be available.
_mkLodeWalletAff :: Aff Wallet
_mkLodeWalletAff = do
  retryNWithIntervalUntil (fromInt' 10) (toNumber 100)
    $ liftEffect (isWalletAvailable LodeWallet)
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

isWalletAvailable :: WalletExtension -> Effect Boolean
isWalletAvailable = _isWalletAvailable <<< walletExtensionToName

cip30Wallet :: Wallet -> Maybe Cip30Wallet
cip30Wallet = case _ of
  Nami c30 -> Just c30
  Gero c30 -> Just c30
  Flint c30 -> Just c30
  Eternl c30 -> Just c30
  Lode c30 -> Just c30
  KeyWallet _ -> Nothing

walletExtensionToName :: WalletExtension -> String
walletExtensionToName = case _ of
  NamiWallet -> "nami"
  GeroWallet -> "gerowallet"
  FlintWallet -> "flint"
  EternlWallet -> "eternl"
  LodeWallet -> "LodeWallet"

walletToWalletExtension :: Wallet -> Maybe WalletExtension
walletToWalletExtension = case _ of
  Nami _ -> Just NamiWallet
  Gero _ -> Just GeroWallet
  Flint _ -> Just FlintWallet
  Eternl _ -> Just EternlWallet
  Lode _ -> Just LodeWallet
  KeyWallet _ -> Nothing

isEnabled :: WalletExtension -> Aff Boolean
isEnabled =
  toAffE <<< _isEnabled <<< walletExtensionToName

apiVersion :: WalletExtension -> Aff String
apiVersion = liftEffect <<< _apiVersion <<< walletExtensionToName

name :: WalletExtension -> Aff String
name = liftEffect <<< _name <<< walletExtensionToName

icon :: WalletExtension -> Aff String
icon = liftEffect <<< _icon <<< walletExtensionToName

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
        ( unsafePartial $ fromJust $ mkPublicKey
            "ed25519_pk1eamrnx3pph58yr5l4z2wghjpu2dt2f0rp0zq9qquqa39p52ct0xsudjp4e"
        )
        /\
          ( unsafePartial $ fromJust $ mkEd25519Signature
              "ed25519_sig1ynufn5umzl746ekpjtzt2rf58ep0wg6mxpgyezh8vx0e8jpgm3kuu3tgm453wlz4rq5yjtth0fnj0ltxctaue0dgc2hwmysr9jvhjzswt86uk"
          )
    )

getNetworkId :: Wallet -> Aff NetworkId
getNetworkId wallet = do
  actionBasedOnWalletAff
    (\w -> intToNetworkId <=< _.getNetworkId w)
    (\kw -> pure (unwrap kw).networkId)
    wallet
  where
  intToNetworkId :: Int -> Aff NetworkId
  intToNetworkId = case _ of
    0 -> pure TestnetId
    1 -> pure MainnetId
    _ -> liftEffect $ throw "Unknown network id"

getUnusedAddresses :: Wallet -> Aff (Array Address)
getUnusedAddresses wallet = fold <$> do
  actionBasedOnWalletAff _.getUnusedAddresses
    (\_ -> pure $ pure [])
    wallet

getChangeAddress :: Wallet -> Aff (Maybe Address)
getChangeAddress wallet = do
  actionBasedOnWalletAff _.getChangeAddress (\kw -> pure $ pure (unwrap kw).address)
    wallet

getRewardAddresses :: Wallet -> Aff (Array Address)
getRewardAddresses wallet = fold <$> do
  actionBasedOnWalletAff _.getRewardAddresses
    (\kw -> pure $ pure $ pure (unwrap kw).address)
    wallet

getWalletAddresses :: Wallet -> Aff (Array Address)
getWalletAddresses wallet = fold <$> do
  actionBasedOnWalletAff _.getWalletAddresses
    (\kw -> pure $ pure $ Array.singleton (unwrap kw).address)
    wallet

signData :: Address -> RawBytes -> Wallet -> Aff (Maybe DataSignature)
signData address payload wallet = do
  actionBasedOnWalletAff
    (\w conn -> w.signData conn address payload)
    (\kw -> pure <$> (unwrap kw).signData payload)
    wallet

actionBasedOnWalletAff
  :: forall (a :: Type)
   . (Cip30Wallet -> Cip30Connection -> Aff a)
  -> (KeyWallet -> Aff a)
  -> Wallet
  -> Aff a
actionBasedOnWalletAff walletAction keyWalletAction =
  case _ of
    Eternl wallet -> callCip30Wallet wallet walletAction
    Nami wallet -> callCip30Wallet wallet walletAction
    Gero wallet -> callCip30Wallet wallet walletAction
    Flint wallet -> callCip30Wallet wallet walletAction
    Lode wallet -> callCip30Wallet wallet walletAction
    KeyWallet kw -> keyWalletAction kw

ownPubKeyHashes :: Wallet -> Aff (Array PubKeyHash)
ownPubKeyHashes wallet = Array.catMaybes <$> do
  getWalletAddresses wallet >>= traverse \address -> do
    paymentCred <-
      Helpers.liftM
        ( error $
            "Unable to get payment credential from Address"
        ) $
        addressPaymentCred address
    pure $ stakeCredentialToKeyHash paymentCred <#> wrap

ownPaymentPubKeyHashes :: Wallet -> Aff (Array PaymentPubKeyHash)
ownPaymentPubKeyHashes wallet = map wrap <$> ownPubKeyHashes wallet

ownStakePubKeysHashes :: Wallet -> Aff (Array (Maybe StakePubKeyHash))
ownStakePubKeysHashes wallet = do
  addresses <- getWalletAddresses wallet
  pure $ addressToMStakePubKeyHash <$> addresses
  where

  addressToMStakePubKeyHash :: Address -> Maybe StakePubKeyHash
  addressToMStakePubKeyHash address = do
    baseAddress <- baseAddressFromAddress address
    wrap <<< wrap <$> stakeCredentialToKeyHash
      (baseAddressDelegationCred baseAddress)

callCip30Wallet
  :: forall (a :: Type)
   . Cip30Wallet
  -> (Cip30Wallet -> (Cip30Connection -> Aff a))
  -> Aff a
callCip30Wallet wallet act = act wallet wallet.connection
