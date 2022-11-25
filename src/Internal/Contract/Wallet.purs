module Ctl.Internal.Contract.Wallet where

import Prelude

import Ctl.Internal.Contract.Monad (Contract)

import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId
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
import Ctl.Internal.Wallet
  ( Cip30Connection
  , Cip30Wallet
  , KeyWallet
  , Wallet(KeyWallet, Lode, Flint, Gero, Nami, Eternl)
  )
import Ctl.Internal.Wallet.Cip30 (DataSignature)
import Data.Array (catMaybes)
import Data.Foldable (fold)
import Data.Newtype (unwrap, wrap)
import Effect.Aff
  ( Aff
  )
import Effect.Aff.Class (liftAff)
import Effect.Exception (error, throw)
import Control.Monad.Reader (withReaderT)
import Control.Monad.Reader.Trans (ReaderT, asks)
import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import Ctl.Internal.Helpers as Helpers
import Ctl.Internal.Types.UsedTxOuts (UsedTxOuts, isTxOutRefUsed)
import Data.Array as Array
import Data.Either (hush)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Traversable (for_, traverse)
import Data.UInt as UInt
import Effect.Class (liftEffect)

getUnusedAddresses :: Contract (Array Address)
getUnusedAddresses = fold <$> do
  actionBasedOnWallet _.getUnusedAddresses
    (\_ -> pure [])

getChangeAddress :: Contract (Maybe Address)
getChangeAddress = do
  networkId <- getNetworkId
  actionBasedOnWallet _.getChangeAddress (\kw -> (unwrap kw).address networkId)

getRewardAddresses :: Contract (Array Address)
getRewardAddresses = fold <$> do
  networkId <- getNetworkId
  actionBasedOnWallet _.getRewardAddresses
    (\kw -> Array.singleton <$> (unwrap kw).address networkId)

getWalletAddresses :: Contract (Array Address)
getWalletAddresses = fold <$> do
  networkId <- getNetworkId
  actionBasedOnWallet _.getWalletAddresses
    (\kw -> Array.singleton <$> (unwrap kw).address networkId)

actionBasedOnWallet
  :: forall (a :: Type)
   . (Cip30Wallet -> Cip30Connection -> Aff (Maybe a))
  -> (KeyWallet -> Aff a)
  -> Contract (Maybe a)
actionBasedOnWallet walletAction keyWalletAction =
  withMWalletAff case _ of
    Eternl wallet -> callCip30Wallet wallet walletAction
    Nami wallet -> callCip30Wallet wallet walletAction
    Gero wallet -> callCip30Wallet wallet walletAction
    Flint wallet -> callCip30Wallet wallet walletAction
    Lode wallet -> callCip30Wallet wallet walletAction
    KeyWallet kw -> pure <$> keyWalletAction kw

signData :: Address -> RawBytes -> Contract (Maybe DataSignature)
signData address payload = do
  networkId <- getNetworkId
  actionBasedOnWallet
    (\wallet conn -> wallet.signData conn address payload)
    (\kw -> (unwrap kw).signData networkId payload)

getWallet :: Contract (Maybe Wallet)
getWallet = asks (_.wallet)

getNetworkId :: Contract NetworkId
getNetworkId = asks _.networkId

ownPubKeyHashes :: Contract (Array PubKeyHash)
ownPubKeyHashes = catMaybes <$> do
  getWalletAddresses >>= traverse \address -> do
    paymentCred <-
      liftM
        ( error $
            "Unable to get payment credential from Address"
        ) $
        addressPaymentCred address
    pure $ stakeCredentialToKeyHash paymentCred <#> wrap

ownPaymentPubKeyHashes :: Contract (Array PaymentPubKeyHash)
ownPaymentPubKeyHashes = map wrap <$> ownPubKeyHashes

ownStakePubKeysHashes :: Contract (Array (Maybe StakePubKeyHash))
ownStakePubKeysHashes = do
  addresses <- getWalletAddresses
  pure $ addressToMStakePubKeyHash <$> addresses
  where

  addressToMStakePubKeyHash :: Address -> Maybe StakePubKeyHash
  addressToMStakePubKeyHash address = do
    baseAddress <- baseAddressFromAddress address
    wrap <<< wrap <$> stakeCredentialToKeyHash
      (baseAddressDelegationCred baseAddress)

withMWalletAff
  :: forall (a :: Type). (Wallet -> Aff (Maybe a)) -> Contract (Maybe a)
withMWalletAff act = withMWallet (liftAff <<< act)

withMWallet
  :: forall (a :: Type). (Wallet -> Contract (Maybe a)) -> Contract (Maybe a)
withMWallet act = asks _.wallet >>= maybe (pure Nothing)
  act

callCip30Wallet
  :: forall (a :: Type)
   . Cip30Wallet
  -> (Cip30Wallet -> (Cip30Connection -> Aff a))
  -> Aff a
callCip30Wallet wallet act = act wallet wallet.connection

filterLockedUtxos :: UtxoMap -> Contract UtxoMap
filterLockedUtxos utxos =
  withTxRefsCache $
    flip Helpers.filterMapWithKeyM utxos
      (\k _ -> not <$> isTxOutRefUsed (unwrap k))

withTxRefsCache
  :: forall (m :: Type -> Type) (a :: Type)
   . ReaderT UsedTxOuts Aff a
  -> Contract a
withTxRefsCache = wrap <<< withReaderT _.usedTxOuts

getWalletCollateral :: Contract (Maybe (Array TransactionUnspentOutput))
getWalletCollateral = do
  queryHandle <- getQueryHandle
  mbCollateralUTxOs <- asks (_.wallet) >>= maybe (pure Nothing)
    case _ of
      Nami wallet -> liftAff $ callCip30Wallet wallet _.getCollateral
      Gero wallet -> liftAff $ callCip30Wallet wallet _.getCollateral
      Flint wallet -> liftAff $ callCip30Wallet wallet _.getCollateral
      Lode wallet -> liftAff $ callCip30Wallet wallet _.getCollateral
      Eternl wallet -> liftAff $ callCip30Wallet wallet _.getCollateral
      KeyWallet kw -> do
        networkId <- getNetworkId
        addr <- liftAff $ (unwrap kw).address networkId
        utxos <- (liftAff $ queryHandle.utxosAt addr) <#> hush >>> fromMaybe Map.empty
          >>= filterLockedUtxos
        pparams <- asks $ _.pparams <#> unwrap
        let
          coinsPerUtxoUnit = pparams.coinsPerUtxoUnit
          maxCollateralInputs = UInt.toInt $
            pparams.maxCollateralInputs
        liftEffect $ (unwrap kw).selectCollateral coinsPerUtxoUnit
          maxCollateralInputs
          utxos
  for_ mbCollateralUTxOs \collateralUTxOs -> do
    pparams <- asks $ _.pparams
    let
      tooManyCollateralUTxOs =
        UInt.fromInt (Array.length collateralUTxOs) >
          (unwrap pparams).maxCollateralInputs
    when tooManyCollateralUTxOs do
      liftEffect $ throw tooManyCollateralUTxOsError
  pure mbCollateralUTxOs
  where
  tooManyCollateralUTxOsError =
    "Wallet returned too many UTxOs as collateral. This is likely a bug in \
    \the wallet."

