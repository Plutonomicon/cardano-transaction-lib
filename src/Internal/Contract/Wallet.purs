module Ctl.Internal.Contract.Wallet where

import Prelude

import Control.Monad.Reader (withReaderT)
import Control.Monad.Reader.Trans (ReaderT, asks)
import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import Ctl.Internal.Cardano.Types.Value (Value)
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.Helpers (liftM, liftedM)
import Ctl.Internal.Helpers as Helpers
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
import Ctl.Internal.Types.UsedTxOuts (UsedTxOuts, isTxOutRefUsed)
import Ctl.Internal.Wallet
  ( Cip30Connection
  , Cip30Wallet
  , Wallet(KeyWallet, Lode, Flint, Gero, Nami, Eternl)
  )
import Ctl.Internal.Wallet
  ( getChangeAddress
  , getRewardAddresses
  , getUnusedAddresses
  , getWalletAddresses
  , signData
  ) as Aff
import Ctl.Internal.Wallet.Cip30 (DataSignature)
import Data.Array (catMaybes, head)
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (fold)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for, for_, traverse)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)

getUnusedAddresses :: Contract (Array Address)
getUnusedAddresses = withWalletAff Aff.getUnusedAddresses

getChangeAddress :: Contract (Maybe Address)
getChangeAddress = withWalletAff Aff.getChangeAddress

getRewardAddresses :: Contract (Array Address)
getRewardAddresses = withWalletAff Aff.getRewardAddresses

getWalletAddresses :: Contract (Array Address)
getWalletAddresses = withWalletAff Aff.getWalletAddresses

signData :: Address -> RawBytes -> Contract (Maybe DataSignature)
signData address payload = withWalletAff (Aff.signData address payload)

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

withWalletAff
  :: forall (a :: Type). (Wallet -> Aff a) -> Contract a
withWalletAff act = withWallet (liftAff <<< act)

withWallet
  :: forall (a :: Type). (Wallet -> Contract a) -> Contract a
withWallet act = do
  wallet <- liftedM (error "No wallet set") $ asks _.wallet
  act wallet

callCip30Wallet
  :: forall (a :: Type)
   . Cip30Wallet
  -> (Cip30Wallet -> (Cip30Connection -> Aff a))
  -> Aff a
callCip30Wallet wallet act = act wallet wallet.connection

-- TODO Move
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
        let addr = (unwrap kw).address
        utxos <- (liftAff $ queryHandle.utxosAt addr)
          <#> hush >>> fromMaybe Map.empty
          >>= filterLockedUtxos
        pparams <- asks $ _.ledgerConstants >>> _.pparams <#> unwrap
        let
          coinsPerUtxoUnit = pparams.coinsPerUtxoUnit
          maxCollateralInputs = UInt.toInt $
            pparams.maxCollateralInputs
        liftEffect $ (unwrap kw).selectCollateral coinsPerUtxoUnit
          maxCollateralInputs
          utxos
  for_ mbCollateralUTxOs \collateralUTxOs -> do
    pparams <- asks $ _.ledgerConstants >>> _.pparams
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

getWalletBalance
  :: Contract (Maybe Value)
getWalletBalance = do
  queryHandle <- getQueryHandle
  asks _.wallet >>= map join <<< traverse case _ of
    Nami wallet -> liftAff $ wallet.getBalance wallet.connection
    Gero wallet -> liftAff $ wallet.getBalance wallet.connection
    Eternl wallet -> liftAff $ wallet.getBalance wallet.connection
    Flint wallet -> liftAff $ wallet.getBalance wallet.connection
    Lode wallet -> liftAff $ wallet.getBalance wallet.connection
    KeyWallet _ -> do
      -- Implement via `utxosAt`
      addresses <- getWalletAddresses
      fold <$> for addresses \address -> do
        liftAff $ queryHandle.utxosAt address <#> hush >>> map
          -- Combine `Value`s
          (fold <<< map _.amount <<< map unwrap <<< Map.values)

getWalletUtxos :: Contract (Maybe UtxoMap)
getWalletUtxos = do
  queryHandle <- getQueryHandle
  asks _.wallet >>= map join <<< traverse case _ of
    Nami wallet -> liftAff $ wallet.getUtxos wallet.connection <#> map toUtxoMap
    Gero wallet -> liftAff $ wallet.getUtxos wallet.connection <#> map toUtxoMap
    Flint wallet -> liftAff $ wallet.getUtxos wallet.connection <#> map
      toUtxoMap
    Eternl wallet -> liftAff $ wallet.getUtxos wallet.connection <#> map
      toUtxoMap
    Lode wallet -> liftAff $ wallet.getUtxos wallet.connection <#> map toUtxoMap
    KeyWallet _ -> do
      mbAddress <- getWalletAddresses <#> head
      map join $ for mbAddress $ map hush <<< liftAff <<< queryHandle.utxosAt
  where
  toUtxoMap :: Array TransactionUnspentOutput -> UtxoMap
  toUtxoMap = Map.fromFoldable <<< map
    (unwrap >>> \({ input, output }) -> input /\ output)
