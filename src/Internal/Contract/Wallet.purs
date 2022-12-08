module Ctl.Internal.Contract.Wallet
  ( getUnusedAddresses
  , getChangeAddress
  , getRewardAddresses
  , getWalletAddresses
  , signData
  , getWallet
  , getNetworkId
  , ownPubKeyHashes
  , ownPaymentPubKeyHashes
  , ownStakePubKeysHashes
  , withWalletAff
  , withWallet
  , getWalletCollateral
  , getWalletBalance
  , getWalletUtxos
  ) where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import Ctl.Internal.Cardano.Types.Value (Value)
import Ctl.Internal.Cardano.Types.Value (geq, lovelaceValueOf) as Value
import Ctl.Internal.Contract.Monad (Contract, filterLockedUtxos)
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.Helpers (liftM, liftedM)
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
  ( Wallet
  , actionBasedOnWallet
  )
import Ctl.Internal.Wallet
  ( getChangeAddress
  , getRewardAddresses
  , getUnusedAddresses
  , getWalletAddresses
  , signData
  ) as Aff
import Ctl.Internal.Wallet.Cip30 (DataSignature)
import Data.Array (catMaybes, cons, foldMap, head)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Either (hush)
import Data.Foldable (fold, foldl)
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

getWalletCollateral :: Contract (Maybe (Array TransactionUnspentOutput))
getWalletCollateral = do
  mbCollateralUTxOs <- asks (_.wallet) >>= maybe (pure Nothing) do
    actionBasedOnWallet _.getCollateral \kw -> do
      queryHandle <- getQueryHandle
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

  let
    {- This is a workaround for the case where Eternl wallet,
       in addition to designated collateral UTxO, returns all UTxO's with
       small enough Ada value that can be used as potential collateral, which
       in turn would result in those UTxO's being filtered out of the balancer's
       available set, and in certain circumstances fail unexpectedly with a
       `InsufficientTxInputs` error.
       The snippet (`sufficientUtxos`) below prevents this by taking the first
       N UTxO's returned by `getCollateral`, such that their total Ada value
       is greater than or equal to 5 Ada.
    -}
    targetCollateral = Value.lovelaceValueOf $ BigInt.fromInt 5_000_000
    utxoValue u = (unwrap (unwrap u).output).amount
    sufficientUtxos = mbCollateralUTxOs <#> \colUtxos ->
      foldl
        ( \us u ->
            if foldMap utxoValue us `Value.geq` targetCollateral then us
            else cons u us
        )
        []
        colUtxos

  for_ sufficientUtxos \collateralUTxOs -> do
    pparams <- asks $ _.ledgerConstants >>> _.pparams
    let
      tooManyCollateralUTxOs =
        UInt.fromInt (Array.length collateralUTxOs) >
          (unwrap pparams).maxCollateralInputs
    when tooManyCollateralUTxOs do
      liftEffect $ throw tooManyCollateralUTxOsError
  pure sufficientUtxos
  where
  tooManyCollateralUTxOsError =
    "Wallet returned too many UTxOs as collateral. This is likely a bug in \
    \the wallet."

getWalletBalance
  :: Contract (Maybe Value)
getWalletBalance = do
  queryHandle <- getQueryHandle
  asks _.wallet >>= map join <<< traverse do
    actionBasedOnWallet _.getBalance \_ -> do
      -- Implement via `utxosAt`
      addresses <- getWalletAddresses
      fold <$> for addresses \address -> do
        liftAff $ queryHandle.utxosAt address <#> hush >>> map
          -- Combine `Value`s
          (fold <<< map _.amount <<< map unwrap <<< Map.values)

getWalletUtxos :: Contract (Maybe UtxoMap)
getWalletUtxos = do
  queryHandle <- getQueryHandle
  asks _.wallet >>= map join <<< traverse do
    actionBasedOnWallet
      (\w conn -> w.getUtxos conn <#> map toUtxoMap)
      \_ -> do
        mbAddress <- getWalletAddresses <#> head
        map join $ for mbAddress $ map hush <<< liftAff <<< queryHandle.utxosAt
  where
  toUtxoMap :: Array TransactionUnspentOutput -> UtxoMap
  toUtxoMap = Map.fromFoldable <<< map
    (unwrap >>> \({ input, output }) -> input /\ output)
