module Ctl.Internal.Contract.Wallet
  ( getUnusedAddresses
  , getChangeAddress
  , getRewardAddresses
  , getWalletAddresses
  , signData
  , getWallet
  , ownPubKeyHashes
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  , withWallet
  , getWalletCollateral
  , getWalletBalance
  , getWalletUtxos
  ) where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Control.Parallel (parTraverse)
import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import Ctl.Internal.Cardano.Types.Value (Value, valueToCoin)
import Ctl.Internal.Cardano.Types.Value (geq, lovelaceValueOf) as Value
import Ctl.Internal.Contract (getProtocolParameters)
import Ctl.Internal.Contract.Monad (Contract, filterLockedUtxos, getQueryHandle)
import Ctl.Internal.Helpers (liftM, liftedM)
import Ctl.Internal.Serialization.Address
  ( Address
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
import Ctl.Internal.Wallet (Wallet, actionBasedOnWallet)
import Ctl.Internal.Wallet.Cip30 (DataSignature)
import Data.Array (cons, foldMap, foldr)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Either (hush)
import Data.Foldable (fold, foldl)
import Data.Function (on)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for_, traverse)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)

getUnusedAddresses :: Contract (Array Address)
getUnusedAddresses = fold <$> do
  withWallet $ actionBasedOnWallet _.getUnusedAddresses mempty

getChangeAddress :: Contract (Maybe Address)
getChangeAddress = withWallet do
  actionBasedOnWallet _.getChangeAddress
    \kw -> do
      networkId <- asks _.networkId
      pure $ pure $ (unwrap kw).address networkId

getRewardAddresses :: Contract (Array Address)
getRewardAddresses = fold <$> withWallet do
  actionBasedOnWallet _.getRewardAddresses
    \kw -> do
      networkId <- asks _.networkId
      pure $ pure $ pure $ (unwrap kw).address networkId

getWalletAddresses :: Contract (Array Address)
getWalletAddresses = fold <$> withWallet do
  actionBasedOnWallet _.getWalletAddresses
    ( \kw -> do
        networkId <- asks _.networkId
        pure $ pure $ Array.singleton $ (unwrap kw).address networkId
    )

signData :: Address -> RawBytes -> Contract (Maybe DataSignature)
signData address payload =
  withWallet $
    actionBasedOnWallet
      (\w conn -> w.signData conn address payload)
      \kw -> do
        networkId <- asks _.networkId
        liftAff $ pure <$> (unwrap kw).signData networkId payload

getWallet :: Contract (Maybe Wallet)
getWallet = asks _.wallet

ownPubKeyHashes :: Contract (Array PubKeyHash)
ownPubKeyHashes = do
  getWalletAddresses >>= traverse \address -> do
    paymentCred <-
      liftM
        (error $ "Unable to get payment credential from Address") $
        addressPaymentCred address
    -- scripts are impossible here, so `stakeCredentialToKeyHash` will never
    -- return `Nothing` (`catMaybes` is safe)
    liftM (error "Impossible happened: CIP-30 method returned script address")
      $ stakeCredentialToKeyHash paymentCred <#> wrap

-- | Gets all wallet `PaymentPubKeyHash`es via `getWalletAddresses`.
ownPaymentPubKeyHashes :: Contract (Array PaymentPubKeyHash)
ownPaymentPubKeyHashes = map wrap <$> ownPubKeyHashes

ownStakePubKeyHashes :: Contract (Array (Maybe StakePubKeyHash))
ownStakePubKeyHashes = do
  addresses <- getWalletAddresses
  pure $ addressToMStakePubKeyHash <$> addresses
  where

  addressToMStakePubKeyHash :: Address -> Maybe StakePubKeyHash
  addressToMStakePubKeyHash address = do
    baseAddress <- baseAddressFromAddress address
    wrap <<< wrap <$> stakeCredentialToKeyHash
      (baseAddressDelegationCred baseAddress)

withWallet
  :: forall (a :: Type). (Wallet -> Contract a) -> Contract a
withWallet act = do
  wallet <- liftedM (error "No wallet set") getWallet
  act wallet

getWalletCollateral :: Contract (Maybe (Array TransactionUnspentOutput))
getWalletCollateral = do
  { maxCollateralInputs, coinsPerUtxoUnit } <- unwrap <$> getProtocolParameters
  mbCollateralUTxOs <- getWallet >>= maybe (pure Nothing) do
    actionBasedOnWallet _.getCollateral \kw -> do
      queryHandle <- getQueryHandle
      networkId <- asks _.networkId
      let addr = (unwrap kw).address networkId
      utxos <- (liftAff $ queryHandle.utxosAt addr)
        <#> hush >>> fromMaybe Map.empty
        >>= filterLockedUtxos
      liftEffect $ (unwrap kw).selectCollateral coinsPerUtxoUnit
        (UInt.toInt maxCollateralInputs)
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

    -- used for sorting by ADA value in descending order
    compareNegatedAdaValues
      :: TransactionUnspentOutput -> TransactionUnspentOutput -> Ordering
    compareNegatedAdaValues =
      compare `on`
        ( unwrap >>> _.output >>> unwrap >>> _.amount >>> valueToCoin >>> unwrap
            >>> negate
        )

    consumeUntilEnough
      :: Array TransactionUnspentOutput
      -> TransactionUnspentOutput
      -> Array TransactionUnspentOutput
    consumeUntilEnough utxos utxo =
      if foldMap utxoValue utxos `Value.geq` targetCollateral then utxos
      else cons utxo utxos
    mbSufficientUtxos = mbCollateralUTxOs <#>
      foldl consumeUntilEnough [] <<< Array.sortBy compareNegatedAdaValues
  for_ mbSufficientUtxos \sufficientUtxos -> do
    let
      tooManyCollateralUTxOs =
        UInt.fromInt (Array.length sufficientUtxos) >
          maxCollateralInputs
    when tooManyCollateralUTxOs do
      liftEffect $ throw $ tooManyCollateralUTxOsError
        <> " Minimal set of UTxOs to cover the collateral are: "
        <> show sufficientUtxos
  pure mbSufficientUtxos
  where
  tooManyCollateralUTxOsError =
    "Wallet returned too many UTxOs as collateral. This is likely a bug in \
    \the wallet."

getWalletBalance
  :: Contract (Maybe Value)
getWalletBalance = do
  queryHandle <- getQueryHandle
  getWallet >>= map join <<< traverse do
    actionBasedOnWallet _.getBalance \_ -> do
      -- Implement via `utxosAt`
      addresses <- getWalletAddresses
      fold <$> flip parTraverse addresses \address -> do
        liftAff $ queryHandle.utxosAt address <#> hush >>> map
          -- Combine `Value`s
          (fold <<< map _.amount <<< map unwrap <<< Map.values)

getWalletUtxos :: Contract (Maybe UtxoMap)
getWalletUtxos = do
  queryHandle <- getQueryHandle
  getWallet >>= map join <<< traverse do
    actionBasedOnWallet
      (\w conn -> w.getUtxos conn <#> map toUtxoMap)
      \_ -> do
        addresses :: Array Address <- getWalletAddresses
        res :: Array (Maybe UtxoMap) <- flip parTraverse addresses $
          map hush <<< liftAff <<< queryHandle.utxosAt
        pure $ Just $ foldr Map.union Map.empty $ map (fromMaybe Map.empty) res
  where
  toUtxoMap :: Array TransactionUnspentOutput -> UtxoMap
  toUtxoMap = Map.fromFoldable <<< map
    (unwrap >>> \({ input, output }) -> input /\ output)
