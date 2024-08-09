module Ctl.Internal.Contract.Wallet
  ( getUnusedAddresses
  , getChangeAddress
  , getRewardAddresses
  , getWalletAddresses
  , signData
  , getWallet
  , ownDrepPubKey
  , ownDrepPubKeyHash
  , ownPubKeyHashes
  , ownPaymentPubKeyHashes
  , ownRegisteredPubStakeKeys
  , ownStakePubKeyHashes
  , ownUnregisteredPubStakeKeys
  , withWallet
  , getWalletCollateral
  , getWalletBalance
  , getWalletUtxos
  ) where

import Prelude

import Cardano.Types (Ed25519KeyHash, RawBytes)
import Cardano.Types.Address
  ( Address(RewardAddress)
  , getPaymentCredential
  , getStakeCredential
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Credential as Credential
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Cardano.Types.PrivateKey (toPublicKey) as PrivateKey
import Cardano.Types.PublicKey (PublicKey)
import Cardano.Types.PublicKey (hash) as PublicKey
import Cardano.Types.StakePubKeyHash (StakePubKeyHash)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.UtxoMap (UtxoMap)
import Cardano.Types.Value (Value, valueToCoin)
import Cardano.Types.Value (geq, lovelaceValueOf, sum) as Value
import Cardano.Wallet.Key
  ( PrivateStakeKey(PrivateStakeKey)
  , getPrivateDrepKey
  , getPrivateStakeKey
  )
import Contract.Log (logWarn')
import Control.Monad.Reader.Trans (asks)
import Control.Parallel (parTraverse)
import Ctl.Internal.BalanceTx.Collateral.Select (minRequiredCollateral)
import Ctl.Internal.Contract (getProtocolParameters)
import Ctl.Internal.Contract.Monad (Contract, filterLockedUtxos, getQueryHandle)
import Ctl.Internal.Helpers (bugTrackerLink, liftM, liftedM)
import Ctl.Internal.Service.Error (pprintClientError)
import Ctl.Internal.Wallet (Wallet, actionBasedOnWallet)
import Ctl.Internal.Wallet.Cip30 (DataSignature)
import Data.Array (cons, foldr)
import Data.Array as Array
import Data.Either (Either(Left, Right), hush)
import Data.Foldable (foldl)
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
import Effect.Unsafe (unsafePerformEffect)

getUnusedAddresses :: Contract (Array Address)
getUnusedAddresses =
  withWallet $ actionBasedOnWallet _.getUnusedAddresses mempty

getChangeAddress :: Contract Address
getChangeAddress = withWallet do
  actionBasedOnWallet _.getChangeAddress
    \kw -> do
      networkId <- asks _.networkId
      addr <- liftAff $ (unwrap kw).address networkId
      pure addr

getRewardAddresses :: Contract (Array Address)
getRewardAddresses =
  withWallet $ actionBasedOnWallet _.getRewardAddresses
    \kw -> do
      networkId <- asks _.networkId
      mStakeCred <- liftAff $ getStakeCredential <$> (unwrap kw).address
        networkId
      pure $ maybe mempty
        ( Array.singleton <<< RewardAddress <<<
            { networkId, stakeCredential: _ }
        )
        mStakeCred

-- | Get all `Address`es of the browser wallet.
getWalletAddresses :: Contract (Array Address)
getWalletAddresses = withWallet do
  actionBasedOnWallet _.getUsedAddresses
    ( \kw -> do
        networkId <- asks _.networkId
        addr <- liftAff $ (unwrap kw).address networkId
        pure $ Array.singleton $ addr
    )

signData :: Address -> RawBytes -> Contract DataSignature
signData address payload =
  withWallet $
    actionBasedOnWallet
      (\w -> w.signData address payload)
      ( \kw -> do
          mDataSig <- liftAff $ (unwrap kw).signData address payload
          liftM
            ( error
                "signData via KeyWallet: Unable to sign data for the supplied address"
            )
            mDataSig
      )

getWallet :: Contract (Maybe Wallet)
getWallet = asks _.wallet

ownPubKeyHashes :: Contract (Array Ed25519KeyHash)
ownPubKeyHashes = do
  getWalletAddresses >>= traverse \address -> do
    paymentCred <-
      liftM
        (error $ "Unable to get payment credential from Address") $
        getPaymentCredential address
    -- scripts are impossible here, so `stakeCredentialToKeyHash` will never
    -- return `Nothing` (`catMaybes` is safe)
    liftM (error "Impossible happened: CIP-30 method returned script address")
      $ Credential.asPubKeyHash (unwrap paymentCred)

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
    cred <- unwrap <$> getStakeCredential address
    wrap <$> Credential.asPubKeyHash cred

withWallet
  :: forall (a :: Type). (Wallet -> Contract a) -> Contract a
withWallet act = do
  wallet <- liftedM (error "No wallet set") getWallet
  act wallet

getWalletCollateral :: Contract (Maybe (Array TransactionUnspentOutput))
getWalletCollateral = do
  { maxCollateralInputs, coinsPerUtxoByte } <- unwrap <$> getProtocolParameters
  mbCollateralUTxOs <- getWallet >>= maybe (pure Nothing) do
    actionBasedOnWallet _.getCollateral \kw -> do
      queryHandle <- getQueryHandle
      networkId <- asks _.networkId
      addr <- liftAff $ (unwrap kw).address networkId
      utxos <- (liftAff $ queryHandle.utxosAt addr)
        <#> hush >>> fromMaybe Map.empty
        >>= filterLockedUtxos
      mColl <- liftAff $ (unwrap kw).selectCollateral
        minRequiredCollateral
        coinsPerUtxoByte
        (UInt.toInt maxCollateralInputs)
        utxos
      pure mColl
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
    targetCollateral = Value.lovelaceValueOf $ BigNum.fromInt 5_000_000
    utxoValue u = (unwrap (unwrap u).output).amount

    -- used for sorting by ADA value in descending order
    compareNegatedAdaValues
      :: TransactionUnspentOutput -> TransactionUnspentOutput -> Ordering
    compareNegatedAdaValues =
      flip compare `on`
        (unwrap >>> _.output >>> unwrap >>> _.amount >>> valueToCoin >>> unwrap)

    consumeUntilEnough
      :: Array TransactionUnspentOutput
      -> TransactionUnspentOutput
      -> Array TransactionUnspentOutput
    consumeUntilEnough utxos utxo =
      case
        Value.geq <$> Value.sum (utxoValue <$> utxos) <*> Just targetCollateral
        of
        Just true -> utxos
        Just false -> cons utxo utxos
        _ -> unsafePerformEffect $ throw $
          "Overflow while calculating collateral. " <> bugTrackerLink
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
  :: Contract Value
getWalletBalance = do
  queryHandle <- getQueryHandle
  wallet <- getWallet >>= liftM (error "getWalletBalance: no wallet is active")
  let
    getKeyWalletBalance :: _ -> Contract Value
    getKeyWalletBalance = \_ -> do
      -- Implement via `utxosAt`
      addresses <- getWalletAddresses
      liftM
        ( error $
            "getWalletBalance: Unable to get payment credential from Address"
        )
        =<< Value.sum <$> flip parTraverse addresses \address -> do
          eiResponse <- liftAff $ queryHandle.utxosAt address
          case eiResponse of
            Left err -> liftEffect $ throw $
              "getWalletBalance: utxosAt call error: " <>
                pprintClientError err
            Right utxoMap -> do
              liftM
                ( error $
                    "getWalletBalance: Unable to get payment credential from Address"
                )
                $ Value.sum
                    ( map _.amount $ map unwrap $ Array.fromFoldable $
                        Map.values utxoMap
                    )
  actionBasedOnWallet _.getBalance getKeyWalletBalance wallet

getWalletUtxos :: Contract (Maybe UtxoMap)
getWalletUtxos = do
  queryHandle <- getQueryHandle
  wallet <- getWallet >>= liftM (error "getWalletUtxos: no wallet is active")
  let
    getKeyWalletUtxos = \_ -> do
      addresses :: Array Address <- getWalletAddresses
      utxoMaps <- flip parTraverse addresses \address -> do
        eiResponse <- liftAff $ queryHandle.utxosAt address
        case eiResponse of
          Left err ->
            liftEffect $ throw $
              "getWalletUtxos via KeyWallet: utxosAt call error: " <>
                pprintClientError err
          Right utxoMap ->
            pure utxoMap
      -- CIP-30 only returns `null` if the requested amount can't be returned
      -- but since we only call `getUtxos` *without* a requested amount, we use
      -- `Just`
      pure $ Just $ foldr Map.union Map.empty utxoMaps

  actionBasedOnWallet (\w -> w.getUtxos <#> map toUtxoMap) getKeyWalletUtxos
    wallet
  where
  toUtxoMap :: Array TransactionUnspentOutput -> UtxoMap
  toUtxoMap = Map.fromFoldable <<< map
    (unwrap >>> \({ input, output }) -> input /\ output)

ownDrepPubKey :: Contract PublicKey
ownDrepPubKey =
  withWallet do
    actionBasedOnWallet _.getPubDrepKey
      ( \kw -> do
          drepKey <- liftAff $ liftedM
            (error "ownDrepPubKey: Unable to get KeyWallet DRep key")
            (getPrivateDrepKey kw)
          pure $ PrivateKey.toPublicKey $ unwrap drepKey
      )

ownDrepPubKeyHash :: Contract Ed25519KeyHash
ownDrepPubKeyHash =
  withWallet do
    actionBasedOnWallet (map PublicKey.hash <<< _.getPubDrepKey)
      ( \kw -> do
          drepKey <- liftAff $ liftedM
            (error "ownDrepPubKeyHash: Unable to get KeyWallet DRep key")
            (getPrivateDrepKey kw)
          pure $ PublicKey.hash $ PrivateKey.toPublicKey $
            unwrap drepKey
      )

ownRegisteredPubStakeKeys :: Contract (Array PublicKey)
ownRegisteredPubStakeKeys =
  withWallet do
    actionBasedOnWallet _.getRegisteredPubStakeKeys
      ( \_kw -> do
          logWarn' $ kwStakeKeysRegStatusWarning "ownRegisteredPubStakeKeys"
          pure mempty
      )

ownUnregisteredPubStakeKeys :: Contract (Array PublicKey)
ownUnregisteredPubStakeKeys =
  withWallet do
    actionBasedOnWallet _.getUnregisteredPubStakeKeys
      ( \kw -> do
          logWarn' $ kwStakeKeysRegStatusWarning "ownUnregisteredPubStakeKeys"
          liftAff (getPrivateStakeKey kw) <#> case _ of
            Just (PrivateStakeKey stakeKey) ->
              Array.singleton $ PrivateKey.toPublicKey stakeKey
            Nothing ->
              mempty
      )

kwStakeKeysRegStatusWarning :: String -> String
kwStakeKeysRegStatusWarning funName =
  funName <>
    " via KeyWallet: KeyWallet does not distinguish between \
    \registered and unregistered stake keys due to the limitations \
    \of the underlying query layer. This means that all controlled \
    \stake keys are returned as part of ownUnregisteredPubStakeKeys, \
    \and the response of ownRegisteredPubStakeKeys is always an \
    \empty array."
