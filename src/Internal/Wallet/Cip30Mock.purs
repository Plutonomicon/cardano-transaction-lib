module Ctl.Internal.Wallet.Cip30Mock
  ( withCip30Mock
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Types
  ( Credential(PubKeyHashCredential)
  , StakeCredential(StakeCredential)
  )
import Cardano.Types.Address (Address(RewardAddress))
import Cardano.Types.Address (fromBech32) as Address
import Cardano.Types.NetworkId (NetworkId(MainnetId, TestnetId))
import Cardano.Types.PrivateKey as PrivateKey
import Cardano.Types.PublicKey as PublicKey
import Cardano.Types.TransactionUnspentOutput as TransactionUnspentOutput
import Cardano.Wallet.Cip30Mock (Cip30Mock, injectCip30Mock)
import Cardano.Wallet.Key
  ( KeyWallet(KeyWallet)
  , PrivateDrepKey
  , PrivatePaymentKey
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )
import Cardano.Wallet.Key (getPrivateDrepKey, getPrivateStakeKey) as KeyWallet
import Contract.Monad (Contract)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (local)
import Control.Promise (fromAff)
import Ctl.Internal.BalanceTx.Collateral.Select (minRequiredCollateral)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Helpers (liftEither)
import Ctl.Internal.Wallet (mkWalletAff)
import Data.Array as Array
import Data.ByteArray (byteArrayToHex, hexToByteArray)
import Data.Either (hush)
import Data.Foldable (fold, foldMap)
import Data.Function.Uncurried (mkFn2)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Data.UInt as UInt
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)

-- | Construct a CIP-30 + CIP-95 wallet mock that exposes `KeyWallet`
-- | functionality behind a CIP-30 interface and uses Ogmios to submit Txs.
-- | The wallet is injected directly to `window.cardano` object, under the
-- | name corresponding to provided `WalletMock`. It works even in NodeJS
-- | (we introduce a global `window` object and delete it afterwards).
-- |
-- | Note that this function will refuse to overwrite existing wallet
-- | with the mock, so the users should disable their browser extensions
-- | before running it in the browser.
-- |
-- | Note that this function implements single-address light wallet logic, so
-- | it will have to be changed a lot to successfully mimic the behavior of
-- | multi-address wallets, like Eternl.
-- |
-- | WARNING: KeyWallet does not distinguish between registered and
-- | unregistered stake keys due to the limitations of the underlying
-- | query layer. This means that all controlled stake keys are
-- | returned as part of getUnregisteredPubStakeKeys, and the response
-- | of getRegisteredPubStakeKeys is always an empty array.
withCip30Mock
  :: forall (a :: Type)
   . KeyWallet
  -> String
  -> Contract a
  -> Contract a
withCip30Mock (KeyWallet keyWallet) mock contract = do
  kwPaymentKey <- liftAff keyWallet.paymentKey
  kwMStakeKey <- liftAff keyWallet.stakeKey
  kwMDrepKey <- liftAff keyWallet.drepKey
  cip30Mock <- mkCip30Mock kwPaymentKey kwMStakeKey kwMDrepKey
  deleteMock <- liftEffect $ injectCip30Mock mock cip30Mock
  wallet <- liftAff $ mkWalletAff { name: mock, exts: { cip95: true } }
  res <- try $ local _ { wallet = Just wallet } contract
  liftEffect deleteMock
  liftEither res

mkCip30Mock
  :: PrivatePaymentKey
  -> Maybe PrivateStakeKey
  -> Maybe PrivateDrepKey
  -> Contract Cip30Mock
mkCip30Mock pKey mSKey mbDrepKey = do
  env <- ask
  queryHandle <- getQueryHandle
  let
    getCollateralUtxos utxos = do
      let
        pparams = unwrap env.ledgerConstants.pparams
        coinsPerUtxoByte = pparams.coinsPerUtxoByte
        maxCollateralInputs = UInt.toInt $
          pparams.maxCollateralInputs
      coll <- liftAff $
        (unwrap keyWallet).selectCollateral
          minRequiredCollateral
          coinsPerUtxoByte
          maxCollateralInputs
          utxos
      pure $ fold coll
    ownUtxos = do
      ownAddress <- liftAff $ (unwrap keyWallet).address env.networkId
      liftMaybe (error "No UTxOs at address") <<< hush =<< do
        queryHandle.utxosAt ownAddress

    keyWallet = privateKeysToKeyWallet pKey mSKey mbDrepKey

  addressHex <- liftAff $
    (byteArrayToHex <<< unwrap <<< encodeCbor) <$>
      ((unwrap keyWallet).address env.networkId :: Aff Address)
  let
    mbRewardAddressHex = mSKey <#> \stakeKey ->
      let
        stakePubKey = PrivateKey.toPublicKey (unwrap stakeKey)
        stakePubKeyHash = PublicKey.hash stakePubKey
        rewardAddress = RewardAddress
          { networkId: env.networkId
          , stakeCredential:
              StakeCredential $ PubKeyHashCredential stakePubKeyHash
          }
      in
        byteArrayToHex $ unwrap $ encodeCbor rewardAddress
  pure $
    { getNetworkId: fromAff $ pure $
        case env.networkId of
          TestnetId -> 0
          MainnetId -> 1
    , getUtxos: fromAff do
        utxos <- ownUtxos
        collateralUtxos <- getCollateralUtxos utxos
        let
          collateralOutputs = collateralUtxos <#> unwrap >>> _.input
          -- filter out UTxOs that will be used as collateral
          utxoMap =
            Map.filterKeys (not <<< flip Array.elem collateralOutputs) utxos
        pure $ byteArrayToHex <<< unwrap <<< encodeCbor <$>
          TransactionUnspentOutput.fromUtxoMap utxoMap
    , getCollateral: fromAff do
        utxos <- ownUtxos
        collateralUtxos <- getCollateralUtxos utxos
        pure $ byteArrayToHex <<< unwrap <<< encodeCbor <$> collateralUtxos
    , getBalance: unsafePartial $ fromAff do
        utxos <- ownUtxos
        let value = foldMap (_.amount <<< unwrap) $ Map.values utxos
        pure $ byteArrayToHex $ unwrap $ encodeCbor value
    , getUsedAddresses: fromAff do
        pure [ addressHex ]
    , getUnusedAddresses: fromAff $ pure []
    , getChangeAddress: fromAff do
        pure addressHex
    , getRewardAddresses: fromAff do
        pure (maybe [] pure mbRewardAddressHex)
    , signTx: \str -> unsafePerformEffect $ fromAff do
        txBytes <- liftMaybe (error "Unable to convert CBOR") $ hexToByteArray
          str
        tx <- liftMaybe (error "Failed to decode Transaction CBOR")
          $ decodeCbor
          $ wrap txBytes
        witness <- (unwrap keyWallet).signTx tx
        pure $ byteArrayToHex $ unwrap $ encodeCbor witness
    , signData: mkFn2 \addrRaw msg -> unsafePerformEffect $ fromAff do
        let addrFromHex = (decodeCbor <<< wrap) <=< hexToByteArray
        addr <-
          liftMaybe
            (error "Failed to decode Address")
            (addrFromHex addrRaw <|> Address.fromBech32 addrRaw)
        msgBytes <-
          liftMaybe
            (error "Failed to decode payload")
            (hexToByteArray msg)
        mDataSig <- (unwrap keyWallet).signData addr (wrap msgBytes)
        { key, signature } <-
          liftMaybe
            (error "Unable to sign data for the supplied address")
            mDataSig
        pure
          { key: byteArrayToHex $ unwrap key
          , signature: byteArrayToHex $ unwrap signature
          }
    , getPubDrepKey: fromAff do
        drepKey <- liftMaybe (error "Unable to get DRep key") =<<
          KeyWallet.getPrivateDrepKey keyWallet
        let drepPubKey = PrivateKey.toPublicKey $ unwrap drepKey
        pure $ byteArrayToHex $ unwrap $ PublicKey.toRawBytes drepPubKey
    , getRegisteredPubStakeKeys: fromAff $ pure mempty
    , getUnregisteredPubStakeKeys: fromAff do
        KeyWallet.getPrivateStakeKey keyWallet <#> case _ of
          Just stakeKey ->
            let
              stakePubKey = PrivateKey.toPublicKey $ unwrap stakeKey
            in
              Array.singleton $ byteArrayToHex $ unwrap $ PublicKey.toRawBytes
                stakePubKey
          Nothing ->
            mempty
    }
