module Ctl.Internal.Wallet.Cip30Mock
  ( withCip30Mock
  , WalletMock
      ( MockFlint
      , MockGero
      , MockNami
      , MockLode
      , MockNuFi
      , MockGenericCip30
      )
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Types
  ( Credential(PubKeyHashCredential)
  , StakeCredential(StakeCredential)
  )
import Cardano.Types.Address (Address(..))
import Cardano.Types.NetworkId (NetworkId(..))
import Cardano.Types.PrivateKey as PrivateKey
import Cardano.Types.PublicKey as PublicKey
import Cardano.Types.TransactionUnspentOutput as TransactionUnspentOutput
import Contract.Monad (Contract)
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (local)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Helpers (liftEither)
import Ctl.Internal.Wallet
  ( Wallet
  , WalletExtension
      ( LodeWallet
      , NamiWallet
      , GeroWallet
      , FlintWallet
      , NuFiWallet
      , GenericCip30Wallet
      )
  , mkWalletAff
  )
import Ctl.Internal.Wallet.Key
  ( KeyWallet(KeyWallet)
  , PrivatePaymentKey
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )
import Data.Array as Array
import Data.ByteArray (byteArrayToHex, hexToByteArray)
import Data.Either (hush)
import Data.Foldable (fold, foldMap)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Map as Map
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (unwrap, wrap)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)

data WalletMock
  = MockFlint
  | MockGero
  | MockNami
  | MockLode
  | MockNuFi
  | MockGenericCip30 String

-- | Construct a CIP-30 wallet mock that exposes `KeyWallet` functionality
-- | behind a CIP-30 interface and uses Ogmios to submit Txs.
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
withCip30Mock
  :: forall (a :: Type)
   . KeyWallet
  -> WalletMock
  -> Contract a
  -> Contract a
withCip30Mock (KeyWallet keyWallet) mock contract = do
  cip30Mock <- mkCip30Mock keyWallet.paymentKey
    keyWallet.stakeKey
  deleteMock <- liftEffect $ injectCip30Mock mockString cip30Mock
  wallet <- liftAff mkWalletAff'
  res <- try $ local _ { wallet = Just wallet } contract
  liftEffect deleteMock
  liftEither res
  where
  mkWalletAff' :: Aff Wallet
  mkWalletAff' = case mock of
    MockFlint -> mkWalletAff FlintWallet
    MockGero -> mkWalletAff GeroWallet
    MockNami -> mkWalletAff NamiWallet
    MockLode -> mkWalletAff LodeWallet
    MockNuFi -> mkWalletAff NuFiWallet
    MockGenericCip30 name -> mkWalletAff (GenericCip30Wallet name)

  mockString :: String
  mockString = case mock of
    MockFlint -> "flint"
    MockGero -> "gerowallet"
    MockNami -> "nami"
    MockLode -> "LodeWallet"
    MockNuFi -> "nufi"
    MockGenericCip30 name -> name

type Cip30Mock =
  { getNetworkId :: Effect (Promise Int)
  -- we ignore both the amount parameter and pagination:
  , getUtxos :: Effect (Promise (Array String))
  -- we ignore the amount parameter:
  , getCollateral :: Effect (Promise (Array String))
  , getBalance :: Effect (Promise String)
  -- we ignore pagination parameter:
  , getUsedAddresses :: Effect (Promise (Array String))
  , getUnusedAddresses :: Effect (Promise (Array String))
  , getChangeAddress :: Effect (Promise String)
  , getRewardAddresses :: Effect (Promise (Array String))
  -- we ignore the 'isPartial' parameter
  , signTx :: String -> Promise String
  , signData ::
      Fn2 String String (Promise { key :: String, signature :: String })
  }

mkCip30Mock
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Contract Cip30Mock
mkCip30Mock pKey mSKey = do
  env <- ask
  queryHandle <- getQueryHandle
  let
    getCollateralUtxos utxos = do
      let
        pparams = unwrap env.ledgerConstants.pparams
        coinsPerUtxoByte = pparams.coinsPerUtxoByte
        maxCollateralInputs = UInt.toInt $
          pparams.maxCollateralInputs
        mbCollateral = fold $
          (unwrap keyWallet).selectCollateral coinsPerUtxoByte
            maxCollateralInputs
            utxos
      pure mbCollateral
    ownUtxos = do
      let ownAddress = (unwrap keyWallet).address env.networkId
      liftMaybe (error "No UTxOs at address") <<< hush =<< do
        queryHandle.utxosAt ownAddress

    keyWallet = privateKeysToKeyWallet pKey mSKey

    addressHex =
      byteArrayToHex $ unwrap $ encodeCbor
        ((unwrap keyWallet).address env.networkId :: Address)

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
    , getBalance: fromAff do
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
    , signData: mkFn2 \_addr msg -> unsafePerformEffect $ fromAff do
        msgBytes <- liftMaybe (error "Unable to convert CBOR") $
          hexToByteArray msg
        { key, signature } <- (unwrap keyWallet).signData env.networkId
          (wrap msgBytes)
        pure
          { key: byteArrayToHex $ unwrap key
          , signature: byteArrayToHex $ unwrap signature
          }
    }

-- returns an action that removes the mock.
foreign import injectCip30Mock :: String -> Cip30Mock -> Effect (Effect Unit)
