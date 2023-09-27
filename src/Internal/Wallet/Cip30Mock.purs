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

import Contract.Monad (Contract)
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (local)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Deserialization.Transaction (deserializeTransaction)
import Ctl.Internal.Helpers (liftEither)
import Ctl.Internal.Serialization
  ( convertTransactionUnspentOutput
  , convertValue
  , publicKeyHash
  , toBytes
  )
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId(MainnetId, TestnetId)
  )
import Ctl.Internal.Serialization.Keys (publicKeyFromPrivateKey)
import Ctl.Internal.Serialization.WitnessSet (convertWitnessSet)
import Ctl.Internal.Types.ByteArray (byteArrayToHex, hexToByteArray)
import Ctl.Internal.Types.CborBytes (cborBytesFromByteArray, cborBytesToHex)
import Ctl.Internal.Types.PubKeyHash
  ( PubKeyHash(PubKeyHash)
  , StakePubKeyHash(StakePubKeyHash)
  )
import Ctl.Internal.Types.RewardAddress
  ( rewardAddressToBytes
  , stakePubKeyHashRewardAddress
  )
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
import Data.Either (hush)
import Data.Foldable (fold, foldMap)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Map as Map
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
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
  , getUtxos :: Effect (Promise (Array String))
  , getCollateral :: Effect (Promise (Array String))
  , getBalance :: Effect (Promise String)
  , getUsedAddresses :: Effect (Promise (Array String))
  , getUnusedAddresses :: Effect (Promise (Array String))
  , getChangeAddress :: Effect (Promise String)
  , getRewardAddresses :: Effect (Promise (Array String))
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
        coinsPerUtxoUnit = pparams.coinsPerUtxoUnit
        maxCollateralInputs = UInt.toInt $
          pparams.maxCollateralInputs
      liftEffect $
        (unwrap keyWallet).selectCollateral coinsPerUtxoUnit
          maxCollateralInputs
          utxos
          <#> fold

    ownUtxos = do
      let ownAddress = (unwrap keyWallet).address env.networkId
      liftMaybe (error "No UTxOs at address") <<< hush =<< do
        queryHandle.utxosAt ownAddress

    keyWallet = privateKeysToKeyWallet pKey mSKey

    addressHex =
      byteArrayToHex $ unwrap $ toBytes
        ((unwrap keyWallet).address env.networkId :: Address)

    mbRewardAddressHex = mSKey <#> \stakeKey ->
      let
        stakePubKey = publicKeyFromPrivateKey (unwrap stakeKey)
        stakePubKeyHash = publicKeyHash stakePubKey
        rewardAddress = stakePubKeyHashRewardAddress env.networkId
          $ StakePubKeyHash
          $ PubKeyHash stakePubKeyHash
      in
        byteArrayToHex $ unwrap $ rewardAddressToBytes rewardAddress

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
          nonCollateralUtxos =
            Map.filterKeys (not <<< flip Array.elem collateralOutputs) utxos
        -- Convert to CSL representation and serialize
        cslUtxos <- traverse (liftEffect <<< convertTransactionUnspentOutput)
          $ Map.toUnfoldable nonCollateralUtxos <#> \(input /\ output) ->
              TransactionUnspentOutput { input, output }
        pure $ (byteArrayToHex <<< unwrap <<< toBytes) <$> cslUtxos
    , getCollateral: fromAff do
        utxos <- ownUtxos
        collateralUtxos <- getCollateralUtxos utxos
        cslUnspentOutput <- liftEffect $ traverse
          convertTransactionUnspentOutput
          collateralUtxos
        pure $ (byteArrayToHex <<< unwrap <<< toBytes) <$>
          cslUnspentOutput
    , getBalance: fromAff do
        utxos <- ownUtxos
        value <- liftEffect $ convertValue $
          (foldMap (_.amount <<< unwrap) <<< Map.values)
            utxos
        pure $ byteArrayToHex $ unwrap $ toBytes value
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
          $ hush
          $ deserializeTransaction
          $ cborBytesFromByteArray txBytes
        witness <- (unwrap keyWallet).signTx tx
        cslWitnessSet <- liftEffect $ convertWitnessSet witness
        pure $ byteArrayToHex $ unwrap $ toBytes cslWitnessSet
    , signData: mkFn2 \_addr msg -> unsafePerformEffect $ fromAff do
        msgBytes <- liftMaybe (error "Unable to convert CBOR") $
          hexToByteArray msg
        { key, signature } <- (unwrap keyWallet).signData env.networkId
          (wrap msgBytes)
        pure { key: cborBytesToHex key, signature: cborBytesToHex signature }
    }

-- returns an action that removes the mock.
foreign import injectCip30Mock :: String -> Cip30Mock -> Effect (Effect Unit)
