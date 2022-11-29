module Ctl.Internal.Wallet.Cip30Mock where

import Prelude

import Contract.Monad (Contract)
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (local)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Ctl.Internal.Contract.QueryHandle (getQueryHandle')
import Ctl.Internal.Deserialization.Transaction (deserializeTransaction)
import Ctl.Internal.Helpers (liftEither)
import Ctl.Internal.Serialization
  ( convertTransactionUnspentOutput
  , convertValue
  , toBytes
  )
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId(TestnetId, MainnetId)
  )
import Ctl.Internal.Serialization.WitnessSet (convertWitnessSet)
import Ctl.Internal.Types.ByteArray (byteArrayToHex, hexToByteArray)
import Ctl.Internal.Types.CborBytes (cborBytesFromByteArray)
import Ctl.Internal.Wallet
  ( Wallet
  , WalletExtension(LodeWallet, NamiWallet, GeroWallet, FlintWallet)
  , mkWalletAff
  )
import Ctl.Internal.Wallet.Cip30 (DataSignature)
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
import Data.Maybe (Maybe(Just))
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
import Untagged.Union (asOneOf)

data WalletMock = MockFlint | MockGero | MockNami | MockLode

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

  mockString :: String
  mockString = case mock of
    MockFlint -> "flint"
    MockGero -> "gerowallet"
    MockNami -> "nami"
    MockLode -> "LodeWallet"

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
  , signData :: Fn2 String String (Promise DataSignature)
  }

mkCip30Mock
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Contract Cip30Mock
mkCip30Mock pKey mSKey = do
  env <- ask
  let
    getCollateralUtxos utxos = do
      let
        pparams = unwrap $ env.ledgerConstants.pparams
        coinsPerUtxoUnit = pparams.coinsPerUtxoUnit
        maxCollateralInputs = UInt.toInt $
          pparams.maxCollateralInputs
      liftEffect $
        (unwrap keyWallet).selectCollateral coinsPerUtxoUnit
          maxCollateralInputs
          utxos
          <#> fold

    utxosAt address = liftMaybe (error "No UTxOs at address") <<< hush =<< do
      let queryHandle = getQueryHandle' env
      queryHandle.utxosAt address

    keyWallet = privateKeysToKeyWallet env.networkId pKey mSKey

    addressHex =
      byteArrayToHex $ toBytes $ asOneOf ((unwrap keyWallet).address :: Address)

  pure $
    { getNetworkId: fromAff $ pure $
        case env.networkId of
          TestnetId -> 0
          MainnetId -> 1
    , getUtxos: fromAff do
        let ownAddress = (unwrap keyWallet).address
        utxos <- utxosAt ownAddress
        collateralUtxos <- getCollateralUtxos utxos
        let
          -- filter UTxOs that will be used as collateral
          nonCollateralUtxos =
            Map.filter
              (flip Array.elem (collateralUtxos <#> unwrap >>> _.output))
              utxos
        -- Convert to CSL representation and serialize
        cslUtxos <- traverse (liftEffect <<< convertTransactionUnspentOutput)
          $ Map.toUnfoldable nonCollateralUtxos <#> \(input /\ output) ->
              TransactionUnspentOutput { input, output }
        pure $ (byteArrayToHex <<< toBytes <<< asOneOf) <$> cslUtxos
    , getCollateral: fromAff do
        let ownAddress = (unwrap keyWallet).address
        utxos <- utxosAt ownAddress
        collateralUtxos <- getCollateralUtxos utxos
        cslUnspentOutput <- liftEffect $ traverse
          convertTransactionUnspentOutput
          collateralUtxos
        pure $ (byteArrayToHex <<< toBytes <<< asOneOf) <$> cslUnspentOutput
    , getBalance: fromAff do
        let ownAddress = (unwrap keyWallet).address
        utxos <- utxosAt ownAddress
        value <- liftEffect $ convertValue $
          (foldMap (_.amount <<< unwrap) <<< Map.values)
            utxos
        pure $ byteArrayToHex $ toBytes $ asOneOf value
    , getUsedAddresses: fromAff do
        pure [ addressHex ]
    , getUnusedAddresses: fromAff $ pure []
    , getChangeAddress: fromAff do
        pure addressHex
    , getRewardAddresses: fromAff do
        pure [ addressHex ]
    , signTx: \str -> unsafePerformEffect $ fromAff do
        txBytes <- liftMaybe (error "Unable to convert CBOR") $ hexToByteArray
          str
        tx <- liftMaybe (error "Failed to decode Transaction CBOR")
          $ hush
          $ deserializeTransaction
          $ cborBytesFromByteArray txBytes
        witness <- (unwrap keyWallet).signTx tx
        cslWitnessSet <- liftEffect $ convertWitnessSet witness
        pure $ byteArrayToHex $ toBytes $ asOneOf cslWitnessSet
    , signData: mkFn2 \_addr msg -> unsafePerformEffect $ fromAff do
        msgBytes <- liftMaybe (error "Unable to convert CBOR")
          (hexToByteArray msg)
        (unwrap keyWallet).signData (wrap msgBytes)
    }

-- returns an action that removes the mock.
foreign import injectCip30Mock :: String -> Cip30Mock -> Effect (Effect Unit)
