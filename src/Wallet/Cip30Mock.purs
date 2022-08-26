module Wallet.Cip30Mock where

import Prelude

import Contract.Monad (Contract, ContractEnv, wrapContract)
import Control.Monad.Error.Class (liftMaybe, throwError, try)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (local)
import Control.Promise (Promise, fromAff)
import Data.Either (Either(Left, Right), hush)
import Data.Foldable (fold)
import Data.Lens ((.~))
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(Just))
import Data.Newtype (unwrap)
import Deserialization.Transaction (deserializeTransaction)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import QueryM (QueryM, runQueryMInRuntime)
import QueryM.Utxos (utxosAt)
import Serialization (convertTransactionUnspentOutput, convertValue, toBytes)
import Serialization.WitnessSet (convertWitnessSet)
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray (byteArrayToHex, hexToByteArray)
import Types.CborBytes (cborBytesFromByteArray)
import Untagged.Union (asOneOf)
import Wallet (mkFlintWalletAff, mkGeroWalletAff, mkNamiWalletAff)
import Wallet.Key
  ( KeyWallet(KeyWallet)
  , PrivatePaymentKey
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )

data WalletMock = MockFlint | MockGero | MockNami

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
  :: forall r a. KeyWallet -> WalletMock -> Contract r a -> Contract r a
withCip30Mock (KeyWallet keyWallet) mock contract = do
  cip30Mock <- wrapContract $ mkCip30Mock keyWallet.paymentKey
    keyWallet.stakeKey
  deleteMock <- liftEffect $ injectCip30Mock mockString cip30Mock
  wallet <- liftAff mkWalletAff
  let
    setUpdatedWallet :: ContractEnv r -> ContractEnv r
    setUpdatedWallet =
      simple _Newtype <<< prop (Proxy :: Proxy "runtime") <<< prop
        (Proxy :: Proxy "wallet") .~
        (Just wallet)
  res <- try $ local setUpdatedWallet contract
  liftEffect deleteMock
  case res of
    Left err -> throwError err
    Right r -> pure r
  where
  mkWalletAff = case mock of
    MockFlint -> mkFlintWalletAff
    MockGero -> mkGeroWalletAff
    MockNami -> mkNamiWalletAff
  mockString = case mock of
    MockFlint -> "flint"
    MockGero -> "gerowallet"
    MockNami -> "nami"

type Cip30Mock =
  { getUsedAddresses :: Effect (Promise (Array String))
  , getCollateral :: Effect (Promise (Array String))
  , signTx :: String -> Promise String
  , getBalance :: Effect (Promise String)
  }

mkCip30Mock
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> QueryM Cip30Mock
mkCip30Mock pKey mSKey = do
  { config, runtime } <- ask
  pure $
    { getUsedAddresses: fromAff do
        (unwrap keyWallet).address config.networkId <#> \address ->
          [ byteArrayToHex $ toBytes (asOneOf address) ]
    , getCollateral: fromAff do
        ownAddress <- (unwrap keyWallet).address config.networkId
        utxos <- map unwrap $ liftMaybe (error "No UTxOs at address") =<<
          runQueryMInRuntime config runtime (utxosAt ownAddress)
        collateralUtxos <- liftMaybe (error "No UTxOs at address") $
          (unwrap keyWallet).selectCollateral utxos
        cslUnspentOutput <- liftEffect $ convertTransactionUnspentOutput
          collateralUtxos
        pure [ byteArrayToHex $ toBytes $ asOneOf cslUnspentOutput ]
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
    , getBalance: fromAff do
        ownAddress <- (unwrap keyWallet).address config.networkId
        utxos <- map unwrap $ liftMaybe (error "No UTxOs at address") =<<
          runQueryMInRuntime config runtime (utxosAt ownAddress)
        value <- liftEffect $ convertValue $
          (fold <<< map _.amount <<< map unwrap <<< Map.values)
            utxos
        pure $ byteArrayToHex $ toBytes $ asOneOf value
    }
  where
  keyWallet = privateKeysToKeyWallet pKey mSKey

-- returns an action that removes the mock.
foreign import injectCip30Mock :: String -> Cip30Mock -> Effect (Effect Unit)
