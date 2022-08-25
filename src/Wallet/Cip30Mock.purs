module Wallet.Cip30Mock where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Reader (ask)
import Control.Promise (Promise, fromAff)
import Data.Either (hush)
import Data.Foldable (fold)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Deserialization.Transaction (deserializeTransaction)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import QueryM (QueryM, runQueryMInRuntime)
import QueryM.Utxos (utxosAt)
import Serialization (convertTransactionUnspentOutput, convertValue, toBytes)
import Serialization.Address (NetworkId)
import Serialization.WitnessSet (convertWitnessSet)
import Types.ByteArray (byteArrayToHex, hexToByteArray)
import Types.CborBytes (cborBytesFromByteArray)
import Untagged.Union (asOneOf)
import Wallet.Key (PrivatePaymentKey, PrivateStakeKey, privateKeysToKeyWallet)

type Cip30Mock =
  { getUsedAddresses :: Effect (Promise (Array String))
  , getCollateral :: Effect (Promise (Array String))
  , signTx :: String -> Promise String
  , getBalance :: Effect (Promise String)
  }

mkCip30Mock
  :: NetworkId -> PrivatePaymentKey -> Maybe PrivateStakeKey -> QueryM Cip30Mock
mkCip30Mock networkId pKey mSKey = do
  { config, runtime } <- ask
  pure $
    { getUsedAddresses: fromAff do
        (unwrap keyWallet).address networkId <#> \address ->
          [ byteArrayToHex $ toBytes (asOneOf address) ]
    , getCollateral: fromAff do
        ownAddress <- (unwrap keyWallet).address networkId
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
        ownAddress <- (unwrap keyWallet).address networkId
        utxos <- map unwrap $ liftMaybe (error "No UTxOs at address") =<<
          runQueryMInRuntime config runtime (utxosAt ownAddress)
        value <- liftEffect $ convertValue $
          (fold <<< map _.amount <<< map unwrap <<< Map.values)
            utxos
        pure $ byteArrayToHex $ toBytes $ asOneOf value
    }
  where
  keyWallet = privateKeysToKeyWallet pKey mSKey

foreign import injectCip30Mock :: String -> Cip30Mock -> Effect Unit
