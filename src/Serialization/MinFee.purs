-- | `min_fee` calculation using CSL.
module Serialization.MinFee (calculateMinFeeCsl) where

import Prelude

import Cardano.Types.Transaction (_vkeys, _witnessSet)
import Cardano.Types.Transaction as T
import Cardano.Types.Value (Coin)
import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Lens ((.~))
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import QueryM.Ogmios (ProtocolParameters(ProtocolParameters))
import Serialization as Serialization
import Serialization.Types (ExUnitPrices, Transaction)
import Types.BigNum (BigNum)
import Types.BigNum as BigNum

calculateMinFeeCsl
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => MonadThrow Error m
  => ProtocolParameters
  -> T.Transaction
  -> m Coin
calculateMinFeeCsl (ProtocolParameters pparams) txNoSigs = do
  let
    tx = addFakeSignatures txNoSigs
    txFeePerByte = BigInt.fromInt $ UInt.toInt pparams.txFeePerByte
  cslTx <- liftEffect $ Serialization.convertTransaction tx
  minFee <- liftMaybe (error "Unable to calculate min_fee") $
    BigNum.toBigInt =<< _minFee maybeFfiHelper cslTx
      (BigNum.fromUInt pparams.txFeeFixed)
      (BigNum.fromUInt pparams.txFeePerByte)
  exUnitPrices <- liftMaybe (error "Unable to get ExUnitPrices") pparams.prices
  exUnitPricesCsl <- liftEffect $ Serialization.convertExUnitPrices exUnitPrices
  minScriptFee <-
    liftMaybe (error "Unable to calculate min_script_fee") $
      BigNum.toBigInt (_minScriptFee exUnitPricesCsl cslTx)
  pure $ wrap $ minFee + minScriptFee + BigInt.fromInt 3 * txFeePerByte

addFakeSignatures :: T.Transaction -> T.Transaction
addFakeSignatures tx =
  let
    nRequiredSigners =
      tx # unwrap >>> _.body >>> unwrap >>> _.requiredSigners
        >>> map Array.length
        >>> fromMaybe 1
  in
    tx # _witnessSet <<< _vkeys .~ Just
      (Array.replicate nRequiredSigners fakeVkeywitness)

fakeVkeywitness :: T.Vkeywitness
fakeVkeywitness = T.Vkeywitness
  ( ( T.Vkey
        ( T.PublicKey
            "ed25519_pk1p9sf9wz3t46u9ghht44203gerxt82kzqaqw74fqrmwjmdy8sjxmqknzq8j"
        )
    )
      /\
        ( T.Ed25519Signature
            "ed25519_sig1mr6pm5kanam2wkmae70jx7fjkzepghefj0lmnczu6fra\
            \6auf2urgrte5axxhunw4x34l3l8tj9c0t4le39tj8lpjdgxmqnujw07t\
            \kzs9m6t6x"
        )
  )

foreign import _minFee
  :: MaybeFfiHelper -> Transaction -> BigNum -> BigNum -> Maybe BigNum

foreign import _minScriptFee :: ExUnitPrices -> Transaction -> BigNum
