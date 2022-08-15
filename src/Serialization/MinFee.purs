-- | `min_fee` calculation using CSL.
module Serialization.MinFee (calculateMinFeeCsl) where

import Prelude

import Cardano.Types.Transaction (NativeScript(ScriptAll), _vkeys, _witnessSet)
import Cardano.Types.Transaction as T
import Cardano.Types.Value (Coin)
import Contract.Address (Ed25519KeyHash)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Lens ((.~))
import Data.Maybe (Maybe(Just), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import NativeScripts (getMaximumSigners)
import QueryM.Ogmios (ProtocolParameters(ProtocolParameters))
import Serialization as Serialization
import Serialization.Types (ExUnitPrices, Transaction)
import Types.BigNum (BigNum)
import Types.BigNum as BigNum

calculateMinFeeCsl
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => ProtocolParameters
  -> T.Transaction
  -> m Coin
calculateMinFeeCsl (ProtocolParameters pparams) txNoSigs = do
  let tx = addFakeSignatures txNoSigs
  let txFeePerByte = BigInt.fromInt $ UInt.toInt pparams.txFeePerByte
  cslTx <- liftEffect $ Serialization.convertTransaction tx
  minFee <- maybe (liftEffect $ throw "Unable to calculate min_fee") pure $
    BigNum.toBigInt =<< _minFee maybeFfiHelper cslTx
      (BigNum.fromUInt pparams.txFeeFixed)
      (BigNum.fromUInt pparams.txFeePerByte)
  exUnitPrices <- maybe (liftEffect $ throw "Unable to get ExUnitPrices") pure $
    pparams.prices
  exUnitPricesCsl <- liftEffect $ Serialization.convertExUnitPrices exUnitPrices
  minScriptFee <-
    maybe (liftEffect $ throw "Unable to calculate min_script_fee") pure $
      BigNum.toBigInt (_minScriptFee exUnitPricesCsl cslTx)
  pure $ wrap $ minFee + minScriptFee + BigInt.fromInt 3 * txFeePerByte

-- | Adds fake signatures for each expected signature of a transaction.
addFakeSignatures :: T.Transaction -> T.Transaction
addFakeSignatures tx =
  let
    -- requiredSigners field of a transaction
    requiredSigners :: Set Ed25519KeyHash
    requiredSigners =
      tx # unwrap >>> _.body >>> unwrap >>> _.requiredSigners
        >>> fromMaybe mempty
        >>> map unwrap
        >>> Set.fromFoldable

    -- All possible signers from NativeScript.
    nsPossibleSigners :: Int
    nsPossibleSigners = getMaximumSigners requiredSigners $ ScriptAll
      ( tx # unwrap >>> _.witnessSet >>> unwrap >>> _.nativeScripts >>>
          fromMaybe mempty
      )

    -- We want to add space for required signatures (at least one, if
    -- none specified).
    nRequiredSigners = tx # unwrap >>> _.body >>> unwrap >>> _.requiredSigners
      >>> map (map unwrap >>> Array.length)
      >>> fromMaybe 1

  in
    tx # _witnessSet <<< _vkeys .~ Just
      (Array.replicate (nRequiredSigners + nsPossibleSigners) fakeVkeywitness)

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
