-- | `min_fee` calculation using CSL.
module Ctl.Internal.Serialization.MinFee (calculateMinFeeCsl) where

import Prelude

import Cardano.Serialization.Lib (linearFee_new, minFee, minScriptFee)
import Cardano.Types
  ( Coin
  , Ed25519KeyHash
  , Transaction
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  , _vkeys
  , _witnessSet
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Ed25519Signature as Ed25519Signature
import Cardano.Types.ExUnitPrices as ExUnitPrices
import Cardano.Types.NativeScript (NativeScript(ScriptAll))
import Cardano.Types.PublicKey as PublicKey
import Cardano.Types.Transaction as Transaction
import Control.Monad.Error.Class (class MonadThrow)
import Ctl.Internal.Helpers (unsafeFromJust)
import Ctl.Internal.NativeScripts (getMaximumSigners)
import Ctl.Internal.Types.ProtocolParameters
  ( ProtocolParameters(ProtocolParameters)
  )
import Data.Array as Array
import Data.Lens ((.~))
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)

calculateMinFeeCsl
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => MonadThrow Error m
  => ProtocolParameters
  -> Set Ed25519KeyHash
  -> Transaction
  -> m Coin
calculateMinFeeCsl (ProtocolParameters pparams) selfSigners txNoSigs = do
  let tx = addFakeSignatures selfSigners txNoSigs
  let cslTx = Transaction.toCsl tx
  let
    cslLinearFee = linearFee_new
      (unwrap $ BigNum.fromUInt pparams.txFeePerByte)
      (unwrap $ BigNum.fromUInt pparams.txFeeFixed)

  let fee = minFee cslTx cslLinearFee
  let exUnitPrices = pparams.prices
  let exUnitPricesCsl = ExUnitPrices.toCsl exUnitPrices
  let scriptFee = minScriptFee cslTx exUnitPricesCsl
  -- Ignore the overflow here: fees are much lower
  pure $ wrap $ unsafeFromJust "calculateMinFeeCsl" $ BigNum.add (wrap fee)
    (wrap scriptFee)

-- | Adds fake signatures for each expected signature of a transaction.
addFakeSignatures :: Set Ed25519KeyHash -> Transaction -> Transaction
addFakeSignatures selfSigners tx =
  let
    -- requiredSigners field of a transaction
    requiredSigners :: Set Ed25519KeyHash
    requiredSigners =
      tx # unwrap >>> _.body >>> unwrap >>> _.requiredSigners
        >>> Set.fromFoldable

    -- All possible signers from NativeScript.
    nsPossibleSigners :: Int
    nsPossibleSigners = getMaximumSigners requiredSigners $ ScriptAll
      ( tx # unwrap >>> _.witnessSet >>> unwrap >>> _.nativeScripts
      )

    -- We want to add space for required signatures (at least one, if
    -- none specified).
    nRequiredSigners = tx # unwrap >>> _.body >>> unwrap >>> _.requiredSigners
      >>> Array.length

    nSelfSigners = let n = Set.size selfSigners in if n == 0 then 1 else n

  in
    tx # _witnessSet <<< _vkeys .~
      ( Array.replicate (nRequiredSigners + nsPossibleSigners + nSelfSigners)
          fakeVkeywitness
      )

fakeVkeywitness :: Vkeywitness
fakeVkeywitness = Vkeywitness
  { vkey:
      ( Vkey
          ( unsafePartial $ fromJust $ PublicKey.fromBech32
              -- This should not fail assuming the hardcoded bech32 key is valid.
              "ed25519_pk1p9sf9wz3t46u9ghht44203gerxt82kzqaqw74fqrmwjmdy8sjxmqknzq8j"
          )
      )
  , signature:
      ( unsafePartial $ fromJust $ Ed25519Signature.fromBech32
          "ed25519_sig1mr6pm5kanam2wkmae70jx7fjkzepghefj0lmnczu6fra\
          \6auf2urgrte5axxhunw4x34l3l8tj9c0t4le39tj8lpjdgxmqnujw07t\
          \kzs9m6t6x"
      )
  }
