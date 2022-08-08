-- | `min_fee` calculation using CSL.
-- | TODO split part to QueryM.CalculateMinFee
module Serialization.MinFee where

import Prelude

import Cardano.Types.Transaction as T
import Cardano.Types.Value (Coin)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (wrap)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import QueryM.Ogmios (ProtocolParameters(ProtocolParameters))
import Serialization as Serialization
import Serialization.Types (Transaction)
import Types.BigNum (BigNum)
import Types.BigNum as BigNum

foreign import _minFee
  :: MaybeFfiHelper -> Transaction -> BigNum -> BigNum -> Maybe BigNum

calculateMinFeeCsl
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => ProtocolParameters
  -> T.Transaction
  -> m Coin
calculateMinFeeCsl (ProtocolParameters pparams) tx = do
  cslTx <- liftEffect $ Serialization.convertTransaction tx
  maybe (liftEffect $ throw "Unable to calculate minfee") (pure <<< wrap) $
    BigNum.toBigInt =<< _minFee maybeFfiHelper cslTx
      (BigNum.fromUInt pparams.txFeeFixed)
      (BigNum.fromUInt pparams.txFeePerByte)
