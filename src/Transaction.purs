module Transaction (attachDatum) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Data.Maybe (Maybe(Just))
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Serialization.WitnessSet as Serialization.WitnessSet
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Serialization.PlutusData as Serialization.PlutusData
import Types.PlutusData (Datum(Datum))
import Types.Transaction (Transaction(Transaction), TransactionWitnessSet)

attachDatum :: Datum -> Transaction -> Effect (Maybe Transaction)
attachDatum (Datum pd) (Transaction tx@{ witness_set: ws }) = runMaybeT $ do
  pd' <- liftMaybe $ Serialization.PlutusData.convertPlutusData pd
  newWits <- MaybeT $
    map Deserialization.WitnessSet.convertWitnessSet
      <<< Serialization.WitnessSet.setPlutusData pd'
      =<< Serialization.WitnessSet.convertWitnessSet ws
  liftMaybe $ Just $ Transaction $ tx { witness_set = newWits }
  where
  liftMaybe
    :: forall (a :: Type) (m :: Type -> Type)
     . Applicative m
    => Maybe a
    -> MaybeT m a
  liftMaybe = MaybeT <<< pure
