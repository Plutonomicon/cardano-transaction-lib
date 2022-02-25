module Transaction (attachDatum) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Data.Maybe (Maybe(Just))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
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

data ModifyTxError
  = ConvertWitsError
  | ConvertDatumError

derive instance Generic ModifyTxError _

instance Show ModifyTxError where
  show a = genericShow a
