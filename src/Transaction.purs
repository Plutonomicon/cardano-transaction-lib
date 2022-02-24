module Transaction (attachDatum) where

import Prelude
import Undefined

import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Traversable
import Effect (Effect)
import Serialization.WitnessSet as Serialization.WitnessSet
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Serialization.PlutusData as Serialization.PlutusData
import Types.PlutusData (Datum(Datum))
import Types.Transaction (Transaction(Transaction))

attachDatum :: Datum -> Transaction -> Effect (Maybe Transaction)
attachDatum (Datum pd) (Transaction tx) = runMaybeT $ do
  pd' <- liftMaybe $ Serialization.PlutusData.convertPlutusData pd
  datumWits <- MaybeT $
    map (map unwrap <<< Deserialization.WitnessSet.convertWitnessSet)
      <<< Serialization.WitnessSet.setPlutusData pd'
      =<< Serialization.WitnessSet.newTransactionWitnessSet
  liftMaybe $ Just $ Transaction $ tx
    { witness_set = wrap $ unwrap tx.witness_set <> datumWits
    }
  where
  liftMaybe
    :: forall (a :: Type) (m :: Type -> Type)
     . Applicative m
    => Maybe a
    -> MaybeT m a
  liftMaybe = MaybeT <<< pure
