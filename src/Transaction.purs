module Transaction
  ( ModifyTxError(..)
  , attachDatum
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Data.Either (Either(Right), note)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Helpers (liftEither)
import Serialization.WitnessSet as Serialization.WitnessSet
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Serialization.PlutusData as Serialization.PlutusData
import Types.PlutusData (Datum(Datum))
import Types.Transaction (Transaction(Transaction), TransactionWitnessSet)

data ModifyTxError
  = ConvertWitsError
  | ConvertDatumError

derive instance Generic ModifyTxError _

instance Show ModifyTxError where
  show a = genericShow a

attachDatum :: Datum -> Transaction -> Effect (Either ModifyTxError Transaction)
attachDatum (Datum pd) (Transaction tx@{ witness_set: ws }) = runExceptT $ do
  pd' <- liftEither
    $ note ConvertDatumError
    $ Serialization.PlutusData.convertPlutusData pd
  newWits <- ExceptT $
    map
      ( note ConvertDatumError
          <<< Deserialization.WitnessSet.convertWitnessSet
      )
      <<< Serialization.WitnessSet.setPlutusData pd'
      =<< Serialization.WitnessSet.convertWitnessSet ws
  liftEither $ Right $ Transaction $ tx { witness_set = newWits }
