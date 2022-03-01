module Transaction
  ( ModifyTxError(..)
  , attachDatum
  ) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(Right), note)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Effect (Effect)
import Effect.Class (liftEffect)
import Helpers (liftEither)
import Serialization.PlutusData as Serialization.PlutusData
import Serialization.WitnessSet as Serialization.WitnessSet
import Types.PlutusData (Datum(Datum))
import Types.Transaction (Transaction(Transaction))

data ModifyTxError
  = ConvertWitnessesError
  | ConvertDatumError

derive instance Generic ModifyTxError _
derive instance Eq ModifyTxError

instance Show ModifyTxError where
  show = genericShow

-- | Attach a `Datum` to a transaction by modifying its existing witness set.
-- | Fails if either the datum or updated witness set cannot be converted during
-- | (de-)serialization
attachDatum :: Datum -> Transaction -> Effect (Either ModifyTxError Transaction)
attachDatum (Datum pd) (Transaction tx@{ witness_set: ws }) = runExceptT $ do
  pd' <- liftEither
    $ note ConvertDatumError
    $ Serialization.PlutusData.convertPlutusData pd
  ws' <- liftEffect $ Serialization.WitnessSet.convertWitnessSet ws
  liftEffect $ Serialization.WitnessSet.setPlutusData pd' ws'
  newWits <- liftEither $ note ConvertWitnessesError
    $ Deserialization.WitnessSet.convertWitnessSet ws'
  liftEither $ Right $ Transaction $ tx { witness_set = newWits }
