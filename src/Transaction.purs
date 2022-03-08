module Transaction
  ( ModifyTxError(..)
  , attachDatum
  , attachRedeemer
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Either (Either(Right), note)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Effect (Effect)
import Effect.Class (liftEffect)
import Helpers (liftEither)
import Serialization.PlutusData as Serialization.PlutusData
import Serialization.Types as Serialization
import Serialization.WitnessSet as Serialization.WitnessSet
import Types.Transaction
  ( Datum(Datum)
  , Transaction(Transaction)
  , Redeemer
  , TransactionWitnessSet
  )

data ModifyTxError
  = ConvertWitnessesError
  | ConvertDatumError

derive instance Generic ModifyTxError _

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
  newWits <- convertWitnessesWith ws $
    Serialization.WitnessSet.setPlutusData pd'
  liftEither $ Right $ Transaction $ tx { witness_set = newWits }

-- | Attach a `Redeemer` to a transaction by modifying its existing witness set.
-- | Note that this is the `Types.Transaction` representation of a redeemer and
-- | not a wrapped `PlutusData`.
--
-- | Fails if either the redeemer or updated witness set cannot be converted
-- | during (de-)serialization
attachRedeemer
  :: Redeemer -> Transaction -> Effect (Either ModifyTxError Transaction)
attachRedeemer r (Transaction tx@{ witness_set: ws }) = runExceptT $ do
  r' <- liftEffect $ Serialization.WitnessSet.convertRedeemer r
  newWits <- convertWitnessesWith ws $
    Serialization.WitnessSet.setRedeemer r'
  pure $ Transaction $ tx { witness_set = newWits }

convertWitnessesWith
  :: TransactionWitnessSet
  -> (Serialization.TransactionWitnessSet -> Effect Unit)
  -> ExceptT ModifyTxError Effect TransactionWitnessSet
convertWitnessesWith ws act = do
  ws' <- liftEffect $ Serialization.WitnessSet.convertWitnessSet ws
  liftEffect $ act ws'
  liftEither $ note ConvertWitnessesError
    $ Deserialization.WitnessSet.convertWitnessSet ws'
