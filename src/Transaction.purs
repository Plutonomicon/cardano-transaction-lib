module Transaction
  ( ModifyTxError(..)
  , attachDatum
  , attachRedeemer
  , attachPlutusScript
  , setScriptDataHash
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Either (Either(Right), note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Data.Newtype (over, unwrap)
import Data.Show.Generic (genericShow)
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Effect (Effect)
import Effect.Class (liftEffect)
import Helpers (liftEither)
import ProtocolParametersAlonzo (costModels)
import Serialization (hashScriptData, toBytes)
import Serialization.PlutusData as Serialization.PlutusData
import Serialization.Types as Serialization
import Serialization.WitnessSet as Serialization.WitnessSet
import Types.Datum (Datum(Datum))
import Types.Transaction
  ( PlutusScript
  , Redeemer
  , ScriptDataHash(ScriptDataHash)
  , Transaction(Transaction)
  , TransactionWitnessSet
  , TxBody(TxBody)
  )
import Untagged.Union (asOneOf)

data ModifyTxError
  = ConvertWitnessesError
  | ConvertDatumError

derive instance Generic ModifyTxError _
derive instance Eq ModifyTxError

instance Show ModifyTxError where
  show = genericShow

-- | Set the `Transaction` body's script data hash. NOTE: Must include all of
-- | the datums and redeemers for the given transaction
setScriptDataHash
  :: Array Redeemer -> Array Datum -> Transaction -> Effect Transaction
setScriptDataHash rs ds tx@(Transaction { body }) = do
  scriptDataHash <- ScriptDataHash <<< toBytes <<< asOneOf
    <$> hashScriptData rs costModels (unwrap <$> ds)
  pure $ over Transaction
    _
      { body = over TxBody _ { script_data_hash = Just scriptDataHash } body
      }
    tx

-- | Attach a `Datum` to a transaction by modifying its existing witness set.
-- | Fails if either the datum or updated witness set cannot be converted during
-- | (de-)serialization
attachDatum :: Datum -> Transaction -> Effect (Either ModifyTxError Transaction)
attachDatum (Datum pd) tx@(Transaction { witness_set: ws }) = runExceptT $ do
  pd' <- liftEither
    $ note ConvertDatumError
    $ Serialization.PlutusData.convertPlutusData pd
  updateTxWithWitnesses tx
    =<< convertWitnessesWith ws (Serialization.WitnessSet.setPlutusData pd')

-- | Attach a `Redeemer` to a transaction by modifying its existing witness set.
-- | Note that this is the `Types.Transaction` representation of a redeemer and
-- | not a wrapped `PlutusData`.
--
-- | Fails if either the redeemer or updated witness set cannot be converted
-- | during (de-)serialization
attachRedeemer
  :: Redeemer -> Transaction -> Effect (Either ModifyTxError Transaction)
attachRedeemer r tx@(Transaction { witness_set: ws }) = runExceptT $ do
  r' <- liftEffect $ Serialization.WitnessSet.convertRedeemer r
  updateTxWithWitnesses tx
    =<< convertWitnessesWith ws (Serialization.WitnessSet.setRedeemer r')

-- | Attach a `PlutusScript` to a transaction by modifying its existing witness
-- | set
--
-- | Fails if either the script or updated witness set cannot be converted
-- | during (de-)serialization
attachPlutusScript
  :: PlutusScript -> Transaction -> Effect (Either ModifyTxError Transaction)
attachPlutusScript ps tx@(Transaction { witness_set: ws }) = runExceptT $ do
  ps' <- liftEffect $ Serialization.WitnessSet.convertPlutusScript ps
  updateTxWithWitnesses tx
    =<< convertWitnessesWith ws (Serialization.WitnessSet.setPlutusScript ps')

convertWitnessesWith
  :: TransactionWitnessSet
  -> (Serialization.TransactionWitnessSet -> Effect Unit)
  -> ExceptT ModifyTxError Effect TransactionWitnessSet
convertWitnessesWith ws act = do
  ws' <- liftEffect $ Serialization.WitnessSet.convertWitnessSet ws
  liftEffect $ act ws'
  liftEither $ note ConvertWitnessesError
    $ Deserialization.WitnessSet.convertWitnessSet ws'

updateTxWithWitnesses
  :: forall (e :: Type)
   . Transaction
  -> TransactionWitnessSet
  -> ExceptT e Effect Transaction
updateTxWithWitnesses tx ws =
  liftEither $ Right $ over Transaction _ { witness_set = ws } tx
