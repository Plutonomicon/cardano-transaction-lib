module CTL.Internal.Transaction
  ( ModifyTxError(ConvertWitnessesError, ConvertDatumError)
  , attachDatum
  , attachRedeemer
  , attachPlutusScript
  , attachNativeScript
  , setScriptDataHash
  ) where

import Prelude

import CTL.Internal.Cardano.Types.Transaction
  ( Costmdls
  , Redeemer
  , ScriptDataHash(ScriptDataHash)
  , Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , TxBody(TxBody)
  )
import CTL.Internal.Cardano.Types.NativeScript (NativeScript)

import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Array as Array
import Data.Either (Either(Right), note)
import Data.Foldable (null)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Data.Newtype (over, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import CTL.Internal.Deserialization.WitnessSet as Deserialization.WitnessSet
import Effect (Effect)
import Effect.Class (liftEffect)
import CTL.Internal.Helpers (liftEither)
import CTL.Internal.Serialization (hashScriptData, toBytes)
import CTL.Internal.Serialization.PlutusData as Serialization.PlutusData
import CTL.Internal.Serialization.PlutusScript as Serialization.PlutusScript
import CTL.Internal.Serialization.Types as Serialization
import CTL.Internal.Serialization.WitnessSet as Serialization.WitnessSet
import CTL.Internal.Types.Datum (Datum)
import CTL.Internal.Types.Scripts (PlutusScript)
import Untagged.Union (asOneOf)

data ModifyTxError
  = ConvertWitnessesError
  | ConvertDatumError

derive instance Generic ModifyTxError _
derive instance Eq ModifyTxError

instance Show ModifyTxError where
  show = genericShow

-- | Set the `Transaction` body's script data hash. NOTE: Must include *all* of
-- | the datums and redeemers for the given transaction
setScriptDataHash
  :: Costmdls
  -> Array Redeemer
  -> Array Datum
  -> Transaction
  -> Effect Transaction
setScriptDataHash costModels rs ds tx@(Transaction { body, witnessSet })
  -- No hash should be set if *all* of the following hold:
  --
  --   * there are no scripts
  --   * there are no redeemers
  --   * there are no datums
  --
  | null (unwrap witnessSet).plutusScripts
  , null rs
  , null ds = pure tx
  | otherwise = do
      scriptDataHash <- ScriptDataHash <<< toBytes <<< asOneOf
        <$> hashScriptData costModels rs (unwrap <$> ds)
      pure $ over Transaction
        _
          { body = over TxBody _ { scriptDataHash = Just scriptDataHash } body
          }
        tx

-- | Attach a `Datum` to a transaction by modifying its existing witness set.
-- | Fails if either the datum or updated witness set cannot be converted during
-- | (de-)serialization
attachDatum :: Datum -> Transaction -> Effect (Either ModifyTxError Transaction)
attachDatum d = runExceptT <<< attachDatums (Array.singleton d)

attachDatums
  :: Array Datum -> Transaction -> ExceptT ModifyTxError Effect Transaction
attachDatums [] tx = liftEither $ Right tx
attachDatums datums tx@(Transaction { witnessSet: ws }) = do
  ds <- traverse
    ( liftEither
        <<< note ConvertDatumError
        <<< Serialization.PlutusData.convertPlutusData
        <<< unwrap
    )
    datums
  updateTxWithWitnesses tx
    =<< convertWitnessesWith ws (Serialization.WitnessSet.setPlutusData ds)

-- | Attach a `Redeemer` to a transaction by modifying its existing witness set.
-- | Note that this is the `Types.Transaction` representation of a redeemer and
-- | not a wrapped `PlutusData`.
--
-- | Fails if either the redeemer or updated witness set cannot be converted
-- | during (de-)serialization
attachRedeemer
  :: Redeemer -> Transaction -> Effect (Either ModifyTxError Transaction)
attachRedeemer r = runExceptT <<< attachRedeemers (Array.singleton r)

attachRedeemers
  :: Array Redeemer -> Transaction -> ExceptT ModifyTxError Effect Transaction
attachRedeemers rs tx@(Transaction { witnessSet: ws }) = do
  rs' <- liftEffect $ traverse Serialization.WitnessSet.convertRedeemer rs
  updateTxWithWitnesses tx
    =<< convertWitnessesWith ws (Serialization.WitnessSet.setRedeemers rs')

-- | Attach a `PlutusScript` to a transaction by modifying its existing witness
-- | set
-- |
-- | Fails if either the script or updated witness set cannot be converted
-- | during (de-)serialization
attachPlutusScript
  :: PlutusScript -> Transaction -> Effect (Either ModifyTxError Transaction)
attachPlutusScript ps = runExceptT <<< attachPlutusScripts (Array.singleton ps)

attachPlutusScripts
  :: Array PlutusScript
  -> Transaction
  -> ExceptT ModifyTxError Effect Transaction
attachPlutusScripts ps tx@(Transaction { witnessSet: ws }) = do
  let ps' = ps # map Serialization.PlutusScript.convertPlutusScript
  updateTxWithWitnesses tx
    =<< convertWitnessesWith ws (Serialization.WitnessSet.setPlutusScripts ps')

attachNativeScript
  :: NativeScript -> Transaction -> Effect (Either ModifyTxError Transaction)
attachNativeScript ns tx = do
  runExceptT $ updateTxWithWitnesses tx $
    mempty # over TransactionWitnessSet _ { nativeScripts = Just [ ns ] }

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
updateTxWithWitnesses tx@(Transaction t) ws =
  liftEither $ Right $ over Transaction _ { witnessSet = t.witnessSet <> ws } tx
