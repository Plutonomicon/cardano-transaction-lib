module Ctl.Internal.Transaction
  ( attachDatum
  , attachRedeemer
  , attachPlutusScript
  , attachNativeScript
  , setScriptDataHash
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.NativeScript (NativeScript)
import Ctl.Internal.Cardano.Types.Transaction
  ( Costmdls
  , Redeemer
  , ScriptDataHash(ScriptDataHash)
  , Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , TxBody(TxBody)
  )
import Ctl.Internal.Deserialization.WitnessSet as Deserialization.WitnessSet
import Ctl.Internal.Serialization (hashScriptData, toBytes)
import Ctl.Internal.Serialization.PlutusData as Serialization.PlutusData
import Ctl.Internal.Serialization.PlutusScript as Serialization.PlutusScript
import Ctl.Internal.Serialization.Types as Serialization
import Ctl.Internal.Serialization.WitnessSet as Serialization.WitnessSet
import Ctl.Internal.Types.Datum (Datum)
import Ctl.Internal.Types.Scripts (PlutusScript)
import Data.Array as Array
import Data.Foldable (null)
import Data.Maybe (Maybe(Just))
import Data.Newtype (over, unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)

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
      scriptDataHash <- ScriptDataHash <<< unwrap <<< toBytes
        <$> hashScriptData costModels rs (unwrap <$> ds)
      pure $ over Transaction
        _
          { body = over TxBody _ { scriptDataHash = Just scriptDataHash } body
          }
        tx

-- | Attach a `Datum` to a transaction by modifying its existing witness set.
-- | Fails if either the datum or updated witness set cannot be converted during
-- | (de-)serialization
attachDatum :: Datum -> Transaction -> Effect Transaction
attachDatum d = attachDatums (Array.singleton d)

attachDatums
  :: Array Datum -> Transaction -> Effect Transaction
attachDatums [] tx = pure tx
attachDatums datums tx@(Transaction { witnessSet: ws }) = do
  let ds = map (Serialization.PlutusData.convertPlutusData <<< unwrap) datums
  updateTxWithWitnesses tx
    <$> convertWitnessesWith ws (Serialization.WitnessSet.setPlutusData ds)

-- | Attach a `Redeemer` to a transaction by modifying its existing witness set.
-- | Note that this is the `Types.Transaction` representation of a redeemer and
-- | not a wrapped `PlutusData`.
--
-- | Fails if either the redeemer or updated witness set cannot be converted
-- | during (de-)serialization
attachRedeemer
  :: Redeemer -> Transaction -> Effect Transaction
attachRedeemer r = attachRedeemers (Array.singleton r)

attachRedeemers
  :: Array Redeemer -> Transaction -> Effect Transaction
attachRedeemers rs tx@(Transaction { witnessSet: ws }) = do
  rs' <- liftEffect $ traverse Serialization.WitnessSet.convertRedeemer rs
  updateTxWithWitnesses tx
    <$> convertWitnessesWith ws (Serialization.WitnessSet.setRedeemers rs')

-- | Attach a `PlutusScript` to a transaction by modifying its existing witness
-- | set
-- |
-- | Fails if either the script or updated witness set cannot be converted
-- | during (de-)serialization
attachPlutusScript
  :: PlutusScript -> Transaction -> Effect Transaction
attachPlutusScript ps = attachPlutusScripts (Array.singleton ps)

attachPlutusScripts
  :: Array PlutusScript
  -> Transaction
  -> Effect Transaction
attachPlutusScripts ps tx@(Transaction { witnessSet: ws }) = do
  let ps' = ps # map Serialization.PlutusScript.convertPlutusScript
  updateTxWithWitnesses tx
    <$> convertWitnessesWith ws (Serialization.WitnessSet.setPlutusScripts ps')

attachNativeScript
  :: NativeScript -> Transaction -> Transaction
attachNativeScript ns tx = do
  updateTxWithWitnesses tx $
    mempty # over TransactionWitnessSet _ { nativeScripts = Just [ ns ] }

convertWitnessesWith
  :: TransactionWitnessSet
  -> (Serialization.TransactionWitnessSet -> Effect Unit)
  -> Effect TransactionWitnessSet
convertWitnessesWith ws act = do
  ws' <- liftEffect $ Serialization.WitnessSet.convertWitnessSet ws
  liftEffect $ act ws'
  pure $ Deserialization.WitnessSet.convertWitnessSet ws'

updateTxWithWitnesses
  :: Transaction
  -> TransactionWitnessSet
  -> Transaction
updateTxWithWitnesses tx@(Transaction t) ws =
  over Transaction _ { witnessSet = t.witnessSet <> ws } tx
