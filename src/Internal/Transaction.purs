-- TODO: consider moving these utils closer to where they are used
module Ctl.Internal.Transaction
  ( attachDatum
  , attachPlutusScript
  , attachNativeScript
  , setScriptDataHash
  ) where

import Prelude

import Cardano.Serialization.Lib
  ( hashScriptData
  , packListContainer
  , packMapContainer
  )
import Cardano.Types.CostModel (CostModel)
import Cardano.Types.CostModel as CostModel
import Cardano.Types.Language (Language)
import Cardano.Types.Language as Language
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.Redeemer (Redeemer)
import Cardano.Types.Redeemer as Redeemer
import Cardano.Types.ScriptDataHash (ScriptDataHash(ScriptDataHash))
import Cardano.Types.Transaction (Transaction(Transaction))
import Cardano.Types.TransactionBody (TransactionBody(TransactionBody))
import Cardano.Types.TransactionWitnessSet
  ( TransactionWitnessSet(TransactionWitnessSet)
  )
import Data.Array (nub)
import Data.Array as Array
import Data.Foldable (null)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just))
import Data.Newtype (over, unwrap)
import Data.Profunctor.Strong ((***))
import Effect (Effect)
import Literals.Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

-- | Set the `Transaction` body's script data hash. NOTE: Must include *all* of
-- | the datums and redeemers for the given transaction
setScriptDataHash
  :: Map Language CostModel
  -> Array Redeemer
  -> Array PlutusData
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
  , null (unwrap witnessSet).plutusData
  , null rs
  , null ds = pure tx
  | otherwise = do
      let
        costMdlsCsl =
          packMapContainer $ map (Language.toCsl *** CostModel.toCsl) $
            Map.toUnfoldable costModels
        redeemersCsl =
          packListContainer $ Redeemer.toCsl <$> rs
        datumsCsl =
          if Array.null ds
          -- This is a hack. The datums argument is optional and is
          -- supposed to not be provided if there are no datums.
          -- TODO: fix upstream
          then unsafeCoerce undefined
          else packListContainer $ PlutusData.toCsl <$> ds
        scriptDataHash =
          ScriptDataHash $ hashScriptData redeemersCsl costMdlsCsl datumsCsl
      pure $ over Transaction
        _
          { body = over TransactionBody
              _ { scriptDataHash = Just scriptDataHash }
              body
          }
        tx

-- | Attach a `Datum` to a transaction by modifying its existing witness set.
-- | Fails if either the datum or updated witness set cannot be converted during
-- | (de-)serialization
attachDatum :: PlutusData -> Transaction -> Transaction
attachDatum d = attachDatums [ d ]

attachDatums :: Array PlutusData -> Transaction -> Transaction
attachDatums [] tx = tx
attachDatums datums tx@(Transaction { witnessSet: TransactionWitnessSet ws }) =
  updateTxWithWitnesses tx $ TransactionWitnessSet $ ws
    { plutusData = nub $ ws.plutusData <> datums
    }

-- | Attach a `PlutusScript` to a transaction by modifying its existing witness
-- | set
attachPlutusScript :: PlutusScript -> Transaction -> Transaction
attachPlutusScript ps = attachPlutusScripts [ ps ]

attachPlutusScripts :: Array PlutusScript -> Transaction -> Transaction
attachPlutusScripts ps tx@(Transaction { witnessSet: TransactionWitnessSet ws }) =
  updateTxWithWitnesses tx $ TransactionWitnessSet $ ws
    { plutusScripts = nub $ ws.plutusScripts <> ps
    }

-- | Attach a `NativeScript` to a transaction by modifying its existing witness
-- | set
attachNativeScript :: NativeScript -> Transaction -> Transaction
attachNativeScript ns tx@(Transaction { witnessSet: TransactionWitnessSet ws }) =
  updateTxWithWitnesses tx $ TransactionWitnessSet $ ws
    { nativeScripts = nub $ ws.nativeScripts <> [ ns ]
    }

updateTxWithWitnesses :: Transaction -> TransactionWitnessSet -> Transaction
updateTxWithWitnesses tx ws =
  over Transaction _ { witnessSet = ws } tx
