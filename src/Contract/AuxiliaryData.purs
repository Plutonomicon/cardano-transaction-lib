module CTL.Contract.AuxiliaryData
  ( setAuxiliaryData
  , setGeneralTxMetadata
  , setTxMetadata
  ) where

import Prelude

import CTL.Contract.Monad (Contract)
import CTL.Contract.ScriptLookups
  ( UnattachedUnbalancedTx(UnattachedUnbalancedTx)
  )
import CTL.Internal.Cardano.Types.Transaction
  ( AuxiliaryData(AuxiliaryData)
  , AuxiliaryDataHash
  )
import CTL.Internal.Cardano.Types.Transaction
  ( _auxiliaryData
  , _auxiliaryDataHash
  , _body
  ) as Tx
import CTL.Internal.Metadata.MetadataType
  ( class MetadataType
  , toGeneralTxMetadata
  )
import CTL.Internal.Serialization.AuxiliaryData (hashAuxiliaryData)
import CTL.Internal.Types.TransactionMetadata (GeneralTransactionMetadata)
import CTL.Internal.Types.UnbalancedTransaction (UnbalancedTx, _transaction)
import Data.Lens (lens', (?~))
import Data.Lens.Getter (view)
import Data.Lens.Types (Lens')
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Effect.Class (liftEffect)

-- These functions involve `UnattachedUnbalancedTx` which in turns involve
-- `UnbalancedTx`, these involve `ScriptOutput` which is what is currently
-- being used in more up-to-date Plutus code (as opposed to `TransactionOutput`).
-- Therefore, we won't provide any conversion. It is worth noting
-- `UnattachedUnbalancedTx` also includes Cardano-style Redeemers, although
-- I don't think there's a way around this because they need to be reattached
-- later on - see Types.ScriptLookups for more detail.

setAuxiliaryData
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> AuxiliaryData
  -> Contract r UnattachedUnbalancedTx
setAuxiliaryData tx auxData = liftEffect do
  auxDataHash <- hashAuxiliaryData auxData
  pure (tx # _auxiliaryData ?~ auxData # _auxiliaryDataHash ?~ auxDataHash)

setGeneralTxMetadata
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> GeneralTransactionMetadata
  -> Contract r UnattachedUnbalancedTx
setGeneralTxMetadata tx generalMetadata =
  let
    auxData = fromMaybe mempty (view _auxiliaryData tx)
  in
    setAuxiliaryData tx (auxData # _metadata ?~ generalMetadata)

setTxMetadata
  :: forall (r :: Row Type) (m :: Type)
   . MetadataType m
  => UnattachedUnbalancedTx
  -> m
  -> Contract r UnattachedUnbalancedTx
setTxMetadata tx =
  setGeneralTxMetadata tx <<< toGeneralTxMetadata

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------

_unbalancedTx :: Lens' UnattachedUnbalancedTx UnbalancedTx
_unbalancedTx = lens' \(UnattachedUnbalancedTx rec@{ unbalancedTx }) ->
  Tuple unbalancedTx \ubTx -> UnattachedUnbalancedTx rec { unbalancedTx = ubTx }

_auxiliaryData :: Lens' UnattachedUnbalancedTx (Maybe AuxiliaryData)
_auxiliaryData =
  _unbalancedTx <<< _transaction <<< Tx._auxiliaryData

_auxiliaryDataHash :: Lens' UnattachedUnbalancedTx (Maybe AuxiliaryDataHash)
_auxiliaryDataHash =
  _unbalancedTx <<< _transaction <<< Tx._body <<< Tx._auxiliaryDataHash

_metadata :: Lens' AuxiliaryData (Maybe GeneralTransactionMetadata)
_metadata = lens' \(AuxiliaryData rec@{ metadata }) ->
  Tuple metadata \md -> AuxiliaryData rec { metadata = md }
