module Contract.AuxiliaryData
  ( setAuxiliaryData
  , setGeneralTxMetadata
  , setTxMetadata
  ) where

import Prelude

import Cardano.Types.Transaction
  ( AuxiliaryData(AuxiliaryData)
  , AuxiliaryDataHash
  )
import Cardano.Types.Transaction
  ( _body
  , _auxiliaryData
  , _auxiliaryDataHash
  ) as Tx
import Data.Maybe (Maybe, fromMaybe)
import Data.Lens (lens', (?~))
import Data.Lens.Getter (view)
import Data.Lens.Types (Lens')
import Data.Tuple (Tuple(Tuple))
import Contract.Monad (Contract)
import Contract.ScriptLookups (UnattachedUnbalancedTx(UnattachedUnbalancedTx))
import Effect.Class (liftEffect)
import Metadata.MetadataType (class MetadataType, toGeneralTxMetadata)
import Serialization.AuxiliaryData (hashAuxiliaryData)
import Types.TransactionMetadata (GeneralTransactionMetadata)
import Types.UnbalancedTransaction (UnbalancedTx, _transaction)

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
