module Contract.AuxiliaryData
  ( setAuxiliaryData
  , setGeneralTxMetadata
  , setTxMetadata
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.ScriptLookups
  ( UnattachedUnbalancedTx(UnattachedUnbalancedTx)
  )
import Ctl.Internal.Cardano.Types.Transaction
  ( AuxiliaryData(AuxiliaryData)
  , AuxiliaryDataHash
  )
import Ctl.Internal.Cardano.Types.Transaction
  ( _auxiliaryData
  , _auxiliaryDataHash
  , _body
  ) as Tx
import Ctl.Internal.Metadata.MetadataType
  ( class MetadataType
  , toGeneralTxMetadata
  )
import Ctl.Internal.Serialization.AuxiliaryData (hashAuxiliaryData)
import Ctl.Internal.Types.TransactionMetadata (GeneralTransactionMetadata)
import Ctl.Internal.Types.UnbalancedTransaction (UnbalancedTx, _transaction)
import Data.Lens (lens', (?~))
import Data.Lens.Getter (view)
import Data.Lens.Types (Lens')
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Effect.Class (liftEffect)

-- These functions involve `UnattachedUnbalancedTx`,
-- which in turn involve `UnbalancedTx`. These functions involve ScriptOutput,
-- which is the type currently being used in more recent Plutus code (as opposed to `TransactionOutput`).
-- As a result, no conversion will be provided.
-- It is worth noting that `UnattachedUnbalancedTx` also includes Cardano-style Redeemers,
-- which must be reattached later on (see Types.ScriptLookups for more information).
-- There does not appear to be a way around this.

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
