module Contract.AuxiliaryData
  ( setAuxiliaryData
  , setGeneralTxMetadata
  , setTxMetadata
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.ScriptLookups (UnattachedUnbalancedTx)
import Ctl.Internal.Cardano.Types.Transaction
  ( AuxiliaryData(AuxiliaryData)
  , AuxiliaryDataHash
  , Transaction
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
import Data.Lens (lens', (?~))
import Data.Lens.Getter (view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Effect.Class (liftEffect)
import Type.Proxy (Proxy(Proxy))

-- These functions involve `UnattachedUnbalancedTx`,
-- which in turn involve `UnbalancedTx`. These functions involve ScriptOutput,
-- which is the type currently being used in more recent Plutus code (as opposed to `TransactionOutput`).
-- As a result, no conversion will be provided.
-- It is worth noting that `UnattachedUnbalancedTx` also includes Cardano-style Redeemers,
-- which must be reattached later on (see Types.ScriptLookups for more information).
-- There does not appear to be a way around this.

setAuxiliaryData
  :: UnattachedUnbalancedTx
  -> AuxiliaryData
  -> Contract UnattachedUnbalancedTx
setAuxiliaryData tx auxData = liftEffect do
  auxDataHash <- hashAuxiliaryData auxData
  pure (tx # _auxiliaryData ?~ auxData # _auxiliaryDataHash ?~ auxDataHash)

setGeneralTxMetadata
  :: UnattachedUnbalancedTx
  -> GeneralTransactionMetadata
  -> Contract UnattachedUnbalancedTx
setGeneralTxMetadata tx generalMetadata =
  let
    auxData = fromMaybe mempty (view _auxiliaryData tx)
  in
    setAuxiliaryData tx (auxData # _metadata ?~ generalMetadata)

setTxMetadata
  :: forall (m :: Type)
   . MetadataType m
  => UnattachedUnbalancedTx
  -> m
  -> Contract UnattachedUnbalancedTx
setTxMetadata tx =
  setGeneralTxMetadata tx <<< toGeneralTxMetadata

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------

_unbalancedTx :: Lens' UnattachedUnbalancedTx Transaction
_unbalancedTx = _Newtype <<< prop (Proxy :: Proxy "transaction")

_auxiliaryData :: Lens' UnattachedUnbalancedTx (Maybe AuxiliaryData)
_auxiliaryData =
  _unbalancedTx <<< Tx._auxiliaryData

_auxiliaryDataHash :: Lens' UnattachedUnbalancedTx (Maybe AuxiliaryDataHash)
_auxiliaryDataHash =
  _unbalancedTx <<< Tx._body <<< Tx._auxiliaryDataHash

_metadata :: Lens' AuxiliaryData (Maybe GeneralTransactionMetadata)
_metadata = lens' \(AuxiliaryData rec@{ metadata }) ->
  Tuple metadata \md -> AuxiliaryData rec { metadata = md }
