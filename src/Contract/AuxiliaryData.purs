-- | A module for setting Auxiliary Data or metadata to an `UnbalancedTx`.
module Contract.AuxiliaryData
  ( setAuxiliaryData
  , setGeneralTxMetadata
  , setTxMetadata
  ) where

import Prelude

import Contract.Monad (Contract)
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
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx)
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

setAuxiliaryData
  :: UnbalancedTx
  -> AuxiliaryData
  -> Contract UnbalancedTx
setAuxiliaryData tx auxData = liftEffect do
  auxDataHash <- hashAuxiliaryData auxData
  pure (tx # _auxiliaryData ?~ auxData # _auxiliaryDataHash ?~ auxDataHash)

setGeneralTxMetadata
  :: UnbalancedTx
  -> GeneralTransactionMetadata
  -> Contract UnbalancedTx
setGeneralTxMetadata tx generalMetadata =
  let
    auxData = fromMaybe mempty (view _auxiliaryData tx)
  in
    setAuxiliaryData tx (auxData # _metadata ?~ generalMetadata)

setTxMetadata
  :: forall (m :: Type)
   . MetadataType m
  => UnbalancedTx
  -> m
  -> Contract UnbalancedTx
setTxMetadata tx =
  setGeneralTxMetadata tx <<< toGeneralTxMetadata

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------

_transaction :: Lens' UnbalancedTx Transaction
_transaction = _Newtype <<< prop (Proxy :: Proxy "transaction")

_auxiliaryData :: Lens' UnbalancedTx (Maybe AuxiliaryData)
_auxiliaryData =
  _transaction <<< Tx._auxiliaryData

_auxiliaryDataHash :: Lens' UnbalancedTx (Maybe AuxiliaryDataHash)
_auxiliaryDataHash =
  _transaction <<< Tx._body <<< Tx._auxiliaryDataHash

_metadata :: Lens' AuxiliaryData (Maybe GeneralTransactionMetadata)
_metadata = lens' \(AuxiliaryData rec@{ metadata }) ->
  Tuple metadata \md -> AuxiliaryData rec { metadata = md }
