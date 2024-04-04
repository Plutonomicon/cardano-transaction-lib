-- | A module for setting Auxiliary Data or metadata to an `UnbalancedTx`.
module Contract.AuxiliaryData
  ( setAuxiliaryData
  , setGeneralTxMetadata
  , setTxMetadata
  ) where

import Prelude

import Cardano.Types
  ( AuxiliaryData(AuxiliaryData)
  , AuxiliaryDataHash
  , GeneralTransactionMetadata
  , Transaction
  )
import Cardano.Types.AuxiliaryData (hashAuxiliaryData)
import Ctl.Internal.Lens (_auxiliaryData, _auxiliaryDataHash, _body) as Tx
import Ctl.Internal.Metadata.MetadataType
  ( class MetadataType
  , toGeneralTxMetadata
  )
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx)
import Data.Lens (lens', (.~), (?~))
import Data.Lens.Getter (view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Type.Proxy (Proxy(Proxy))

setAuxiliaryData
  :: UnbalancedTx
  -> AuxiliaryData
  -> UnbalancedTx
setAuxiliaryData tx auxData =
  let
    auxDataHash = hashAuxiliaryData auxData
  in
    tx # _auxiliaryData .~ Just auxData
      # _auxiliaryDataHash ?~ auxDataHash

setGeneralTxMetadata
  :: UnbalancedTx
  -> GeneralTransactionMetadata
  -> UnbalancedTx
setGeneralTxMetadata tx generalMetadata =
  let
    auxData = view _auxiliaryData tx
  in
    setAuxiliaryData tx
      (fromMaybe mempty auxData # _metadata ?~ generalMetadata)

setTxMetadata
  :: forall (m :: Type)
   . MetadataType m
  => UnbalancedTx
  -> m
  -> UnbalancedTx
setTxMetadata tx =
  setGeneralTxMetadata tx <<< toGeneralTxMetadata

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------

_transaction :: Lens' UnbalancedTx Transaction
_transaction = _Newtype <<< prop (Proxy :: Proxy "transaction")

_auxiliaryData :: Lens' UnbalancedTx (Maybe AuxiliaryData)
_auxiliaryData =
  _Newtype <<< prop (Proxy :: Proxy "transaction") <<< Tx._auxiliaryData

_auxiliaryDataHash :: Lens' UnbalancedTx (Maybe AuxiliaryDataHash)
_auxiliaryDataHash =
  _transaction <<< Tx._body <<< Tx._auxiliaryDataHash

_metadata :: Lens' AuxiliaryData (Maybe GeneralTransactionMetadata)
_metadata = lens' \(AuxiliaryData rec@{ metadata }) ->
  Tuple metadata \md -> AuxiliaryData rec { metadata = md }
