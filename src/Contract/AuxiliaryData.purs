-- | A module for setting Auxiliary Data or metadata to an `UnbalancedTx`.
module Contract.AuxiliaryData
  ( setAuxiliaryData
  , setGeneralTxMetadata
  , setTxMetadata
  ) where

import Prelude

import Cardano.Types
  ( AuxiliaryData
  , GeneralTransactionMetadata
  , Transaction
  , _auxiliaryData
  , _auxiliaryDataHash
  , _body
  )
import Cardano.Types.AuxiliaryData (hashAuxiliaryData)
import Ctl.Internal.Metadata.MetadataType
  ( class MetadataType
  , toGeneralTxMetadata
  )
import Data.Lens ((.~), (?~))
import Data.Lens.Getter (view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just), fromMaybe)
import Type.Proxy (Proxy(Proxy))

setAuxiliaryData
  :: Transaction
  -> AuxiliaryData
  -> Transaction
setAuxiliaryData tx auxData =
  tx # _auxiliaryData .~ Just auxData
    # _body <<< _auxiliaryDataHash ?~ hashAuxiliaryData auxData

setGeneralTxMetadata
  :: Transaction
  -> GeneralTransactionMetadata
  -> Transaction
setGeneralTxMetadata tx generalMetadata =
  let
    auxData = view _auxiliaryData tx
  in
    setAuxiliaryData tx
      ( fromMaybe mempty auxData #
          _Newtype <<< prop (Proxy :: Proxy "metadata") ?~ generalMetadata
      )

setTxMetadata
  :: forall (m :: Type)
   . MetadataType m
  => Transaction
  -> m
  -> Transaction
setTxMetadata tx =
  setGeneralTxMetadata tx <<< toGeneralTxMetadata
