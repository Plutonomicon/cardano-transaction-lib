module Ctl.Internal.Serialization.AuxiliaryData
  ( convertAuxiliaryData
  , hashAuxiliaryData
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction
  ( AuxiliaryData(AuxiliaryData)
  , AuxiliaryDataHash
  ) as T
import Ctl.Internal.FfiHelpers (ContainerHelper, containerHelper)
import Ctl.Internal.Helpers (fromJustEff)
import Ctl.Internal.Serialization.NativeScript (convertNativeScripts)
import Ctl.Internal.Serialization.PlutusScript (convertPlutusScript)
import Ctl.Internal.Serialization.Types
  ( AuxiliaryData
  , GeneralTransactionMetadata
  , NativeScripts
  , PlutusScripts
  , TransactionMetadatum
  )
import Ctl.Internal.Serialization.WitnessSet (addPlutusScript, newPlutusScripts)
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.BigNum (fromBigInt) as BigNum
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.Int as Int
import Ctl.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  , TransactionMetadatum(Text, Bytes, Int, MetadataList, MetadataMap)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  ) as T
import Data.Map as Map
import Data.Newtype (wrap)
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)

foreign import newAuxiliaryData :: Effect AuxiliaryData

foreign import setAuxiliaryDataNativeScripts
  :: AuxiliaryData -> NativeScripts -> Effect Unit

foreign import setAuxiliaryDataPlutusScripts
  :: AuxiliaryData -> PlutusScripts -> Effect Unit

foreign import setAuxiliaryDataGeneralTransactionMetadata
  :: AuxiliaryData -> GeneralTransactionMetadata -> Effect Unit

foreign import newGeneralTransactionMetadata
  :: ContainerHelper
  -> Array (BigNum /\ TransactionMetadatum)
  -> Effect GeneralTransactionMetadata

foreign import newMetadataMap
  :: ContainerHelper
  -> Array (TransactionMetadatum /\ TransactionMetadatum)
  -> Effect TransactionMetadatum

foreign import newMetadataList
  :: ContainerHelper
  -> Array TransactionMetadatum
  -> Effect TransactionMetadatum

foreign import newMetadataInt
  :: Int.Int -> Effect TransactionMetadatum

foreign import newMetadataBytes
  :: ByteArray -> Effect TransactionMetadatum

foreign import newMetadataText
  :: String -> Effect TransactionMetadatum

foreign import _hashAuxiliaryData
  :: AuxiliaryData -> ByteArray

hashAuxiliaryData :: T.AuxiliaryData -> Effect T.AuxiliaryDataHash
hashAuxiliaryData =
  map (wrap <<< wrap <<< _hashAuxiliaryData) <<< convertAuxiliaryData

convertAuxiliaryData :: T.AuxiliaryData -> Effect AuxiliaryData
convertAuxiliaryData
  (T.AuxiliaryData { metadata, nativeScripts, plutusScripts }) = do
  ad <- newAuxiliaryData
  for_ metadata $
    convertGeneralTransactionMetadata >=>
      setAuxiliaryDataGeneralTransactionMetadata ad
  for_ nativeScripts $
    ( fromJustEff "convertAuxiliaryData: failed to convert NativeScripts"
        <<< convertNativeScripts
    ) >=> setAuxiliaryDataNativeScripts ad
  for_ plutusScripts \ps -> do
    scripts <- newPlutusScripts
    for_ ps (convertPlutusScript >>> addPlutusScript scripts)
    setAuxiliaryDataPlutusScripts ad scripts
  pure ad

convertGeneralTransactionMetadata
  :: T.GeneralTransactionMetadata -> Effect GeneralTransactionMetadata
convertGeneralTransactionMetadata (T.GeneralTransactionMetadata mp) = do
  newGeneralTransactionMetadata containerHelper =<<
    for (Map.toUnfoldable mp)
      \(T.TransactionMetadatumLabel l /\ d) -> do
        label <-
          fromJustEff
            "convertGeneralTransactionMetadata: failing to convert MetadataumLabel"
            (BigNum.fromBigInt l)
        datum <- convertTransactionMetadatum d
        pure $ label /\ datum

convertTransactionMetadatum
  :: T.TransactionMetadatum -> Effect TransactionMetadatum
convertTransactionMetadatum = case _ of
  T.MetadataMap mp ->
    newMetadataMap containerHelper =<< for (Map.toUnfoldable mp) \(k /\ v) -> do
      Tuple <$> convertTransactionMetadatum k <*> convertTransactionMetadatum v
  T.MetadataList l ->
    newMetadataList containerHelper =<<
      traverse convertTransactionMetadatum l
  T.Int int -> newMetadataInt int
  T.Bytes bytes -> newMetadataBytes bytes
  T.Text text -> newMetadataText text
