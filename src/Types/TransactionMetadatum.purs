module Cardano.Types.TransactionMetadatum where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Serialization.Lib
  ( packMapContainer
  , transactionMetadatum_newBytes
  , transactionMetadatum_newInt
  , transactionMetadatum_newList
  , transactionMetadatum_newMap
  , transactionMetadatum_newText
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Serialization.Lib.Internal (packListContainer)
import Cardano.Types.Int (Int) as Int
import Ctl.Internal.Helpers (encodeMap, encodeTagged')
import Data.ByteArray (ByteArray)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Newtype (unwrap)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)

data TransactionMetadatum
  = MetadataMap (Map TransactionMetadatum TransactionMetadatum)
  | MetadataList (Array TransactionMetadatum)
  | Int Int.Int
  | Bytes ByteArray
  | Text String

derive instance Eq TransactionMetadatum
derive instance Ord TransactionMetadatum
derive instance Generic TransactionMetadatum _

instance Show TransactionMetadatum where
  show x = genericShow x

instance EncodeAeson TransactionMetadatum where
  encodeAeson = case _ of
    MetadataMap m -> encodeTagged' "MetadataMap" $ encodeMap m
    MetadataList arr -> encodeTagged' "MetadataList" arr
    Int n -> encodeTagged' "Int" n
    Bytes bytes -> encodeTagged' "Bytes" bytes
    Text string -> encodeTagged' "Text" string

toCsl
  :: TransactionMetadatum -> Csl.TransactionMetadatum
toCsl = case _ of
  MetadataMap mp ->
    transactionMetadatum_newMap $ packMapContainer $ map (toCsl *** toCsl) $
      Map.toUnfoldable mp
  MetadataList l ->
    transactionMetadatum_newList $ packListContainer $ map toCsl l
  Int int -> transactionMetadatum_newInt $ unwrap int
  Bytes bytes -> transactionMetadatum_newBytes bytes
  Text text -> transactionMetadatum_newText text
