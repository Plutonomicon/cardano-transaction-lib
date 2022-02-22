module Types.PlutusData
  ( BuiltinPlutusData(..)
  , Datum(..)
  , DatumHash
  , PlutusData(..)
  , class FromPlutusData
  , class ToPlutusData
  , fromBuiltinPlutusData
  , toBuiltinPlutusData
  , unitRedeemer
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Types.ByteArray (ByteArray)
import Types.RedeemerTag (RedeemerTag)
import Types.Transaction (DataHash, Redeemer)
import Undefined (undefined)

data PlutusData
  = Constr BigInt (Array PlutusData)
  | Map (Map PlutusData PlutusData)
  | List (Array PlutusData)
  | Integer BigInt
  | Bytes ByteArray

derive instance Eq PlutusData
derive instance Ord PlutusData
derive instance Generic PlutusData _

instance Show PlutusData where
  show x = genericShow x

-- On-chain PlutusData. Do we need this representation to separate when writing
-- the Contract Monad?
newtype BuiltinPlutusData = BuiltinPlutusData PlutusData

derive newtype instance Eq BuiltinPlutusData
derive newtype instance Ord BuiltinPlutusData
derive instance Newtype BuiltinPlutusData _
derive instance Generic BuiltinPlutusData _

instance Show BuiltinPlutusData where
  show = genericShow

class ToPlutusData (a :: Type) where
  toBuiltinPlutusData :: a -> BuiltinPlutusData

-- FIX ME (create an issue unless someone notices simple solution to this in PR
-- review)
unitRedeemer :: RedeemerTag -> Redeemer
unitRedeemer = undefined

class FromPlutusData (a :: Type) where
  -- | Convert a value from 'BuiltinPlutusData', returning 'Nothing' if this fails.
  fromBuiltinPlutusData :: BuiltinPlutusData -> Maybe a

newtype Datum = Datum BuiltinPlutusData

derive newtype instance Eq Datum
derive newtype instance Ord Datum
derive instance Newtype Datum _
derive instance Generic Datum _

instance showDatum :: Show Datum where
  show = genericShow

-- Do we want a newtype?
type DatumHash = DataHash