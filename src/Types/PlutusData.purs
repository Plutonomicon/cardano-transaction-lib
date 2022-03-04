module Types.PlutusData
  ( PlutusData(..)
  , class FromData
  , class ToData
  , fromData
  , toData
  )
  where

import Prelude

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Types.ByteArray (ByteArray)
import Types.RedeemerTag (RedeemerTag)
import Undefined (undefined)

-- | Don't distinguish "BuiltinData" and "Data" like Plutus:
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

-- | Don't distinguish "BuiltinData" and "Data" like Plutus:
class ToData (a :: Type) where
  toData :: a -> PlutusData

-- | Don't distinguish "BuiltinData" and "Data" like Plutus:
class FromData (a :: Type) where
  -- | Convert a value from `PlutusData`, returning `Nothing` if this fails.
  fromData :: PlutusData -> Maybe a
