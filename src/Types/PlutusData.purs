module Types.PlutusData
  ( Datum(..)
  , DatumHash
  , PlutusData(..)
  , RedeemerHash(..)
  , class FromData
  , class ToData
  , fromData
  , toData
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

-- Don't distinguish "BuiltinData" and "Data" like Plutus:
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

class ToData (a :: Type) where
  toData :: a -> PlutusData

-- FIX ME (create an issue unless someone notices simple solution to this in PR
-- review)
unitRedeemer :: RedeemerTag -> Redeemer
unitRedeemer = undefined

class FromData (a :: Type) where
  -- | Convert a value from `PlutusData`, returning `Nothing` if this fails.
  fromData :: PlutusData -> Maybe a

newtype Datum = Datum PlutusData

derive newtype instance Eq Datum
derive newtype instance Ord Datum
derive instance Newtype Datum _
derive instance Generic Datum _

instance showDatum :: Show Datum where
  show = genericShow

-- To help with people copying & pasting code from Haskell to Purescript
type DatumHash = DataHash

newtype RedeemerHash = RedeemerHash ByteArray

derive instance Generic RedeemerHash _
derive instance Newtype RedeemerHash _
derive newtype instance Eq RedeemerHash
derive newtype instance Ord RedeemerHash

instance Show RedeemerHash where
  show = genericShow