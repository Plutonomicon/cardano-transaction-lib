module Types.PlutusData
  ( PlutusData(Constr, Map, List, Integer, Bytes)
  ) where

import Prelude

import Types.ByteArray (ByteArray)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data PlutusData
  = Constr BigInt (Array PlutusData)
  | Map PlutusData PlutusData
  | List (Array PlutusData)
  | Integer BigInt
  | Bytes ByteArray

derive instance Eq PlutusData
derive instance Ord PlutusData
derive instance Generic PlutusData _

instance Show PlutusData where
  show x = genericShow x
