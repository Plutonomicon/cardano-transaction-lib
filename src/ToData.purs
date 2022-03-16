module ToData
  ( class ToData
  , toData
  ) where

import Prelude

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Profunctor.Strong ((***))
import Data.Tuple.Nested (type (/\), (/\))
import Prim.TypeError (class Fail, Text)
import Serialization.Hash (Ed25519KeyHash, ed25519KeyHashToBytes)
import Types.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Types.PlutusData (PlutusData(Integer, List, Map, Bytes))

class ToData (a :: Type) where
  toData :: a -> PlutusData

instance ToData Unit where
  toData _ = List []

instance ToData Boolean where
  toData false = Integer (BigInt.fromInt 0)
  toData true = Integer (BigInt.fromInt 1)

instance Fail (Text "Int is not supported, use BigInt instead") => ToData Int where
  toData = toData <<< BigInt.fromInt

instance ToData BigInt where
  toData = Integer

instance ToData a => ToData (Array a) where
  toData = List <<< map toData

instance ToData a => ToData (List a) where
  toData = foldableToPlutusData

instance (ToData a, ToData b) => ToData (a /\ b) where
  toData (a /\ b) = List [ toData a, toData b ]

instance (ToData k, ToData v) => ToData (Map k v) where
  toData mp = Map $ entries # map (toData *** toData) # Map.fromFoldable
    where
    entries = Map.toUnfoldable mp :: Array (k /\ v)

instance ToData ByteArray where
  toData = Bytes

-- | This covers `Bech32` which is just a type alias for `String`
instance ToData String where
  toData = Bytes <<< hexToByteArrayUnsafe -- unsafe, maybe there's a better way.

instance ToData Ed25519KeyHash where
  toData = Bytes <<< ed25519KeyHashToBytes

foldableToPlutusData :: forall (a :: Type) (t :: Type -> Type). Foldable t => ToData a => t a -> PlutusData
foldableToPlutusData = Array.fromFoldable >>> map toData >>> List
