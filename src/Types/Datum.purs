module Types.Datum
  ( Datum(..)
  , DatumHash
  , Redeemer(..)
  , RedeemerHash(..)
  , datumHash
  , plutusDatumHash
  , redeemerHash
  , unitDatum
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Serialization (toBytes)
import Serialization.PlutusData (convertPlutusData)
import ToData (toData)
import Types.ByteArray (ByteArray)
import Types.PlutusData (PlutusData)
import Types.Transaction (DataHash)
import Untagged.Union (asOneOf)

-- | Define data types mirroring Plutus `Datum`, like `Datum` itself and
-- | `Redeemer` where the latter is not to be confused with the CSL-stype
-- | `Types.Transaction.Redeemer`.

-- | `Datum` is defined as a newtype of `PlutusData`
newtype Datum = Datum PlutusData

derive newtype instance Eq Datum
derive newtype instance Ord Datum
derive instance Newtype Datum _
derive instance Generic Datum _

instance Show Datum where
  show = genericShow

unitDatum :: Datum
unitDatum = Datum (toData unit)

-- | This `Redeemer` is a `PlutusData` newtype which should be used for contract
-- | related code and not the CSL-style `Redeemer` from `Types.Transaction`.
newtype Redeemer = Redeemer PlutusData

derive newtype instance Eq Redeemer
derive newtype instance Ord Redeemer
derive instance Newtype Redeemer _
derive instance Generic Redeemer _

instance Show Redeemer where
  show = genericShow

-- To help with people copying & pasting code from Haskell to Purescript
type DatumHash = DataHash

instance Show RedeemerHash where
  show = genericShow

-- | Converts Plutus-style `Datum` to internal (non-CSL) `DatumHash`
datumHash :: Datum -> Maybe DatumHash
datumHash = plutusDatumHash

-- We could also use `type RedeemerHash = DataHash`?
newtype RedeemerHash = RedeemerHash ByteArray

derive instance Generic RedeemerHash _
derive instance Newtype RedeemerHash _
derive newtype instance Eq RedeemerHash
derive newtype instance Ord RedeemerHash

-- | Converts Plutus-style `Redeemer` to internal (non-CSL) `RedeemerHash`.
-- | This is a duplicate of `datumHash`.
redeemerHash :: Redeemer -> Maybe RedeemerHash
redeemerHash = plutusDatumHash

plutusDatumHash
  :: forall (m :: Type) (n :: Type)
   . Newtype m PlutusData
  => Newtype n ByteArray
  => m
  -> Maybe n
plutusDatumHash =
  map (wrap <<< toBytes <<< asOneOf) <<< convertPlutusData <<< unwrap
