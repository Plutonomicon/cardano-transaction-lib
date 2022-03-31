module Types.Datum
  ( Datum(..)
  , unitDatum
  , module X
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import FromData (class FromData)
import Serialization (toBytes)
import Serialization.PlutusData (convertPlutusData)
import ToData (class ToData, toData)
import Types.PlutusData (PlutusData)
import Types.Transaction (DatumHash)
import Types.Transaction (DatumHash) as X
import Untagged.Union (asOneOf)

-- | Define data types mirroring Plutus `Datum`, like `Datum` itself and
-- | `Redeemer` where the latter is not to be confused with the CSL-stype
-- | `Types.Transaction.Redeemer`.

-- | `Datum` is defined as a newtype of `PlutusData`
newtype Datum = Datum PlutusData

derive instance Newtype Datum _
derive instance Generic Datum _
derive newtype instance Eq Datum
derive newtype instance FromData Datum
derive newtype instance Ord Datum
derive newtype instance ToData Datum

instance Show Datum where
  show = genericShow

unitDatum :: Datum
unitDatum = Datum (toData unit)
