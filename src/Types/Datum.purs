module Types.Datum
  ( Datum(..)
  , unitDatum
  , module X
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import FromData (class FromData)
import ToData (class ToData, toData)
import Types.PlutusData (PlutusData)
import Types.Transaction (DataHash(DataHash)) as X

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
derive newtype instance DecodeAeson Datum
derive newtype instance EncodeAeson Datum

instance Show Datum where
  show = genericShow

unitDatum :: Datum
unitDatum = Datum (toData unit)
