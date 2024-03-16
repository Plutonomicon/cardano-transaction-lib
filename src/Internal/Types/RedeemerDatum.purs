module Ctl.Internal.Types.RedeemerDatum where

import Prelude

import Cardano.FromData (class FromData)
import Cardano.ToData (class ToData, toData)
import Cardano.Types.PlutusData (PlutusData)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Prelude as Prelude

-- | Redeemer without ExUnits, tag or index - just a plain wrapper over `PlutusData`
newtype RedeemerDatum = RedeemerDatum PlutusData

derive instance Generic RedeemerDatum _
derive instance Newtype RedeemerDatum _
derive newtype instance Eq RedeemerDatum
derive newtype instance FromData RedeemerDatum
derive newtype instance Ord RedeemerDatum
derive newtype instance ToData RedeemerDatum

instance Show RedeemerDatum where
  show = genericShow

unit :: RedeemerDatum
unit = RedeemerDatum (toData Prelude.unit)
