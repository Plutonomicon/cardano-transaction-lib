module Ctl.Internal.Types.Redeemer
  ( Redeemer(Redeemer)
  , unitRedeemer
  , RedeemerHash(RedeemerHash)
  , redeemerHash
  ) where

import Prelude

import Cardano.FromData (class FromData)
import Cardano.ToData (class ToData, toData)
import Cardano.Types.DataHash (DataHash(..), hashPlutusData)
import Cardano.Types.PlutusData (PlutusData)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)

newtype Redeemer = Redeemer PlutusData

derive instance Generic Redeemer _
derive instance Newtype Redeemer _
derive newtype instance Eq Redeemer
derive newtype instance FromData Redeemer
derive newtype instance Ord Redeemer
derive newtype instance ToData Redeemer

instance Show Redeemer where
  show = genericShow

unitRedeemer :: Redeemer
unitRedeemer = Redeemer (toData unit)

newtype RedeemerHash = RedeemerHash DataHash

derive instance Generic RedeemerHash _
derive instance Newtype RedeemerHash _
derive newtype instance Eq RedeemerHash
derive newtype instance FromData RedeemerHash
derive newtype instance Ord RedeemerHash
derive newtype instance ToData RedeemerHash

instance Show RedeemerHash where
  show = genericShow

-- | Converts Plutus-style `Redeemer` to internal (non-CSL) `RedeemerHash`.
-- | This is a duplicate of `datumHash`.
redeemerHash :: Redeemer -> RedeemerHash
redeemerHash =
  RedeemerHash <<< hashPlutusData <<< unwrap
