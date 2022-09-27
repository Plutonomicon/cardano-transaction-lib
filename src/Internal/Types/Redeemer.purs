module Ctl.Internal.Types.Redeemer
  ( Redeemer(Redeemer)
  , unitRedeemer
  , RedeemerHash(RedeemerHash)
  , redeemerHash
  ) where

import Prelude

import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Serialization.PlutusData (convertPlutusData)
import Ctl.Internal.Serialization.ToBytes (toBytes)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.PlutusData (PlutusData)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Untagged.Union (asOneOf)

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

-- We could also use `type RedeemerHash = DataHash`?
newtype RedeemerHash = RedeemerHash CborBytes

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
redeemerHash :: Redeemer -> Maybe RedeemerHash
redeemerHash =
  map (wrap <<< toBytes <<< asOneOf) <<< convertPlutusData <<< unwrap
