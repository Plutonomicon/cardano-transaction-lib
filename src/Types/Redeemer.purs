module Types.Redeemer
  ( Redeemer(..)
  , unitRedeemer
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Types.PlutusData (PlutusData)
import ToData (toData)

newtype Redeemer = Redeemer PlutusData

derive instance Generic Redeemer _
derive newtype instance Eq Redeemer

instance Show Redeemer where
  show = genericShow

unitRedeemer :: Redeemer
unitRedeemer = Redeemer (toData unit)
