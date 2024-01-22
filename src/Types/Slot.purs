module Cardano.Types.Slot (Slot(Slot)) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Types.BigNum (BigNum)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.ToData (class ToData)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype Slot = Slot BigNum

derive instance Newtype Slot _
derive instance Generic Slot _
derive newtype instance Eq Slot
derive newtype instance Ord Slot
derive newtype instance DecodeAeson Slot
derive newtype instance EncodeAeson Slot
derive newtype instance FromData Slot
derive newtype instance ToData Slot

instance Show Slot where
  show = genericShow
