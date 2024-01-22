module Cardano.Types.Pointer where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.Slot (Slot)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype Pointer = Pointer { slot :: Slot, txIndex :: BigNum, certIndex :: BigNum }

derive instance Eq Pointer
derive instance Ord Pointer
derive instance Generic Pointer _
derive instance Newtype Pointer _

instance Show Pointer where
  show = genericShow

derive newtype instance EncodeAeson Pointer
derive newtype instance DecodeAeson Pointer
