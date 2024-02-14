module Cardano.Types.Vkey where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Types.PublicKey (PublicKey(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype Vkey = Vkey PublicKey

derive instance Generic Vkey _
derive instance Newtype Vkey _
derive newtype instance Eq Vkey
derive newtype instance Ord Vkey
derive newtype instance EncodeAeson Vkey

instance Show Vkey where
  show = genericShow
