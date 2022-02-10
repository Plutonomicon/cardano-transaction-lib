module Types.RedeemerTag where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- lives in it's own module due to a name conflict with the `Mint` Type
data RedeemerTag = Spend | Mint | Cert | Reward

derive instance Generic RedeemerTag _
derive instance Eq RedeemerTag

instance Show RedeemerTag where
  show = genericShow
