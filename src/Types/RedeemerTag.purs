module Types.RedeemerTag
  ( RedeemerTag(Spend, Mint, Cert, Reward)
  , fromString
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)

-- lives in it's own module due to a name conflict with the `Mint` Type
data RedeemerTag = Spend | Mint | Cert | Reward

fromString :: String -> Maybe RedeemerTag
fromString = case _ of
  "spend" -> Just Spend
  "mint" -> Just Mint
  "certificate" -> Just Cert
  "withdrawal" -> Just Reward
  _ -> Nothing

derive instance Generic RedeemerTag _
derive instance Eq RedeemerTag
derive instance Ord RedeemerTag

instance Show RedeemerTag where
  show = genericShow
