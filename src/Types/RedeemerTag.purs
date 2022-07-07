module Types.RedeemerTag
  ( RedeemerTag(Spend, Mint, Cert, Reward)
  , fromString
  ) where

import Prelude

import Aeson (class EncodeAeson, encodeAeson')
import Data.Enum (class Enum, class BoundedEnum)
import Data.Enum.Generic
  ( genericPred
  , genericSucc
  , genericCardinality
  , genericToEnum
  , genericFromEnum
  )
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)
import Helpers (encodeTagged')

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

instance EncodeAeson RedeemerTag where
  encodeAeson' = case _ of
    Spend -> encodeAeson' $ encodeTagged' "Spend" {}
    Mint -> encodeAeson' $ encodeTagged' "Mint" {}
    Cert -> encodeAeson' $ encodeTagged' "Cert" {}
    Reward -> encodeAeson' $ encodeTagged' "Reward" {}
