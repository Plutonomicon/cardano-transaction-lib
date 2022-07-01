module Types.RedeemerTag
  ( RedeemerTag(Spend, Mint, Cert, Reward)
  , fromString
  ) where

import Prelude

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

-- lives in it's own module due to a name conflict with the `Mint` Type
data RedeemerTag = Spend | Mint | Cert | Reward

fromString :: String -> Maybe RedeemerTag
fromString tag
  | tag == "spend" = Just Spend
  | tag == "mint" = Just Mint
  | tag == "certificate" = Just Cert
  | tag == "withdrawal" = Just Reward
  | otherwise = Nothing

derive instance Generic RedeemerTag _
derive instance Eq RedeemerTag
derive instance Ord RedeemerTag

instance Show RedeemerTag where
  show = genericShow

instance Enum RedeemerTag where
  pred = genericPred
  succ = genericSucc

instance Bounded RedeemerTag where
  top = Reward
  bottom = Spend

instance BoundedEnum RedeemerTag where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
