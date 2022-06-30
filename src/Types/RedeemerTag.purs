module Types.RedeemerTag
  ( RedeemerTag(Spend, Mint, Cert, Reward)
  ) where

import Prelude

import Aeson (class EncodeAeson, encodeAeson', encodeAeson)
import Aeson.Encode (encodeTagged)
import Data.Enum (class Enum, class BoundedEnum)
import Data.Enum.Generic
  ( genericPred
  , genericSucc
  , genericCardinality
  , genericToEnum
  , genericFromEnum
  )
import Data.Generic.Rep (class Generic)
import Data.Op (Op(Op))
import Data.Show.Generic (genericShow)

-- lives in it's own module due to a name conflict with the `Mint` Type
data RedeemerTag = Spend | Mint | Cert | Reward

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

instance EncodeAeson RedeemerTag where
  encodeAeson' = case _ of
    Spend -> encodeTagged' "Spend" {}
    Mint -> encodeTagged' "Mint" {}
    Cert -> encodeTagged' "Cert" {}
    Reward -> encodeTagged' "Reward" {}
    where
    encodeTagged' str x = encodeAeson' $ encodeTagged str x (Op encodeAeson)
