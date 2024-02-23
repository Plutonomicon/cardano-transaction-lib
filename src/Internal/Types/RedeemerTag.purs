module Ctl.Internal.Types.RedeemerTag
  ( RedeemerTag(Spend, Mint, Cert, Reward, Vote, Propose)
  , fromString
  ) where

import Prelude

import Aeson (class EncodeAeson)
import Ctl.Internal.Helpers (encodeTagged')
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)

-- lives in it's own module due to a name conflict with the `Mint` Type
data RedeemerTag = Spend | Mint | Cert | Reward | Vote | Propose

fromString :: String -> Maybe RedeemerTag
fromString = case _ of
  "spend" -> Just Spend
  "mint" -> Just Mint
  "publish" -> Just Cert
  "withdraw" -> Just Reward
  "vote" -> Just Vote
  "propose" -> Just Propose
  _ -> Nothing

derive instance Generic RedeemerTag _
derive instance Eq RedeemerTag
derive instance Ord RedeemerTag

instance Show RedeemerTag where
  show = genericShow

instance EncodeAeson RedeemerTag where
  encodeAeson = case _ of
    Spend -> encodeTagged' "Spend" {}
    Mint -> encodeTagged' "Mint" {}
    Cert -> encodeTagged' "Cert" {}
    Reward -> encodeTagged' "Reward" {}
    Vote -> encodeTagged' "Vote" {}
    Propose -> encodeTagged' "Propose" {}
