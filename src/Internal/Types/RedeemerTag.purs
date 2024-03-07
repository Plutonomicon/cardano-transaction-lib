module Ctl.Internal.Types.RedeemerTag
  ( fromString
  ) where

import Cardano.Types.RedeemerTag (RedeemerTag(..))
import Data.Maybe (Maybe(Just, Nothing))

fromString :: String -> Maybe RedeemerTag
fromString = case _ of
  "spend" -> Just Spend
  "mint" -> Just Mint
  "certificate" -> Just Cert
  "withdrawal" -> Just Reward
  _ -> Nothing
