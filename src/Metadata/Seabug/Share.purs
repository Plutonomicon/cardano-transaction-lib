module Metadata.Seabug.Share
  ( Share
  , mkShare
  , unShare
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Just, Nothing))
import FromData (class FromData)
import ToData (class ToData)
import Types.PlutusData (PlutusData(Integer))

-- | A number between 0 and 10000 (inclusive) representing percentage of the price.
newtype Share = Share BigInt

derive newtype instance ToData Share

instance FromData Share where
  fromData (Integer n) = BigInt.toInt n >>= mkShare
  fromData _ = Nothing

instance Show Share where
  show (Share share) = "(mkShare (" <> show share <> "))"

derive instance Eq Share

mkShare :: Int -> Maybe Share
mkShare n
  | n >= 0 && n <= 10000 = Just $ Share $ BigInt.fromInt n
  | otherwise = Nothing

unShare :: Share -> BigInt
unShare (Share n) = n
