module Serialization.Bech32
  ( Bech32String
  , verifyBech32
  , unwrapBech32
  , getPrefix
  ) where

import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import FFiHelpers (MaybeFfiHelper, maybeFfiHelper)

foreign import verifyBech32Impl :: MaybeFfiHelper -> Int -> String -> Maybe Bech32String
foreign import getPrefixImpl :: Int -> Bech32String -> String

-- | Cardano itself doesn't impose limitations on the length of bech32 strings,
-- but a fixed number must be used in order to use 'bech32.js' and not exceeding
-- 1023 is strongly suggested.
bech32Limit :: Int
bech32Limit = 1023

-- | Represents strings that are in Bech32 BIP173 format but with length limit
-- | extended to 1023 characters.
newtype Bech32String = Bech32String String

derive instance Eq Bech32String
derive newtype instance Show Bech32String

verifyBech32 :: String -> Maybe Bech32String
verifyBech32 = verifyBech32Impl maybeFfiHelper bech32Limit

unwrapBech32 :: Bech32String -> String
unwrapBech32 (Bech32String x) = x

getPrefix :: Bech32String -> String
getPrefix = getPrefixImpl bech32Limit
