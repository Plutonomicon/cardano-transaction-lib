module Serialization.Bech32
  ( Bech32String
  , verifyBech32
  ) where

import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import Data.Typelevel.Undefined (undefined)


newtype Bech32String = Bech32String String

derive instance Eq Bech32String
derive newtype instance Show Bech32String

verifyBech32 :: String -> Maybe Bech32String
verifyBech32 = undefined
