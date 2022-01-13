module Ada
  ( adaSymbol
  , adaToken
  , fromValue
  , getLovelace
  , lovelaceValueOf
  , toValue
  ) where

-- import Prelude
import Data.BigInt (BigInt)
import Data.Newtype (unwrap)

import Value (singleton, valueOf)
import Types.Transaction (Ada(..), CurrencySymbol(..), TokenName(..), Value)

getLovelace :: Ada -> BigInt
getLovelace = unwrap

adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol ""

adaToken :: TokenName
adaToken = TokenName ""

lovelaceValueOf :: BigInt -> Value
lovelaceValueOf = singleton adaSymbol adaToken

-- | Create a 'Value' containing only the given 'Ada'.
toValue :: Ada -> Value
toValue (Lovelace i) = singleton adaSymbol adaToken i

{-# INLINABLE fromValue #-}
-- | Get the 'Ada' in the given 'Value'.
fromValue :: Value -> Ada
fromValue v = Lovelace (valueOf v adaSymbol adaToken)