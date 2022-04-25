module Plutus.Types.Value
  ( Value
  , CurrencySymbol
  , getCurrencySymbol
  , adaSymbol
  , mkCurrencySymbol
  , getValue
  , singleton
  , singleton'
  , valueOf
  , lovelaceValueOf
  , isZero
  , negation
  , split
  , unionWith
  ) where

import Prelude

import Control.Apply (lift3)
import Data.BigInt (BigInt)
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.Ring (negate)
import Data.Show.Generic (genericShow)
import Data.These (These(Both, That, This), these)
import Data.Tuple.Nested (type (/\), (/\))
import Plutus.Types.AssocMap
  ( Map(Map)
  , singleton
  , lookup
  , keys
  , union
  , unionWith
  , mapThese
  ) as PlutusMap
import FromData (class FromData, fromData)
import Serialization.Hash (scriptHashFromBytes)
import ToData (class ToData, toData)
import Types.ByteArray (ByteArray)
import Types.TokenName (TokenName, adaToken, mkTokenName)

type PlutusMap = PlutusMap.Map

newtype Value = Value (PlutusMap CurrencySymbol (PlutusMap TokenName BigInt))

derive instance Generic Value _
derive instance Eq Value

instance Show Value where
  show = genericShow

instance Semigroup Value where
  append = unionWith add

instance Monoid Value where
  mempty = Value (PlutusMap.Map [])

--------------------------------------------------------------------------------
-- ToData / FromData
--------------------------------------------------------------------------------

instance ToData Value where
  toData (Value mp) = toData mp

instance FromData Value where
  fromData = map Value <<< fromData

--------------------------------------------------------------------------------
-- Exported Functions
--------------------------------------------------------------------------------

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#Value
-- | Get the underlying `Plutus.Types.AssocMap.Map`.
getValue :: Value -> PlutusMap CurrencySymbol (PlutusMap TokenName BigInt)
getValue (Value mp) = mp

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#singleton
-- | Make a `Value` containing only the given quantity of the given currency.
singleton :: CurrencySymbol -> TokenName -> BigInt -> Value
singleton cs tn = Value <<< PlutusMap.singleton cs <<< PlutusMap.singleton tn

-- | Creates a singleton value given two byte arrays for currency symbol and
-- | token name respectively.
singleton' :: ByteArray -> ByteArray -> BigInt -> Maybe Value
singleton' cs tn amount
  | cs == mempty && tn == mempty =
      pure (singleton adaSymbol adaToken amount)
  | otherwise =
      lift3 singleton (mkCurrencySymbol cs) (mkTokenName tn)
        (pure amount)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#valueOf
-- | Get the quantity of the given currency in the `Value`.
valueOf :: Value -> CurrencySymbol -> TokenName -> BigInt
valueOf (Value mp) cs tn = fromMaybe zero $
  PlutusMap.lookup cs mp >>= PlutusMap.lookup tn

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Ada.html#lovelaceValueOf
-- | A Value with the given amount of Lovelace (the currency unit).
lovelaceValueOf :: BigInt -> Value
lovelaceValueOf = singleton adaSymbol adaToken

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#symbols
-- | The list of `CurrencySymbol`s of a `Value`.
symbols :: Value -> Array CurrencySymbol
symbols (Value mp) = PlutusMap.keys mp

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#isZero
-- | Check whether a `Value` is zero.
isZero :: Value -> Boolean
isZero = all (all (eq zero)) <<< getValue

negation :: Value -> Value
negation (Value mp) = Value (map (map negate) mp)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#split
-- | Split a value into its positive and non-positive parts. The first element of
-- | the tuple contains the non-positive parts of the value, the second element
-- | contains the positive parts. The convention is non-positive parts are
-- | negated to make them positive in the output.
split :: Value -> Value /\ Value
split (Value mp) =
  let
    neg /\ pos = PlutusMap.mapThese worker mp
  in
    negation (Value neg) /\ Value pos
  where
  worker
    :: PlutusMap TokenName BigInt
    -> These (PlutusMap TokenName BigInt) (PlutusMap TokenName BigInt)
  worker mp' = Both l r
    where
    l /\ r =
      PlutusMap.mapThese (\a -> if a <= zero then This a else That a) mp'

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionWith
-- | Combines `Value` with a binary function on `BigInt`s.
unionWith :: (BigInt -> BigInt -> BigInt) -> Value -> Value -> Value
unionWith f lhs =
  Value <<< map (map (these identity identity f)) <<< unionVal lhs
  where
  unionVal (Value lhs) (Value rhs) =
    these (map This) (map That) PlutusMap.union <$>
      PlutusMap.union lhs rhs

--------------------------------------------------------------------------------
-- CurrencySymbol
--------------------------------------------------------------------------------

newtype CurrencySymbol = CurrencySymbol ByteArray

derive newtype instance Eq CurrencySymbol
derive newtype instance Ord CurrencySymbol
derive newtype instance FromData CurrencySymbol
derive newtype instance ToData CurrencySymbol

instance Show CurrencySymbol where
  show (CurrencySymbol cs) = "(CurrencySymbol" <> show cs <> ")"

getCurrencySymbol :: CurrencySymbol -> ByteArray
getCurrencySymbol (CurrencySymbol curSymbol) = curSymbol

adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol mempty

mkCurrencySymbol :: ByteArray -> Maybe CurrencySymbol
mkCurrencySymbol byteArr
  | byteArr == mempty =
      pure adaSymbol
  | otherwise =
      scriptHashFromBytes byteArr *> pure (CurrencySymbol byteArr)
