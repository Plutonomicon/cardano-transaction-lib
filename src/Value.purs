module Value
  ( flattenValue
  ) where

import Prelude
import Control.Alternative (guard)
import Data.BigInt as BigInt
import Data.List ((:), List(..))
import Data.Map (Map, toUnfoldable)
import Data.Tuple.Nested ((/\), type (/\))

import Types.Transaction as Transaction

-- Could use Data.Newtype (unwrap) too.
getValue
  :: Transaction.Value
  -> Map Transaction.CurrencySymbol (Map Transaction.TokenName BigInt.BigInt)
getValue (Transaction.Value v) = v

-- Taken from https://staging.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#flattenValue
flattenValue
  :: Transaction.Value
  -> List (Transaction.CurrencySymbol /\ Transaction.TokenName /\ BigInt.BigInt)
flattenValue v = do
    cs /\ m <- toUnfoldable <<< getValue $ v
    tn /\ a <- toUnfoldable m
    guard $ a /= zero
    pure $ cs /\ tn /\ a

isAdaOnly :: Transaction.Value -> Boolean
isAdaOnly v =
  case flattenValue v of
    (Transaction.CurrencySymbol "" /\ Transaction.TokenName "" /\ _) : Nil -> true
    _ -> false