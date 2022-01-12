module Value
  ( emptyValue
  , filterNonAda
  , flattenValue
  , isAdaOnly
  , isNonNeg
  , isZero
  , minus
  ) where

import Prelude
import Control.Alternative (guard)
import Data.BigInt (BigInt)
import Data.Foldable as Foldable
import Data.List ((:), List(..), all, foldMap)
import Data.Map (Map, filterKeys, toUnfoldable)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\), type (/\))

import Ada (adaSymbol)
import Types.Transaction (CurrencySymbol(..), TokenName(..), Value(..))

-- This module rewrites functionality from:
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
-- https://staging.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value

-- Could use Data.Newtype (unwrap) too.
getValue :: Value -> Map CurrencySymbol (Map TokenName BigInt)
getValue = unwrap

-- Taken from https://staging.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#flattenValue
flattenValue :: Value -> List (CurrencySymbol /\ TokenName /\ BigInt)
flattenValue v = do
    cs /\ m <- toUnfoldable <<< getValue $ v
    tn /\ a <- toUnfoldable m
    guard $ a /= zero
    pure $ cs /\ tn /\ a

-- From https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
unflattenValue :: (CurrencySymbol /\ TokenName /\ BigInt) -> Value
unflattenValue (curSymbol /\ tokenName /\ amount) =
  Value <<< Map.singleton curSymbol <<< Map.singleton tokenName $ amount

-- | Predicate on whether some Value contains Ada only.
isAdaOnly :: Value -> Boolean
isAdaOnly v =
  case flattenValue v of
    (CurrencySymbol "" /\ TokenName "" /\ _) : Nil -> true
    _ -> false

emptyValue :: Value
emptyValue = Value $ Map.empty

-- From https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
minus :: Value -> Value -> Value
minus x y =
  let negativeValues = flattenValue y <#>
        (\(c /\ t /\ a) -> (c /\ t /\ negate a))
        :: List (CurrencySymbol /\ TokenName /\ BigInt)
   in x <> foldMap unflattenValue negativeValues

-- From https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
-- | Filter a value to contain only non Ada assets
filterNonAda :: Value -> Value
filterNonAda = Value <<< filterKeys (_ /= adaSymbol) <<< getValue

-- From https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
-- "isValueNat" uses flattenValue which guards against zeros, so non-strict
-- inequality is redundant. We'll follow the original code exactly for now.
isNonNeg :: Value -> Boolean
isNonNeg = all (\(_ /\ _ /\ a) -> a >= zero) <<< flattenValue

-- From https://staging.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#isZero
-- | Check whether a 'Value' is zero.
isZero :: Value -> Boolean
isZero = Foldable.all (Foldable.all ((==) zero)) <<< getValue