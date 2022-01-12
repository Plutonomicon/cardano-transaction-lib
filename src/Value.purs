module Value
  ( emptyValue
  , eq
  , filterNonAda
  , flattenValue
  , geq
  , gt
  , isAdaOnly
  , isNonNeg
  , isZero
  , leq
  , lt
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
import Data.These (These(..))
import Data.Tuple.Nested ((/\), type (/\))

import Ada (adaSymbol)
import Types.Transaction (CurrencySymbol(..), TokenName(..), Value(..), unionVal)

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

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkPred
checkPred
  :: (These BigInt BigInt -> Boolean)
  -> Value
  -> Value
  -> Boolean
checkPred f l r =
  let inner :: Map TokenName (These BigInt BigInt) -> Boolean
      inner = Foldable.all f -- this "all" may need to be checked?
    in Foldable.all inner (unionVal l r) -- this "all" may need to be checked?

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkBinRel
-- | Check whether a binary relation holds for value pairs of two 'Value' maps,
-- |  supplying 0 where a key is only present in one of them.
checkBinRel
 :: (BigInt -> BigInt -> Boolean)
 -> Value
 -> Value
 -> Boolean
checkBinRel f l r =
  let unThese k' = case k' of
        This a -> f a zero
        That b -> f zero b
        Both a b -> f a b
   in checkPred unThese l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#geq
-- | Check whether one 'Value' is greater than or equal to another. See 'Value' for an explanation of how operations on 'Value's work.
geq :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
geq = checkBinRel (>=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#gt
-- | Check whether one 'Value' is strictly greater than another. See 'Value' for an explanation of how operations on 'Value's work.
gt :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
gt l r = not (isZero l && isZero r) && checkBinRel (>) l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#leq
-- | Check whether one 'Value' is less than or equal to another. See 'Value' for an explanation of how operations on 'Value's work.
leq :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
leq = checkBinRel (<=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#lt
-- | Check whether one 'Value' is strictly less than another. See 'Value' for an explanation of how operations on 'Value's work.
lt :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
lt l r = not (isZero l && isZero r) && checkBinRel (<) l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#eq
-- | Check whether one 'Value' is equal to another. See 'Value' for an explanation of how operations on 'Value's work.
eq :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
eq = checkBinRel (==)