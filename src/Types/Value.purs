module Types.Value
  ( CurrencySymbol(CurrencySymbol)
  , TokenName(TokenName)
  , Value(Value)
  , allTokenNames
  , emptyValue
  , eq
  , flattenValue
  , geq
  , getValue
  , gt
  , isAdaOnly
  , isPos
  , isZero
  , leq
  , lt
  , minus
  , numCurrencySymbols
  , numCurrencySymbols'
  , numTokenNames
  , numTokenNames'
  , singleton
  , unflattenValue
  , valueOf
  )
  where

import Prelude
import Control.Alternative (guard)
import Data.Array (filter)
import Data.BigInt (BigInt, fromInt)
import Data.Foldable (any, length)
import Data.Generic.Rep (class Generic)
import Data.List ((:), all, foldMap, List(Nil))
import Data.Map (keys, lookup, Map, toUnfoldable, unions, values)
import Data.Map as Map
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.These (These(Both, That, This))
import Data.Tuple.Nested ((/\), type (/\))

import Types.ByteArray (ByteArray)

-- This module rewrites functionality from:
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value

newtype CurrencySymbol = CurrencySymbol ByteArray
derive instance newtypeCurrencySymbol :: Newtype CurrencySymbol _
derive instance genericCurrencySymbol :: Generic CurrencySymbol _
derive newtype instance eqCurrencySymbol :: Eq CurrencySymbol
derive newtype instance ordCurrencySymbol :: Ord CurrencySymbol

instance showCurrencySymbol :: Show CurrencySymbol where
  show = genericShow

newtype TokenName = TokenName ByteArray
derive instance newtypeTokenName :: Newtype TokenName _
derive instance genericTokenName :: Generic TokenName _
derive newtype instance eqTokenName :: Eq TokenName
derive newtype instance ordTokenName :: Ord TokenName

instance showTokenName :: Show TokenName where
  show = genericShow

newtype Value = Value (Map CurrencySymbol (Map TokenName BigInt))
derive instance genericValue :: Generic Value _
derive instance newtypeValue :: Newtype Value _
derive newtype instance eqValue :: Eq Value

instance showValue :: Show Value where
  show = genericShow

instance semigroupValue :: Semigroup Value where
  append = unionWith (+)
  -- append v1 v2 =
  --   Value $ Map.unionWith (Map.unionWith (+)) (unwrap v1) (unwrap v2)

instance monoidValue :: Monoid Value where
  mempty = Value Map.empty

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/src/PlutusTx.AssocMap.html#union
-- | Combine two 'Map's.
union :: ∀ k v r. Ord k => Map k v -> Map k r -> Map k (These v r)
union l r =
  let ls :: Array (k /\ v)
      ls = Map.toUnfoldable l

      rs :: Array (k /\ r)
      rs = Map.toUnfoldable r

      f :: v -> Maybe r -> These v r
      f a b' = case b' of
          Nothing -> This a
          Just b  -> Both a b

      ls' :: Array (k /\ These v r)
      ls' = map (\(c /\ i) -> (c /\ f i (Map.lookup c (Map.fromFoldable rs)))) ls

      rs' :: Array (k /\ r)
      rs' = filter (\(c /\ _) -> not (any (\(c' /\ _) -> c' == c) ls)) rs

      rs'' :: Array (k /\ These v r)
      rs'' = map (map That) rs'
   in Map.fromFoldable (ls' <> rs'')

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionVal
-- | Combine two 'Value' maps
unionVal
  :: Value
  -> Value
  -> Map CurrencySymbol (Map TokenName (These BigInt BigInt))
unionVal (Value l) (Value r) =
  let combined
        :: Map CurrencySymbol (These (Map TokenName BigInt) (Map TokenName BigInt))
      combined = union l r
      unBoth
        :: These (Map TokenName BigInt) (Map TokenName BigInt)
        -> Map TokenName (These BigInt BigInt)
      unBoth k = case k of
        This a -> This <$> a
        That b -> That <$> b
        Both a b -> union a b
   in unBoth <$> combined

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionWith
unionWith
  :: (BigInt -> BigInt -> BigInt)
  -> Value
  -> Value
  -> Value
unionWith f ls rs =
  let combined :: Map CurrencySymbol (Map TokenName (These BigInt BigInt))
      combined = unionVal ls rs
      unBoth :: These BigInt BigInt -> BigInt
      unBoth k' = case k' of
        This a -> f a zero
        That b -> f zero b
        Both a b -> f a b
   in Value (map (map unBoth) combined)

-- Could use Data.Newtype (unwrap) too.
getValue :: Value -> Map CurrencySymbol (Map TokenName BigInt)
getValue = unwrap

-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#flattenValue
flattenValue :: Value -> List (CurrencySymbol /\ TokenName /\ BigInt)
flattenValue v = do
    cs /\ m :: CurrencySymbol /\ (Map TokenName BigInt) <-
      toUnfoldable $ getValue v
    tn /\ a :: TokenName /\ BigInt <- toUnfoldable m
    guard $ a /= zero
    pure $ cs /\ tn /\ a

-- From https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
unflattenValue :: CurrencySymbol /\ TokenName /\ BigInt -> Value
unflattenValue (curSymbol /\ tokenName /\ amount) =
  Value <<< Map.singleton curSymbol <<< Map.singleton tokenName $ amount

-- | Predicate on whether some Value contains Ada only.
isAdaOnly :: Value -> Boolean
isAdaOnly v =
  case flattenValue v of
    (cs /\ tn /\ _) : Nil ->
      cs == CurrencySymbol mempty &&
      tn == TokenName mempty
    _ -> false

emptyValue :: Value
emptyValue = Value $ Map.empty

-- From https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
minus :: Value -> Value -> Value
minus x y =
  let negativeValues :: List (CurrencySymbol /\ TokenName /\ BigInt)
      negativeValues = flattenValue y <#>
        (\(c /\ t /\ a) -> c /\ t /\ negate a)
   in x <> foldMap unflattenValue negativeValues

-- From https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- "isValueNat" uses flattenValue which guards against zeros, so non-strict
-- inequality is redundant. So we use strict equality instead.
isPos :: Value -> Boolean
isPos = all (\(_ /\ _ /\ a) -> a > zero) <<< flattenValue

-- From https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#isZero
-- | Check whether a 'Value' is zero.
isZero :: Value -> Boolean
isZero = all (all ((==) zero)) <<< getValue

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkPred
checkPred :: (These BigInt BigInt -> Boolean) -> Value -> Value -> Boolean
checkPred f l r =
  let inner :: Map TokenName (These BigInt BigInt) -> Boolean
      inner = all f -- this "all" may need to be checked?
    in all inner (unionVal l r) -- this "all" may need to be checked?

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkBinRel
-- | Check whether a binary relation holds for value pairs of two 'Value' maps,
-- |  supplying 0 where a key is only present in one of them.
checkBinRel :: (BigInt -> BigInt -> Boolean) -> Value -> Value -> Boolean
checkBinRel f l r =
  let unThese :: These BigInt BigInt -> Boolean
      unThese k' = case k' of
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

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#valueOf
-- | Get the quantity of the given currency in the 'Value'.
valueOf :: Value -> CurrencySymbol -> TokenName -> BigInt
valueOf (Value mp) cur tn =
  case lookup cur mp of
    Nothing -> zero
    Just i -> case lookup tn i of
      Nothing -> zero
      Just v -> v

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#singleton
-- | Make a 'Value' containing only the given quantity of the given currency.
singleton :: CurrencySymbol -> TokenName -> BigInt -> Value
singleton c tn i = Value (Map.singleton c (Map.singleton tn i))

-- | The number of distinct currency symbols, i.e. the number of policy IDs.
numCurrencySymbols :: Value -> BigInt
numCurrencySymbols = fromInt <<< length <<< getValue

numCurrencySymbols' :: Maybe Value -> BigInt
numCurrencySymbols' = maybe zero numCurrencySymbols

-- Don't export this, we don't really care about the v in k,v.
allTokenNames' :: Value -> Map TokenName BigInt
allTokenNames' = unions <<< values <<< getValue

allTokenNames :: Value -> Set TokenName
allTokenNames = keys <<< allTokenNames'

-- | The number of distinct token names.
numTokenNames :: Value -> BigInt
numTokenNames = length <<< allTokenNames'

numTokenNames' :: Maybe Value -> BigInt
numTokenNames' = maybe zero numTokenNames
