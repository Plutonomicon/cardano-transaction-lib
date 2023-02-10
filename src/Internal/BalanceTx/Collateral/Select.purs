module Ctl.Internal.BalanceTx.Collateral.Select where

import Prelude

import Ctl.Internal.BalanceTx.FakeOutput (fakeOutputWithNonAdaAssets)
import Ctl.Internal.BalanceTx.UtxoMinAda (utxoMinAdaValue)
import Ctl.Internal.Cardano.Types.Transaction (TransactionOutput, UtxoMap)
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Ctl.Internal.Cardano.Types.Value (NonAdaAsset)
import Ctl.Internal.Cardano.Types.Value (getNonAdaAsset, valueToCoin') as Value
import Ctl.Internal.QueryM.Ogmios (CoinsPerUtxoUnit, ProtocolParameters(ProtocolParameters))
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, fromNumber, toNumber) as BigInt
import Data.Foldable (foldMap, foldl, length)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.List (List(Nil, Cons))
import Data.List as List
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ordering (invert) as Ordering
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toInt, toNumber) as UInt
import Effect (Effect)
import Math (ceil) as Math
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)

--------------------------------------------------------------------------------
-- Collateral selection constraints
--------------------------------------------------------------------------------

-- | Specifies an upper bound on the search space size.
data SearchSpaceLimit
  -- | Limits the numbers of candidate utxo combinations.
  = SearchSpaceLimit
      { maxCandidateUtxos :: Int
      , maxCollateralInputs :: Int
      }
  -- | Specifies that there is no search space limit.
  -- | WARNING: This should only be used for testing purposes.
  | UnsafeNoSearchSpaceLimit

derive instance Generic SearchSpaceLimit _

instance Show SearchSpaceLimit where
  show = genericShow

getMaxCandidateUtxos :: SearchSpaceLimit -> Maybe Int
getMaxCandidateUtxos searchSpaceLimit =
  case searchSpaceLimit of
    SearchSpaceLimit { maxCandidateUtxos } -> Just maxCandidateUtxos
    UnsafeNoSearchSpaceLimit -> Nothing

getMaxCollateralInputs :: SearchSpaceLimit -> Maybe Int
getMaxCollateralInputs searchSpaceLimit =
  case searchSpaceLimit of
    SearchSpaceLimit { maxCollateralInputs } -> Just maxCollateralInputs
    UnsafeNoSearchSpaceLimit -> Nothing

-- | Using the default search space limits will allow to select collateral
-- | from 45_825 utxo combinations.
defaultSearchSpaceLimit :: SearchSpaceLimit
defaultSearchSpaceLimit = SearchSpaceLimit
  { maxCandidateUtxos: 65
  , maxCollateralInputs: 3
  }

type CollateralSelectionConstraintsRec =
  { searchSpaceLimit :: SearchSpaceLimit
  , minRequiredCollateral :: Maybe BigInt
  }

newtype CollateralSelectionConstraints =
  CollateralSelectionConstraints CollateralSelectionConstraintsRec

derive instance Generic CollateralSelectionConstraints _
derive instance Newtype CollateralSelectionConstraints _

instance Show CollateralSelectionConstraints where
  show = genericShow

defaultCollateralSelectionConstraints :: CollateralSelectionConstraints
defaultCollateralSelectionConstraints = wrap
  { searchSpaceLimit: defaultSearchSpaceLimit
  , minRequiredCollateral: Just (BigInt.fromInt 5_000_000)
  }

--------------------------------------------------------------------------------
-- Collateral selection
--------------------------------------------------------------------------------

newtype CollateralSelectionParams = CollateralSelectionParams
  { transactionFees :: BigInt
  , utxos :: UtxoMap
  , constraints :: CollateralSelectionConstraints
  , maxCollateralInputs :: Int
  , collateralPercent :: Number
  , collateralReturnCompensator :: Number
  , coinsPerUtxoUnit :: CoinsPerUtxoUnit
  }

derive instance Generic CollateralSelectionParams _
derive instance Newtype CollateralSelectionParams _

instance Show CollateralSelectionParams where
  show = genericShow

defaultCollateralSelectionParams
  :: BigInt
  -> UtxoMap
  -> ProtocolParameters
  -> CollateralSelectionParams
defaultCollateralSelectionParams fees utxos (ProtocolParameters pparams) = wrap
  { transactionFees: fees
  , utxos
  , constraints: defaultCollateralSelectionConstraints
  , maxCollateralInputs: UInt.toInt pparams.maxCollateralInputs
  , collateralPercent: (UInt.toNumber pparams.collateralPercent) / 100.0
  , collateralReturnCompensator: 1.5
  , coinsPerUtxoUnit: pparams.coinsPerUtxoUnit
  }

selectCollateral
  :: CollateralSelectionParams
  -> Effect (Maybe (List TransactionUnspentOutput))
selectCollateral (CollateralSelectionParams p@{ coinsPerUtxoUnit }) = p.utxos
  -- 1. Sort utxos by ada value in decreasing order
  # Map.toUnfoldable
  # map (AdaOut <<< asTxUnspentOutput)
  # List.sortBy (\lhs -> Ordering.invert <<< compare lhs)

  -- 2. Limit the number of candidate utxos for collateral selection to have
  -- acceptable time complexity
  # map unwrap
  # maybe identity List.take (getMaxCandidateUtxos constraints.searchSpaceLimit)

  -- 3. Generate utxo combinations with number of utxos in each combination
  -- <= `maxCollateralInputs`
  # combinations maxCollateralInputs

  -- 4. Filter out utxo combinations with total Ada value < `requiredCollateral`
  # List.filter (\x -> foldl adaValue' zero x >= requiredCollateral)

  -- 5. For each candidate utxo combination calculate min ada value of the
  -- corresponding collateral return output
  # traverse (\x -> Tuple x <$> collateralReturnMinAdaValue coinsPerUtxoUnit x)
  # map (List.mapMaybe mkCollateralCandidate)

  -- 6. Sort candidate utxo combinations in ascending order by min ada value,
  -- then select the first combination
  # map (map (Tuple.fst <<< unwrap) <<< List.head <<< List.sort)
  where
  constraints :: CollateralSelectionConstraintsRec
  constraints = unwrap p.constraints

  requiredCollateral :: BigInt
  requiredCollateral =
    constraints.minRequiredCollateral
      # maybe requiredCollateral' (max requiredCollateral')
    where
    requiredCollateral' :: BigInt
    requiredCollateral' =
      fromMaybe fallback $ BigInt.fromNumber $
        Math.ceil (BigInt.toNumber p.transactionFees * feesMultiplier)

    feesMultiplier :: Number
    feesMultiplier = p.collateralPercent * p.collateralReturnCompensator

    fallback :: BigInt
    fallback = p.transactionFees * BigInt.fromInt 2

  maxCollateralInputs :: Int
  maxCollateralInputs =
    case constraints.searchSpaceLimit of
      SearchSpaceLimit searchSpaceLimit ->
        max searchSpaceLimit.maxCollateralInputs p.maxCollateralInputs
      UnsafeNoSearchSpaceLimit -> p.maxCollateralInputs

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

collateralReturnMinAdaValue
  :: CoinsPerUtxoUnit -> List TransactionUnspentOutput -> Effect (Maybe BigInt)
collateralReturnMinAdaValue coinsPerUtxoUnit =
  utxoMinAdaValue coinsPerUtxoUnit <<< fakeOutputWithNonAdaAssets <<< foldMap
    nonAdaAsset

type ReturnOutMinAdaValue = BigInt

newtype CollateralCandidate =
  CollateralCandidate (List TransactionUnspentOutput /\ ReturnOutMinAdaValue)

derive instance Newtype CollateralCandidate _

instance Eq CollateralCandidate where
  eq = eq `on` (Tuple.snd <<< unwrap)

instance Ord CollateralCandidate where
  compare lhs rhs =
    caseEq (on compare byReturnOutMinAda lhs rhs) $
      -- If two candidate utxo combinations correspond to return outputs with
      -- the same utxo min ada value, order them by the number of
      -- collateral inputs:
      caseEq (on compare byNumOfInputs lhs rhs)
        -- If two candidate utxo combinations have the same number of inputs,
        -- order them by ada value:
        (on compare byAdaValue lhs rhs)
    where
    caseEq :: Ordering -> Ordering -> Ordering
    caseEq EQ ordering = ordering
    caseEq ordering _ = ordering

    byReturnOutMinAda :: CollateralCandidate -> ReturnOutMinAdaValue
    byReturnOutMinAda = Tuple.snd <<< unwrap

    byNumOfInputs :: CollateralCandidate -> Int
    byNumOfInputs = List.length <<< Tuple.fst <<< unwrap

    byAdaValue :: CollateralCandidate -> BigInt
    byAdaValue = foldl adaValue' zero <<< Tuple.fst <<< unwrap

mkCollateralCandidate
  :: List TransactionUnspentOutput /\ Maybe ReturnOutMinAdaValue
  -> Maybe CollateralCandidate
mkCollateralCandidate (unspentOutputs /\ returnOutMinAdaValue) =
  CollateralCandidate <<< Tuple unspentOutputs <$> returnOutMinAdaValue

{-
-- | Selects an utxo combination to use as collateral by generating all possible
-- | utxo combinations and then applying the following constraints:
-- |
-- |   1. `maxCollateralInputs` protocol parameter limits the maximum
-- |   cardinality of a single utxo combination.
-- |
-- |   2. Collateral inputs must have a total value of at least 5 Ada
-- |   (`minRequiredCollateral`).
-- |
-- |   3. We prefer utxo combinations that require the lowest utxo min ada
-- |   value for the corresponding collateral output, thus maintaining a
-- |   sufficient `totalCollateral`.
-- |
-- |   4. If two utxo combinations correspond to return outputs with the same
-- |   utxo min ada value, we prefer the one with fewer inputs.
-- |
selectCollateral
  :: CoinsPerUtxoUnit
  -> Int
  -> UtxoMap
  -> Effect (Maybe (List TransactionUnspentOutput))
selectCollateral coinsPerUtxoUnit maxCollateralInputs =
  -- Sort candidate utxo combinations in ascending order by utxo min ada value
  -- of return output, then select the first utxo combination:
  map (map (Tuple.fst <<< unwrap) <<< List.head <<< List.sort)
    -- For each candidate utxo combination calculate
    -- the min Ada value of the corresponding collateral return output:
    <<< map (List.mapMaybe mkCollateralCandidate)
    <<< traverse
      (\x -> Tuple x <$> collateralReturnMinAdaValue coinsPerUtxoUnit x)
    -- Filter out all utxo combinations
    -- with total Ada value < `minRequiredCollateral`:
    <<< List.filter (\x -> foldl adaValue' zero x >= minRequiredCollateral)
    -- Get all possible non-empty utxo combinations
    -- with the number of utxos <= `maxCollateralInputs`:
    <<< combinations maxCollateralInputs
    -- Limit the number of candidate utxos for collateral selection to
    -- maintain acceptable time complexity:
    <<< List.take maxCandidateUtxos
    <<< map unwrap
    -- Sort utxos by ada value in decreasing order:
    <<< List.sortBy (\lhs -> Ordering.invert <<< compare lhs)
    <<< map (AdaOut <<< asTxUnspentOutput)
    <<< Map.toUnfoldable
-}

-- | A wrapper around an utxo with ordering by ada value.
newtype AdaOut = AdaOut TransactionUnspentOutput

derive instance Newtype AdaOut _

instance Eq AdaOut where
  eq = eq `on` (adaValue <<< unwrap)

instance Ord AdaOut where
  compare = compare `on` (adaValue <<< unwrap)

asTxUnspentOutput
  :: TransactionInput /\ TransactionOutput -> TransactionUnspentOutput
asTxUnspentOutput (input /\ output) = wrap { input, output }

adaValue :: TransactionUnspentOutput -> BigInt
adaValue =
  Value.valueToCoin' <<< _.amount <<< unwrap <<< _.output <<< unwrap

adaValue' :: BigInt -> TransactionUnspentOutput -> BigInt
adaValue' init = add init <<< adaValue

nonAdaAsset :: TransactionUnspentOutput -> NonAdaAsset
nonAdaAsset =
  Value.getNonAdaAsset <<< _.amount <<< unwrap <<< _.output <<< unwrap

--------------------------------------------------------------------------------
-- Generating combinations
--------------------------------------------------------------------------------

-- | Generates all possible combinations of list elements with the number of
-- | elements in each combination not exceeding `k`.
-- |
-- | This function can use an excessive amount of time and space.
-- | Running time:
-- |   for small `k`: approximately `O((n^(k+1) - 1)/(n - 1))` or `O(k*n^k)`
combinations :: forall (a :: Type). Int -> List a -> List (List a)
combinations k l
  | k <= zero = mempty
  | otherwise = combinationsOfSize k l <> combinations (k - 1) l

-- | Generates all combinations of size `k` from elements of a given list.
-- |
-- | This function can use an excessive amount of time and space.
-- | Running time: approximately `O(n^k)` for small `k`
combinationsOfSize :: forall (a :: Type). Int -> List a -> List (List a)
combinationsOfSize k l =
  let
    n = length l
    i = n - k + 1
  in
    if k <= zero || k > n then Cons Nil Nil
    else
      List.concat $ List.zipWith next (List.take i l) (tails i l)
  where
  next :: a -> List a -> List (List a)
  next x = map (Cons x) <<< combinationsOfSize (k - 1)

-- | Returns the list of `k` tail segments of a given list.
-- |
-- | Running time: `O(k)`
tails :: forall (a :: Type). Int -> List a -> List (List a)
tails k l
  | k <= 0 = mempty
  | otherwise =
      maybe mempty
        (\{ tail: xs } -> Cons xs $ tails (k - 1) xs)
        (List.uncons l)
