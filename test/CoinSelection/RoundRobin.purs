module Test.Ctl.CoinSelection.RoundRobin where

import Prelude

import Cardano.Types.AssetName (AssetName)
import Ctl.Internal.BalanceTx.CoinSelection (runRoundRobinM)
import Mote.TestPlanM (TestPlanM)
import Data.Foldable (all, foldl, sum)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(Identity))
import Data.List
  ( List
  , fromFoldable
  , length
  , singleton
  , sort
  , sortBy
  , tail
  , zip
  , (:)
  )
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Ordering (invert)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple (snd, swap, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicateA)
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Ctl.CoinSelection.Arbitrary (ArbitraryMap)
import Test.QuickCheck (Result, (<?>), (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt)
import Test.Spec.QuickCheck (quickCheck)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Grouping and ungrouping" do
    test "groupByKey_ungroupByKey" $
      quickCheck
        \(x :: List (Int /\ Int)) -> on (===) sort
          x
          (ungroupByKey $ groupByKey x)
    test "ungroupByKey_groupByKey" $
      quickCheck
        -- There is no Arbitrary for Map
        \(wm :: ArbitraryMap Int (NonEmpty List Int)) ->
          let
            m = map fromFoldable $ unwrap wm
          in
            on (===) (map sort)
              m
              (groupByKey $ ungroupByKey m)
  group "RoundRobin" do
    test "identity" $ quickCheck prop_runRoundRobin_identity
    test "iterationCount" $ quickCheck prop_runRoundRobin_iterationCount
    test "iterationOrder" $
      quickCheck prop_runRoundRobin_iterationOrder
    test "generationCount" $
      quickCheck prop_runRoundRobin_iterationOrder
    test "generationOrder" $
      quickCheck prop_runRoundRobin_generationOrder

-- Tests

prop_runRoundRobin_identity :: SimpleMockRoundRobinState -> Array Unit -> Result
prop_runRoundRobin_identity state processors =
  runRoundRobin state ((const Nothing) <$ processors) === state

prop_runRoundRobin_iterationCount :: SimpleMockRoundRobinState -> Result
prop_runRoundRobin_iterationCount initialState = (===)
  ( length $
      (unwrap $ runMockRoundRobin initialState).accumulatedEntries
  )
  (sum (unwrap initialState).processorLifetimes)

prop_runRoundRobin_iterationOrder :: SimpleMockRoundRobinState -> Result
prop_runRoundRobin_iterationOrder initialState =
  sortDescending entries === entries
  where
  entries = swap <$>
    (unwrap $ runMockRoundRobin initialState).accumulatedEntries
  sortDescending = sortBy \x y -> invert $ compare x y

prop_runRoundRobin_generationCount :: SimpleMockRoundRobinState -> Result
prop_runRoundRobin_generationCount initialState =
  Map.filter (_ > 0) (unwrap initialState).processorLifetimes
    === generationCounts
  where
  finalState = runMockRoundRobin initialState

  generationCounts :: Map AssetName Int
  generationCounts = (unwrap finalState).accumulatedEntries
    # groupByKey
    # map length

prop_runRoundRobin_generationOrder :: SimpleMockRoundRobinState -> Result
prop_runRoundRobin_generationOrder initialState =
  result <?> "initialState: " <> show initialState
  where
  finalState = runMockRoundRobin initialState

  generations :: Map Int (Set.Set AssetName)
  generations = (unwrap finalState).accumulatedEntries
    # map swap
    # groupByKey
    # map Set.fromFoldable
  result = all (uncurry (flip Set.subset))
    $ consecutivePairs
    $ snd <$> Map.toUnfoldable generations

-- Utilites for tests

runRoundRobin :: forall (s :: Type). s -> Array (s -> Maybe s) -> s
runRoundRobin state processors =
  unwrap $ runRoundRobinM state $ (map Identity) <$> processors

consecutivePairs :: forall a. List a -> List (a /\ a)
consecutivePairs xs = case tail xs of
  Nothing -> fromFoldable []
  Just ys -> xs `zip` ys

groupByKey :: forall k v. Ord k => List (k /\ v) -> Map k (List v)
groupByKey = foldl acc Map.empty
  where
  acc :: Map k (List v) -> (k /\ v) -> Map k (List v)
  acc m (k /\ v) = Map.alter (Just <<< (maybe (singleton v) ((:) v))) k m

ungroupByKey :: forall k v. Map k (List v) -> List (k /\ v)
ungroupByKey m = do
  (k /\ vs) <- Map.toUnfoldable m
  v <- vs
  pure (k /\ v)

-- Mock state used for testing

-- From https://github.com/input-output-hk/cardano-wallet/blob/9d73b57e23392e25148cfc8db560cb8f656cb56a/lib/coin-selection/test/spec/Cardano/CoinSelection/BalanceSpec.hs#L4036
newtype MockRoundRobinState k n = MockRoundRobinState
  { processorLifetimes :: Map k n
  , accumulatedEntries :: List (k /\ n)
  }

derive instance Newtype (MockRoundRobinState k n) _
derive instance Generic (MockRoundRobinState k n) _

instance (Show k, Show n) => Show (MockRoundRobinState k n) where
  show = genericShow

type SimpleMockRoundRobinState = MockRoundRobinState AssetName Int

derive instance Eq (MockRoundRobinState AssetName Int)

instance Arbitrary SimpleMockRoundRobinState where
  arbitrary = do
    processorCount <- chooseInt 0 16
    lifetimes <- genProcessorLifetimes processorCount
    pure $ MockRoundRobinState
      { processorLifetimes: lifetimes, accumulatedEntries: fromFoldable [] }
    where
    genProcessorLifetimes :: Int -> Gen (Map.Map AssetName Int)
    genProcessorLifetimes processorCount =
      Map.fromFoldable <$>
        (replicateA processorCount genProcessorLifetime :: Gen (Array _))

    genProcessorLifetime :: Gen (AssetName /\ Int)
    genProcessorLifetime = (/\)
      <$> arbitrary
      <*> chooseInt 0 127 -- Using `Arbitrary` leads to stack overflows in tests

runMockRoundRobin
  :: forall k n
   . Ord k
  => Ord n
  => EuclideanRing n
  => MockRoundRobinState k n
  -> MockRoundRobinState k n
runMockRoundRobin initialState = runRoundRobin initialState processors
  where
  processors
    :: Array (MockRoundRobinState k n -> Maybe (MockRoundRobinState k n))
  processors = mkProcessor <$> Map.toUnfoldable
    (unwrap initialState).processorLifetimes

  mkProcessor
    :: (k /\ n) -> MockRoundRobinState k n -> Maybe (MockRoundRobinState k n)
  mkProcessor (k /\ n) s
    | remainingLifetime k s <= zero =
        Nothing
    | otherwise =
        Just $ MockRoundRobinState
          { processorLifetimes: Map.update (Just <<< (_ - one)) k
              (unwrap s).processorLifetimes
          , accumulatedEntries: entry : (unwrap s).accumulatedEntries
          }
        where
        entry :: (k /\ n)
        entry = (k /\ (n - remainingLifetime k s))

  remainingLifetime :: k -> MockRoundRobinState k n -> n
  remainingLifetime k =
    fromMaybe zero <<< Map.lookup k <<< _.processorLifetimes <<< unwrap
