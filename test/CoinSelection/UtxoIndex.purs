module Test.Ctl.CoinSelection.UtxoIndex (suite) where

import Prelude

import Ctl.Internal.CoinSelection.UtxoIndex
  ( SelectionFilter
  , UtxoIndex
  , UtxoIndexInvariantStatus(InvariantHolds)
  )
import Ctl.Internal.CoinSelection.UtxoIndex
  ( buildUtxoIndex
  , checkUtxoIndexInvariants
  , emptyUtxoIndex
  , selectRandomWithFilter
  , utxoIndexDeleteEntry
  , utxoIndexDisjoint
  , utxoIndexInsertEntry
  , utxoIndexPartition
  ) as UtxoIndex
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Unsafe (unsafePerformEffect)
import Mote (group, test)
import Test.Ctl.CoinSelection.Arbitrary
  ( ArbitraryTxUnspentOut
  , ArbitraryUtxoIndex
  , ArbitraryUtxoMap
  )
import Test.QuickCheck (Result(Failed, Success)) as QuickCheck
import Test.QuickCheck ((===))
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "UtxoIndex" do
    test "prop_buildUtxoIndex_empty" do
      UtxoIndex.buildUtxoIndex Map.empty `shouldEqual` UtxoIndex.emptyUtxoIndex

    test "prop_buildUtxoIndex_invariant" do
      quickCheck prop_buildUtxoIndex_invariant

    test "prop_partition_disjoint" do
      quickCheck prop_partition_disjoint

    test "prop_utxoIndexInsertEntry_invariant" do
      quickCheck prop_utxoIndexInsertEntry_invariant

    test "prop_utxoIndexDeleteEntry_invariant" do
      quickCheck prop_utxoIndexDeleteEntry_invariant

    group "SelectRandom" do
      test "prop_selectRandom_invariant" do
        quickCheck prop_selectRandom_invariant

      test "prop_selectRandom_empty" do
        quickCheck prop_selectRandom_empty

prop_buildUtxoIndex_invariant :: ArbitraryUtxoMap -> QuickCheck.Result
prop_buildUtxoIndex_invariant =
  invariantHolds <<< UtxoIndex.buildUtxoIndex <<< unwrap

prop_utxoIndexInsertEntry_invariant
  :: ArbitraryTxUnspentOut -> ArbitraryUtxoIndex -> QuickCheck.Result
prop_utxoIndexInsertEntry_invariant entry utxoIndex =
  invariantHolds $
    UtxoIndex.utxoIndexInsertEntry (unwrap entry) (unwrap utxoIndex)

prop_partition_disjoint
  :: (TransactionInput -> Boolean) -> ArbitraryUtxoIndex -> Boolean
prop_partition_disjoint predicate index = UtxoIndex.utxoIndexDisjoint yes no
  where
  (yes /\ no) = UtxoIndex.utxoIndexPartition predicate (unwrap index)

prop_utxoIndexDeleteEntry_invariant
  :: ArbitraryTxUnspentOut -> ArbitraryUtxoIndex -> QuickCheck.Result
prop_utxoIndexDeleteEntry_invariant entry utxoIndex =
  invariantHolds $
    UtxoIndex.utxoIndexDeleteEntry (unwrap entry) (unwrap utxoIndex)

prop_selectRandom_invariant
  :: ArbitraryUtxoIndex -> SelectionFilter -> QuickCheck.Result
prop_selectRandom_invariant index f =
  let
    result = unsafePerformEffect $ UtxoIndex.selectRandomWithFilter
      (unwrap index)
      f
    check Nothing = QuickCheck.Success
    check (Just (_ /\ index')) = invariantHolds index'
  in
    check result

prop_selectRandom_empty :: SelectionFilter -> QuickCheck.Result
prop_selectRandom_empty f =
  let
    result = unsafePerformEffect $ UtxoIndex.selectRandomWithFilter
      UtxoIndex.emptyUtxoIndex
      f
  in
    result === Nothing

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/9d73b57e23392e25148cfc8db560cb8f656cb56a/lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/UTxOIndexSpec.hs#L183
invariantHolds :: UtxoIndex -> QuickCheck.Result
invariantHolds utxoIndex =
  case UtxoIndex.checkUtxoIndexInvariants utxoIndex of
    InvariantHolds -> QuickCheck.Success
    status -> QuickCheck.Failed (show status)
