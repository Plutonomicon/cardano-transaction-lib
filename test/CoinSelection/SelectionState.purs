-- Taken from cardano-wallet:
-- https://github.com/input-output-hk/cardano-wallet/blob/9d73b57e23392e25148cfc8db560cb8f656cb56a/lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/UTxOSelectionSpec.hs

module Test.Ctl.CoinSelection.SelectionState
  ( suite
  ) where

import Prelude

import Cardano.Types.Asset (Asset)
import Ctl.Internal.BalanceTx.CoinSelection (SelectionState)
import Ctl.Internal.BalanceTx.CoinSelection as CoinSelection
import Ctl.Internal.CoinSelection.UtxoIndex as UtxoIndex
import Mote.TestPlanM (TestPlanM)
import Data.Array.NonEmpty (cons')
import Data.Maybe (Maybe(Just), isJust)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Unsafe (unsafePerformEffect)
import Mote (group, test)
import Test.Ctl.CoinSelection.Arbitrary
  ( ArbitrarySelectionState
  , ArbitraryTxUnspentOut
  , ArbitraryUtxoIndex
  )
import Test.Spec.QuickCheck (quickCheck)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "SelectionState" do
    group "Generation" do
      test "Arbitrary is valid" $ quickCheck prop_arbitrary_isValid
    group "Construction" do
      test "prop_mkSelectionState_isValid" $ quickCheck
        prop_mkSelectionState_isValid
    group "Modification" do
      test "prop_select_isValid" $ quickCheck prop_select_isValid
      test "prop_selectRandomWithPriority" $ quickCheck
        prop_selectRandomWithPriority

-- Tests

prop_arbitrary_isValid :: ArbitrarySelectionState -> Boolean
prop_arbitrary_isValid = isValidSelection <<< unwrap

prop_mkSelectionState_isValid :: ArbitraryUtxoIndex -> Boolean
prop_mkSelectionState_isValid = isValidSelection
  <<< CoinSelection.mkSelectionState
  <<< unwrap

prop_select_isValid
  :: ArbitraryTxUnspentOut -> ArbitrarySelectionState -> Boolean
prop_select_isValid u s =
  isValidSelection $ CoinSelection.selectUtxo (unwrap u) (unwrap s)

prop_selectRandomWithPriority
  :: ArbitraryUtxoIndex -> Asset -> Asset -> Boolean
prop_selectRandomWithPriority index a1 a2 =
  if a1 == a2 then true
  else
    let
      s1 = UtxoIndex.SelectPairWith a1
      s2 = UtxoIndex.SelectPairWith a2
      index' = unwrap index
      haveMatchForAsset1 = unsafePerformEffect $ isJust <$>
        (UtxoIndex.selectRandomWithFilter index' s1)
      haveMatchForAsset2 = unsafePerformEffect $ isJust <$>
        (UtxoIndex.selectRandomWithFilter index' s2)
      result = unsafePerformEffect $
        CoinSelection.selectRandomWithPriority index' (cons' s1 [ s2 ])
    in
      case result of
        Just ((_ /\ b) /\ _) | (unwrap b).amount `UtxoIndex.valueHasAsset` a1 ->
          do
            haveMatchForAsset1
        Just ((_ /\ b) /\ _) | (unwrap b).amount `UtxoIndex.valueHasAsset` a2 ->
          do
            (not haveMatchForAsset1) && haveMatchForAsset2
        _ -> do
          (not haveMatchForAsset1) && (not haveMatchForAsset2)

-- Invariants

isValidSelection :: SelectionState -> Boolean
isValidSelection s = UtxoIndex.utxoIndexDisjoint
  (indexRecord.leftoverUtxos)
  (UtxoIndex.buildUtxoIndex $ indexRecord.selectedUtxos)
  where
  indexRecord = unwrap s
