module Test.Ctl.CoinSelection.UtxoIndex (suite) where

import Prelude

import Ctl.Internal.CoinSelection.UtxoIndex
  ( UtxoIndex
  , UtxoIndexInvariantStatus(InvariantHolds)
  )
import Ctl.Internal.CoinSelection.UtxoIndex
  ( buildUtxoIndex
  , checkUtxoIndexInvariants
  , emptyUtxoIndex
  , utxoIndexDeleteEntry
  , utxoIndexInsertEntry
  ) as UtxoIndex
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Map (empty) as Map
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Ctl.CoinSelection.Arbitrary
  ( ArbitraryTxUnspentOut
  , ArbitraryUtxoIndex
  , ArbitraryUtxoMap
  )
import Test.QuickCheck (Result(Failed, Success)) as QuickCheck
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "UtxoIndex" do
    test "prop_buildUtxoIndex_empty" do
      UtxoIndex.buildUtxoIndex Map.empty `shouldEqual` UtxoIndex.emptyUtxoIndex

    test "prop_buildUtxoIndex_invariant" do
      quickCheck prop_buildUtxoIndex_invariant

    test "prop_utxoIndexInsertEntry_invariant" do
      quickCheck prop_utxoIndexInsertEntry_invariant

    test "prop_utxoIndexDeleteEntry_invariant" do
      quickCheck prop_utxoIndexDeleteEntry_invariant

prop_buildUtxoIndex_invariant :: ArbitraryUtxoMap -> QuickCheck.Result
prop_buildUtxoIndex_invariant =
  invariantHolds <<< UtxoIndex.buildUtxoIndex <<< unwrap

prop_utxoIndexInsertEntry_invariant
  :: ArbitraryTxUnspentOut -> ArbitraryUtxoIndex -> QuickCheck.Result
prop_utxoIndexInsertEntry_invariant entry utxoIndex =
  invariantHolds $
    UtxoIndex.utxoIndexInsertEntry (unwrap entry) (unwrap utxoIndex)

prop_utxoIndexDeleteEntry_invariant
  :: ArbitraryTxUnspentOut -> ArbitraryUtxoIndex -> QuickCheck.Result
prop_utxoIndexDeleteEntry_invariant entry utxoIndex =
  invariantHolds $
    UtxoIndex.utxoIndexDeleteEntry (unwrap entry) (unwrap utxoIndex)

-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/9d73b57e23392e25148cfc8db560cb8f656cb56a/lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/UTxOIndexSpec.hs#L183
invariantHolds :: UtxoIndex -> QuickCheck.Result
invariantHolds utxoIndex =
  case UtxoIndex.checkUtxoIndexInvariants utxoIndex of
    InvariantHolds -> QuickCheck.Success
    status -> QuickCheck.Failed (show status)
