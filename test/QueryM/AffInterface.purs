module Test.Ctl.QueryM.AffInterface (suite) where

import Prelude

import Control.Monad.Except (throwError)
import Ctl.Internal.QueryM
  ( QueryM
  , getChainTip
  , submitTxOgmios
  )
import Ctl.Internal.QueryM.CurrentEpoch (getCurrentEpoch)
import Ctl.Internal.QueryM.EraSummaries (getEraSummaries)
import Ctl.Internal.QueryM.GetTxByHash (getTxByHash)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.ByteArray (hexToByteArrayUnsafe)
import Data.Either (Either(Left, Right))
import Data.Maybe (isJust)
import Data.Newtype (wrap)
import Data.String.CodeUnits (indexOf)
import Data.String.Pattern (Pattern(Pattern))
import Effect.Aff (error, try)
import Mote (group, test)
import Test.Spec.Assertions (shouldSatisfy)

-- note: currently this suite relies on Ogmios being open and running against the
-- testnet, and does not directly test outputs, as this suite is intended to
-- help verify that the Aff interface for websockets itself works,
-- not that the data represents expected values, as that would depend on chain
-- state, and ogmios itself.
--
-- note: the only way to run QueryM is via Contract, which implicitly requires
-- some Ogmios endpoints to be called, and are therefore not included here.
suite :: TestPlanM (QueryM Unit) Unit
suite = do
  group "QueryM" do
    group "Aff Interface" do
      test "Get ChainTip" testGetChainTip
      test "Get CurrentEpoch" testGetCurrentEpoch
      test "Get EraSummaries" testGetEraSummaries
    group "Ogmios error" do
      test "Ogmios fails with user-friendly message" do
        try testSubmitTxFailure >>= case _ of
          Right _ -> do
            void $ throwError $ error $
              "Unexpected success in testSubmitTxFailure"
          Left error -> do
            (Pattern "Server responded with `fault`" `indexOf` show error)
              `shouldSatisfy` isJust
    group "Ogmios datum cache" do
      test "Can process GetTxByHash" do
        testOgmiosGetTxByHash

testOgmiosGetTxByHash :: QueryM Unit
testOgmiosGetTxByHash = do
  void $ getTxByHash $ hexToByteArrayUnsafe
    "f7c47c65216f7057569111d962a74de807de57e79f7efa86b4e454d42c875e4e"

testGetChainTip :: QueryM Unit
testGetChainTip = do
  void getChainTip

testGetEraSummaries :: QueryM Unit
testGetEraSummaries = do
  void getEraSummaries

testSubmitTxFailure :: QueryM Unit
testSubmitTxFailure = do
  let bytes = hexToByteArrayUnsafe "00"
  void $ submitTxOgmios bytes (wrap bytes)

testGetCurrentEpoch :: QueryM Unit
testGetCurrentEpoch = do
  void getCurrentEpoch
