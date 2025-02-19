module Test.Ctl.QueryM.AffInterface (suite) where

import Prelude

import Cardano.Serialization.Lib (fromBytes)
import Contract.Transaction (TransactionHash(TransactionHash))
import Control.Monad.Except (throwError)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM.CurrentEpoch (getCurrentEpoch)
import Ctl.Internal.QueryM.EraSummaries (getEraSummaries)
import Ctl.Internal.QueryM.Ogmios (getChainTip, submitTxOgmios)
import Data.ByteArray (hexToByteArrayUnsafe)
import Data.Either (Either(Left, Right))
import Data.Maybe (fromJust, isJust)
import Data.Newtype (wrap)
import Data.String.CodeUnits (indexOf)
import Data.String.Pattern (Pattern(Pattern))
import Effect.Aff (error, try)
import Mote (group, test)
import Mote.TestPlanM (TestPlanM)
import Partial.Unsafe (unsafePartial)
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
            (Pattern "Ogmios responded with error: " `indexOf` show error)
              -- Error: (TypeMismatch "Expected error code in a range [3000, 3999]")
              `shouldSatisfy` isJust

testGetChainTip :: QueryM Unit
testGetChainTip = do
  void getChainTip

testGetEraSummaries :: QueryM Unit
testGetEraSummaries = do
  void getEraSummaries

testSubmitTxFailure :: QueryM Unit
testSubmitTxFailure = do
  let
    someBytes = hexToByteArrayUnsafe
      "ffffffffffff55555555555555555555a1af1b7534b51e60fad3fe9c164313e8"
    txHash = TransactionHash $ unsafePartial $ fromJust $ fromBytes someBytes
  void $ submitTxOgmios txHash (wrap someBytes)

testGetCurrentEpoch :: QueryM Unit
testGetCurrentEpoch = do
  void getCurrentEpoch
