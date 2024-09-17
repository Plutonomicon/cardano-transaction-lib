module Test.Ctl.Testnet.ClusterParameters
  ( mkSuite
  , runTest
  ) where

import Prelude

import Contract.Log (logDebug')
import Contract.Test (ContractTest, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Testnet (defaultTestnetConfig, testTestnetContracts)
import Ctl.Internal.Contract.Hooks (ClusterParameters)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Mote (group, test)
import Test.Spec.Assertions (shouldNotEqual)

runTest :: TestPlanM (Aff Unit) Unit
runTest = do
  clusterParamsRef <-
    liftEffect $ Ref.new
      { nodeSocketPath: mempty
      }
  testTestnetContracts
    defaultTestnetConfig
      { hooks = defaultTestnetConfig.hooks
          { onClusterStartup = Just (flip Ref.write clusterParamsRef)
          }
      }
    (mkSuite clusterParamsRef)

mkSuite :: Ref ClusterParameters -> TestPlanM ContractTest Unit
mkSuite ref = do
  group "ClusterParameters" do
    test "Reading cardano-testnet cluster parameters" do
      withWallets unit \_ -> do
        clusterParams <- liftEffect $ Ref.read ref
        clusterParams.nodeSocketPath `shouldNotEqual` mempty
        logDebug' $ "ClusterParameters: " <> show clusterParams
        pure unit
