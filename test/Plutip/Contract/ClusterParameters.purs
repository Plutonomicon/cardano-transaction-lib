module Test.Ctl.Plutip.Contract.ClusterParameters (mkSuite, runTest) where

import Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Test (ContractTest, InitialUTxOs, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip (testPlutipContracts)
import Ctl.Internal.Contract.Hooks (ClusterParameters)
import Data.Array as Array
import Data.Maybe (Maybe(Just))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Mote (group, test)
import Test.Ctl.Plutip.Common (config)
import Test.Spec.Assertions (shouldEqual)

runTest :: TestPlanM (Aff Unit) Unit
runTest =
  do
    clusterParamsRef <-
      liftEffect $ Ref.new
        { privateKeys: []
        , nodeSocketPath: ""
        , nodeConfigPath: ""
        , privateKeysDirectory: ""
        }
    testPlutipContracts
      config
        { hooks = config.hooks
            { onClusterStartup = Just (flip Ref.write clusterParamsRef) }
        }
      (mkSuite clusterParamsRef)

mkSuite :: Ref ClusterParameters -> TestPlanM ContractTest Unit
mkSuite ref = do
  group "Getting ClusterParameters" do
    let
      initialUtxos :: InitialUTxOs
      initialUtxos =
        [ BigNum.fromInt 2_000_000_000, BigNum.fromInt 2_000_000_000 ]

      distribution :: InitialUTxOs /\ InitialUTxOs
      distribution = initialUtxos /\ initialUtxos
    test "getting cluster parameters" do
      withWallets distribution \(_alice /\ _bob) -> do
        clusterParams <- liftEffect $ Ref.read ref
        Array.length clusterParams.privateKeys `shouldEqual` 2
        pure unit
