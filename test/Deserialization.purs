module Test.Ctl.Deserialization (suite) where

import Prelude

import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Types (TransactionWitnessSet)
import Contract.Prim.ByteArray (ByteArray)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Ctl.Fixtures
  ( nativeScriptFixture1
  , nativeScriptFixture2
  , nativeScriptFixture3
  , nativeScriptFixture4
  , nativeScriptFixture5
  , nativeScriptFixture6
  , nativeScriptFixture7
  , plutusDataFixture1
  , plutusDataFixture2
  , plutusDataFixture3
  , plutusDataFixture4
  , plutusDataFixture5
  , plutusDataFixture6
  , plutusDataFixture7
  , plutusDataFixture8
  , txFixture1
  , txFixture2
  , txFixture3
  , txFixture4
  , txFixture5
  , txFixture6
  , txInputFixture1
  , txOutputFixture1
  , witnessSetFixture1
  , witnessSetFixture2
  , witnessSetFixture2Value
  , witnessSetFixture3
  , witnessSetFixture3Value
  , witnessSetFixture4
  , mint0
  , mint1
  , int1
  )
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(Proxy))

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "deserialization and serialization roundtrip" $ do
    group "NativeScript" do
      roundtripTest "nativeScriptFixture1" nativeScriptFixture1
      roundtripTest "nativeScriptFixture1" nativeScriptFixture2
      roundtripTest "nativeScriptFixture3" nativeScriptFixture3
      roundtripTest "nativeScriptFixture4" nativeScriptFixture4
      roundtripTest "nativeScriptFixture5" nativeScriptFixture5
      roundtripTest "nativeScriptFixture6" nativeScriptFixture6
      roundtripTest "nativeScriptFixture7" nativeScriptFixture7
    group "PlutusData" do
      roundtripTest "plutusDataFixture1" plutusDataFixture1
      roundtripTest "plutusDataFixture2" plutusDataFixture2
      roundtripTest "plutusDataFixture3" plutusDataFixture3
      roundtripTest "plutusDataFixture4" plutusDataFixture4
      roundtripTest "plutusDataFixture5" plutusDataFixture5
      roundtripTest "plutusDataFixture6" plutusDataFixture6
      roundtripTest "plutusDataFixture7" plutusDataFixture7
      roundtripTest "plutusDataFixture8" plutusDataFixture8
    group "Transaction" do
      roundtripTest "txFixture1" txFixture1
      roundtripTest "txFixture2" txFixture2
      roundtripTest "txFixture3" txFixture3
      -- roundtripTest "txFixture4" txFixture4
      roundtripTest "txFixture5" txFixture5
      roundtripTest "txFixture6" txFixture6
    group "TransactionInput" do
      roundtripTest "txInputFixture1" txInputFixture1
    group "Int" do
      roundtripTest "int0" int1
    -- group "Mint" do
    --   roundtripTest "mint1" mint1
      -- roundtripTest "mint0" mint0
    group "TransactionOutput" do
      roundtripTest "txOutputFixture1" txOutputFixture1
    group "TransactionWitnessSet" do
      roundtripTest "witnessSetFixture2Value" witnessSetFixture2Value
      roundtripTest "witnessSetFixture3Value" witnessSetFixture3Value
      roundtripTestBytes "witnessSetFixture1"
        (Proxy :: Proxy TransactionWitnessSet)
        witnessSetFixture1
      roundtripTestBytes "witnessSetFixture2"
        (Proxy :: Proxy TransactionWitnessSet)
        witnessSetFixture2
      roundtripTestBytes "witnessSetFixture3"
        (Proxy :: Proxy TransactionWitnessSet)
        witnessSetFixture3
      roundtripTestBytes "witnessSetFixture4"
        (Proxy :: Proxy TransactionWitnessSet)
        witnessSetFixture4

roundtripTest
  :: forall a
   . Eq a
  => Show a
  => AsCbor a
  => String
  -> a
  -> TestPlanM (Aff Unit) Unit
roundtripTest label a =
  test ("Deserialization is inverse to serialization: " <> label) do
    decodeCbor (encodeCbor a) `shouldEqual` Just a

roundtripTestBytes
  :: forall a
   . Eq a
  => Show a
  => AsCbor a
  => String
  -> Proxy a
  -> ByteArray
  -> TestPlanM (Aff Unit) Unit
roundtripTestBytes label _ bytes = do
  test ("Serialization is inverse to deserialization: " <> label) do
    (encodeCbor <$> (decodeCbor (wrap bytes) :: Maybe a)) `shouldEqual` Just
      (wrap bytes)

plutipSuccessResponseFixture :: String
plutipSuccessResponseFixture =
  """
{"contents":{"keysDirectory":"/tmp/nix-shell.xvz2Lq/test-cluster149328/signing-keys","nodeConfigPath":"/tmp/nix-shell.xvz2Lq/test-cluster149328/pool-3/node.config","nodeSocketPath":"/tmp/nix-shell.xvz2Lq/test-cluster149328/pool-3/node.socket","privateKeys":["e61e29b40ba4e5a3100363d86669f3cb604ea3fc971b43510c97daeb9bf2e4e2","9cd3bd5deb0a04ef2e20160b85b889f32dfc66d2f7b7a0fac5aec3b1ad950227"]},"tag":"ClusterStartupSuccess"}
"""
