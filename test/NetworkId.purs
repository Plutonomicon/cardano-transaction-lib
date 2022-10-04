module Test.Ctl.NetworkId
  ( suite
  ) where

import Prelude

import Contract.Address (addressFromBech32) as Address
import Contract.Config (testnetConfig)
import Contract.Monad (runContract)
import Ctl.Internal.Plutus.Conversion.Address (fromPlutusAddress)
import Ctl.Internal.Serialization.Address (NetworkId(TestnetId), addressBech32)
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Ctl.TestM (TestPlanM)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = group "NetworkId Tests" $ do
  test "Testnet Address in Testnet Env" testTestnetAddress

testTestnetAddress :: Aff Unit
testTestnetAddress = runContract testnetConfig do
  let
    bechstr =
      "addr_test1qqm0z9quxyefwwq902p5f9t4s35smhegjthhhqpeclnpx2rzhuq2p6jahnky7qqua9nz9tcw6nlgy6cpjvmlaaye4apqzc6ppq"
  addr <- Address.addressFromBech32 bechstr
  addressBech32 (fromPlutusAddress TestnetId addr) `shouldEqual` bechstr
