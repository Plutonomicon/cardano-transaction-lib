module Test.Ctl.NetworkId
  ( suite
  ) where

import Prelude

import Contract.Address (addressFromBech32) as Address
import Contract.Config (testnetConfig)
import Contract.Monad (runContract)
import Data.Maybe (Maybe(Nothing))
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Ctl.TestM (TestPlanM)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = group "NetworkId Tests" $ do
  test "Testnet Address in Testnet Env" testTestnetAddress
  test "Mainnet Address in Testnet Env" testMainnetAddress

testTestnetAddress :: Aff Unit
testTestnetAddress = runContract testnetConfig do
  addr <- Address.addressFromBech32
    "addr_test1qqm0z9quxyefwwq902p5f9t4s35smhegjthhhqpeclnpx2rzhuq2p6jahnky7qqua9nz9tcw6nlgy6cpjvmlaaye4apqzc6ppq"
  addr `shouldNotEqual` Nothing

testMainnetAddress :: Aff Unit
testMainnetAddress = runContract testnetConfig do
  addr <- Address.addressFromBech32
    "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"
  addr `shouldEqual` Nothing
