module Test.Ctl.Plutip.Contract.NetworkId
  ( suite
  ) where

import Prelude

import Contract.Address (addressFromBech32) as Address
import Contract.Test (ContractTest)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip (noWallet)
import Ctl.Internal.Plutus.Conversion.Address (fromPlutusAddress)
import Ctl.Internal.Serialization.Address (NetworkId(MainnetId), addressBech32)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM ContractTest Unit
suite = group "NetworkId Tests" $ do
  test "Mainnet Address in Mainnet Env" testMainnetAddress

testMainnetAddress :: ContractTest
testMainnetAddress = noWallet do
  let
    bechstr =
      "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"
  addr <- Address.addressFromBech32 bechstr
  addressBech32 (fromPlutusAddress MainnetId addr) `shouldEqual` bechstr
