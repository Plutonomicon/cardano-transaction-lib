module Test.Ctl.Plutip.NetworkId
  ( suite
  ) where

import Prelude

import Contract.Address (addressFromBech32) as Address
import Contract.Test.Plutip (runPlutipContract)
import Ctl.Internal.Plutus.Conversion.Address (fromPlutusAddress)
import Ctl.Internal.Serialization.Address (NetworkId(MainnetId), addressBech32)
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Ctl.Plutip.Common (config)
import Test.Ctl.TestM (TestPlanM)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = group "NetworkId Tests" $ do
  test "Mainnet Address in Mainnet Env" testMainnetAddress

testMainnetAddress :: Aff Unit
testMainnetAddress = runPlutipContract config unit \_ -> do
  let
    bechstr =
      "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"
  addr <- Address.addressFromBech32 bechstr
  addressBech32 (fromPlutusAddress MainnetId addr) `shouldEqual` bechstr
