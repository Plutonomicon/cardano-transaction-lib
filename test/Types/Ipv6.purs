module Test.Ctl.Types.Ipv6
  ( suite
  ) where

import Prelude

import Contract.Prim.ByteArray (hexToByteArray)
import Ctl.Internal.Cardano.Types.Transaction (Ipv6(Ipv6))
import Ctl.Internal.QueryM.Pools (parseIpv6String)
import Effect.Aff (Aff)
import Mote (group, test)
import Test.Ctl.TestM (TestPlanM)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Ipv6 type (parsing)" do
    testIpv6 "2345:425:2CA1:0000:0000:567:5673:23b5"
      "234504252CA1000000000567567323b5"
    testIpv6 "2345:0425:2CA1:0:0:0567:5673:23b5"
      "234504252CA1000000000567567323b5"
    testIpv6 "2345:0425:2CA1::0567:5673:23b5" "234504252CA1000000000567567323b5"
    testIpv6 "2345:0425:2CA1::5673:23b5" "234504252CA1000000000000567323b5"
    testIpv6 "2345:0425:2CA1::23b5" "234504252CA1000000000000000023b5"

testIpv6 :: String -> String -> TestPlanM (Aff Unit) Unit
testIpv6 str expected =
  test str do
    parseIpv6String str `shouldEqual` (Ipv6 <$> hexToByteArray expected)
