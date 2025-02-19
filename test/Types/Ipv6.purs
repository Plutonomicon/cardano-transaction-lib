module Test.Ctl.Types.Ipv6
  ( suite
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Ctl.Internal.QueryM.Ogmios.Types (parseIpv6String)
import Data.ByteArray (hexToByteArrayUnsafe)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Mote (group, test)
import Mote.TestPlanM (TestPlanM)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Ipv6 type (parsing)" do
    testIpv6 "2345:425:2CA1:0000:0000:567:5673:23b5"
      "50234504252CA1000000000567567323b5"
    testIpv6 "2345:0425:2CA1:0:0:0567:5673:23b5"
      "50234504252CA1000000000567567323b5"
    testIpv6 "2345:0425:2CA1::0567:5673:23b5"
      "50234504252CA1000000000567567323b5"
    testIpv6 "2345:0425:2CA1::5673:23b5" "50234504252CA1000000000000567323b5"
    testIpv6 "2345:0425:2CA1::23b5" "50234504252CA1000000000000000023b5"

testIpv6 :: String -> String -> TestPlanM (Aff Unit) Unit
testIpv6 str expected =
  test str do
    let ipv6 = parseIpv6String str
    ipv6 `shouldNotEqual` Nothing
    ipv6 `shouldEqual` (decodeCbor (wrap $ hexToByteArrayUnsafe expected))
