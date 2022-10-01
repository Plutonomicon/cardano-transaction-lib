module Test.Ogmios.Address (suite) where

import Prelude

import Address (addressToOgmiosAddress, ogmiosAddressToAddress)
import Control.Monad.Except (throwError)
import Ctl.Internal.Test.Utils (TestPlanM)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff, error)
import Mote (group, test)
import QueryM.Ogmios (OgmiosAddress)
import Test.Spec.Assertions (shouldEqual)

testnetAddrFixture1 :: OgmiosAddress
testnetAddrFixture1 =
  "addr_test1qr7g8nrv76fc7k4ueqwecljxx9jfwvsgawhl55hck3n8uwaz26mpcwu58zdkhpdnc6nuq3fa8vylc8ak9qvns7r2dsysp7ll4d"

addrFixture1 :: OgmiosAddress
addrFixture1 =
  "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"

suite :: TestPlanM (Aff Unit) Unit
suite = do
  -- Test inverse in one direction.
  group "Address loop" do
    test "Ogmios Address to Address & back Testnet"
      $ testFromOgmiosAddress testnetAddrFixture1
    test "Ogmios Address to Address & back non-Testnet"
      $ testFromOgmiosAddress addrFixture1

testFromOgmiosAddress :: OgmiosAddress -> Aff Unit
testFromOgmiosAddress testAddr = do
  case ogmiosAddressToAddress testAddr of
    Nothing -> throwError $ error "Failed Address loop"
    Just addr -> addressToOgmiosAddress addr `shouldEqual` testAddr
