module Test.AffInterface (suite) where

import Prelude
import Control.Monad.Reader.Trans (runReaderT)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import TestM (TestPlanM)
import Mote (group, test)
import Ogmios (addressToOgmiosAddress, defaultServerConfig, mkOgmiosWebSocketAff, ogmiosAddressToAddress, utxosAt)
import Test.Spec.Assertions (shouldEqual)
import Types.JsonWsp (Address)

testnet_addr1 :: Address
testnet_addr1 = "addr_test1qr7g8nrv76fc7k4ueqwecljxx9jfwvsgawhl55hck3n8uwaz26mpcwu58zdkhpdnc6nuq3fa8vylc8ak9qvns7r2dsysp7ll4d"

addr1 :: Address
addr1 = "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"

-- note: currently this suite relies on Ogmios being open and running against the
-- testnet, and does not directly test outputs, as this suite is intended to
-- help verify that the Aff interface for websockets itself works,
-- not that the data represents expected values, as that would depend on chain
-- state, and ogmios itself.
suite :: TestPlanM Unit
suite = do
  -- Test UtxosAt using internal types.
  group "Aff Int" do
    test "UtxosAt Testnet" $ testUtxosAt testnet_addr1
    test "UtxosAt non-Testnet" $ testUtxosAt addr1
  -- Test inverse in one direction.
  group "Address loop" do
    test "Ogmios Address to Address & back Testnet"
      $ testFromOgmiosAddress testnet_addr1
    test "Ogmios Address to Address & back non-Testnet"
      $ testFromOgmiosAddress addr1

testUtxosAt :: Address -> Aff Unit
testUtxosAt testAddr = do
  ws <- mkOgmiosWebSocketAff "ws:127.0.0.1:1337"
  addr' <- liftEffect $ ogmiosAddressToAddress testAddr
  case addr' of
    Nothing -> liftEffect $ throw "Failed UtxosAt"
    Just addr -> runReaderT
      (utxosAt addr *> pure unit)
      { ws, serverConfig: defaultServerConfig }

testFromOgmiosAddress :: Address -> Aff Unit
testFromOgmiosAddress testAddr = do
  addr'' <- liftEffect $ ogmiosAddressToAddress testAddr
  liftEffect $ case addr'' of
    Nothing -> throw "Failed Address loop"
    Just addr' -> do
      addr <- addressToOgmiosAddress addr'
      addr `shouldEqual` testAddr
