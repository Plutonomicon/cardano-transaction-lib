module Test.AffInterface (suite) where

import Prelude

import Address (addressToOgmiosAddress, ogmiosAddressToAddress)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, test)
import QueryM (getChainTip, runQueryM, traceQueryConfig, getDatumByHash)
import Types.Transaction (DataHash(..))
import QueryM.Ogmios (OgmiosAddress)
import QueryM.Utxos (utxosAt)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Types.ByteArray (hexToByteArrayUnsafe)

testnet_addr1 :: OgmiosAddress
testnet_addr1 =
  "addr_test1qr7g8nrv76fc7k4ueqwecljxx9jfwvsgawhl55hck3n8uwaz26mpcwu58zdkhpdnc6nuq3fa8vylc8ak9qvns7r2dsysp7ll4d"

addr1 :: OgmiosAddress
addr1 =
  "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"

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
    test "Get ChainTip" $ testGetChainTip
  -- Test inverse in one direction.
  group "Address loop" do
    test "Ogmios Address to Address & back Testnet"
      $ testFromOgmiosAddress testnet_addr1
    test "Ogmios Address to Address & back non-Testnet"
      $ testFromOgmiosAddress addr1
  group "Ogmios datum cache" do
    test "Can process GetDatumByHash" do
      testOgmiosDatumCacheGetDatumByHash

testOgmiosDatumCacheGetDatumByHash :: Aff Unit
testOgmiosDatumCacheGetDatumByHash =
  traceQueryConfig >>= flip runQueryM do
    -- Use this to trigger block fetching in order to actually get the datum:
    -- ```
    -- curl localhost:9999/control/fetch_blocks -X POST -d '{"slot": 54066900, "id": "6eb2542a85f375d5fd6cbc1c768707b0e9fe8be85b7b1dd42a85017a70d2623d", "datumFilter": {"address": "addr_xyz"}}' -H 'Content-Type: application/json'
    -- ```
    _datum <- getDatumByHash $ DataHash $ hexToByteArrayUnsafe
      "f7c47c65216f7057569111d962a74de807de57e79f7efa86b4e454d42c875e4e"
    pure unit

testUtxosAt :: OgmiosAddress -> Aff Unit
testUtxosAt testAddr = case ogmiosAddressToAddress testAddr of
  Nothing -> liftEffect $ throw "Failed UtxosAt"
  Just addr -> flip runQueryM (utxosAt addr *> pure unit) =<< traceQueryConfig

testGetChainTip :: Aff Unit
testGetChainTip = do
  flip runQueryM (getChainTip *> pure unit) =<< traceQueryConfig

testFromOgmiosAddress :: OgmiosAddress -> Aff Unit
testFromOgmiosAddress testAddr = do
  liftEffect $ case ogmiosAddressToAddress testAddr of
    Nothing -> throw "Failed Address loop"
    Just addr -> addressToOgmiosAddress addr `shouldEqual` testAddr
