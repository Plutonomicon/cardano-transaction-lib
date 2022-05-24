module Test.AffInterface (suite) where

import Prelude

import Address (addressToOgmiosAddress, ogmiosAddressToAddress)
import Contract.Address (Slot(Slot))
import Data.Maybe (Maybe(Just, Nothing))
import Data.UInt as UInt
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, test)
import QueryM
  ( cancelFetchBlocks
  , getChainTip
  , getDatumByHash
  , getDatumsByHashes
  , runQueryM
  , startFetchBlocks
  , traceQueryConfig
  )
import QueryM.CurrentEpoch (getCurrentEpoch)
import QueryM.EraSummaries (getEraSummaries)
import QueryM.Ogmios (OgmiosAddress)
import QueryM.Utxos (utxosAt)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Types.ByteArray (hexToByteArrayUnsafe)
import Types.Chain (BlockHeaderHash(BlockHeaderHash))
import Types.Transaction (DataHash(DataHash))

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
    test "Get ChainTip" testGetChainTip
    test "Get EraSummaries" testGetEraSummaries
    test "Get CurentEpoch" testGetCurrentEpoch
  -- Test inverse in one direction.
  group "Address loop" do
    test "Ogmios Address to Address & back Testnet"
      $ testFromOgmiosAddress testnet_addr1
    test "Ogmios Address to Address & back non-Testnet"
      $ testFromOgmiosAddress addr1
  group "Ogmios datum cache" do
    test "Can process GetDatumByHash" do
      testOgmiosDatumCacheGetDatumByHash
    test "Can process GetDatumsByHashes" do
      testOgmiosDatumCacheGetDatumsByHashes
    test "Fetcher works" do
      testOgmiosDatumCacheFetcher

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

testOgmiosDatumCacheGetDatumsByHashes :: Aff Unit
testOgmiosDatumCacheGetDatumsByHashes =
  traceQueryConfig >>= flip runQueryM do
    -- Use this to trigger block fetching in order to actually get the datum:
    -- ```
    -- curl localhost:9999/control/fetch_blocks -X POST -d '{"slot": 54066900, "id": "6eb2542a85f375d5fd6cbc1c768707b0e9fe8be85b7b1dd42a85017a70d2623d", "datumFilter": {"address": "addr_xyz"}}' -H 'Content-Type: application/json'
    -- ```
    _datums <- getDatumsByHashes $ pure $ DataHash $ hexToByteArrayUnsafe
      "f7c47c65216f7057569111d962a74de807de57e79f7efa86b4e454d42c875e4e"
    pure unit

testOgmiosDatumCacheFetcher :: Aff Unit
testOgmiosDatumCacheFetcher =
  traceQueryConfig >>= flip runQueryM do
    void $ try cancelFetchBlocks -- ignore error if the fetcher was not running
    startFetchBlocks
      { slot: Slot (UInt.fromInt 54066900)
      , id: BlockHeaderHash
          "6eb2542a85f375d5fd6cbc1c768707b0e9fe8be85b7b1dd42a85017a70d2623d"
      }
    cancelFetchBlocks

testUtxosAt :: OgmiosAddress -> Aff Unit
testUtxosAt testAddr = case ogmiosAddressToAddress testAddr of
  Nothing -> liftEffect $ throw "Failed UtxosAt"
  Just addr -> flip runQueryM (utxosAt addr $> unit) =<< traceQueryConfig

testGetChainTip :: Aff Unit
testGetChainTip = do
  flip runQueryM (getChainTip $> unit) =<< traceQueryConfig

testFromOgmiosAddress :: OgmiosAddress -> Aff Unit
testFromOgmiosAddress testAddr = do
  liftEffect $ case ogmiosAddressToAddress testAddr of
    Nothing -> throw "Failed Address loop"
    Just addr -> addressToOgmiosAddress addr `shouldEqual` testAddr

testGetEraSummaries :: Aff Unit
testGetEraSummaries = do
  flip runQueryM (getEraSummaries $> unit) =<< traceQueryConfig

testGetCurrentEpoch :: Aff Unit
testGetCurrentEpoch = do
  flip runQueryM (getCurrentEpoch $> unit) =<< traceQueryConfig