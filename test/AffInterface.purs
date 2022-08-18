module Test.AffInterface (suite) where

import Prelude

import Address (addressToOgmiosAddress, ogmiosAddressToAddress)
import Contract.Chain (ChainTip(ChainTip), Tip(Tip, TipAtGenesis))
import Control.Monad.Except (throwError)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust)
import Data.Newtype (over, wrap)
import Data.String.CodeUnits (indexOf)
import Data.String.Pattern (Pattern(Pattern))
import Effect.Aff (try, error)
import Mote (group, test)
import QueryM
  ( QueryM
  , getChainTip
  , getDatumByHash
  , getDatumsByHashes
  , submitTxOgmios
  )
import QueryM.CurrentEpoch (getCurrentEpoch)
import QueryM.EraSummaries (getEraSummaries)
import QueryM.Ogmios (OgmiosAddress)
import QueryM.ProtocolParameters (getProtocolParameters)
import QueryM.SystemStart (getSystemStart)
import QueryM.Utxos (utxosAt)
import QueryM.WaitUntilSlot (waitUntilSlot)
import Serialization.Address (Slot(Slot))
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import TestM (TestPlanM)
import Types.BigNum (fromInt, add) as BigNum
import Types.ByteArray (hexToByteArrayUnsafe)
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
suite :: TestPlanM (QueryM Unit) Unit
suite = do
  -- Test UtxosAt using internal types.
  group "Aff Int" do
    test "UtxosAt Testnet" $ testUtxosAt testnet_addr1
    test "UtxosAt non-Testnet" $ testUtxosAt addr1
    test "Get ChainTip" testGetChainTip
    test "Get waitUntilSlot" testWaitUntilSlot
    test "Get EraSummaries" testGetEraSummaries
    test "Get ProtocolParameters" testGetProtocolParameters
    test "Get CurrentEpoch" testGetCurrentEpoch
    test "Get SystemStart" testGetSystemStart
  -- Test inverse in one direction.
  group "Address loop" do
    test "Ogmios Address to Address & back Testnet"
      $ testFromOgmiosAddress testnet_addr1
    test "Ogmios Address to Address & back non-Testnet"
      $ testFromOgmiosAddress addr1
  group "Ogmios error" do
    test "Ogmios fails with user-freindly message" do
      try testSubmitTxFailure >>= case _ of
        Right _ -> do
          void $ throwError $ error $
            "Unexpected success in testSubmitTxFailure"
        Left error -> do
          (Pattern "Server responded with `fault`" `indexOf` show error)
            `shouldSatisfy` isJust
  group "Ogmios datum cache" do
    test "Can process GetDatumByHash" do
      testOgmiosDatumCacheGetDatumByHash
    test "Can process GetDatumsByHashes" do
      testOgmiosDatumCacheGetDatumsByHashes

testOgmiosDatumCacheGetDatumByHash :: QueryM Unit
testOgmiosDatumCacheGetDatumByHash = do
  void $ getDatumByHash $ DataHash $ hexToByteArrayUnsafe
    "f7c47c65216f7057569111d962a74de807de57e79f7efa86b4e454d42c875e4e"

testOgmiosDatumCacheGetDatumsByHashes :: QueryM Unit
testOgmiosDatumCacheGetDatumsByHashes = do
  _datums <- getDatumsByHashes $ pure $ DataHash $ hexToByteArrayUnsafe
    "f7c47c65216f7057569111d962a74de807de57e79f7efa86b4e454d42c875e4e"
  pure unit

testUtxosAt :: OgmiosAddress -> QueryM Unit
testUtxosAt testAddr = case ogmiosAddressToAddress testAddr of
  Nothing -> throwError (error "Failed UtxosAt")
  Just addr -> void $ utxosAt addr

testGetChainTip :: QueryM Unit
testGetChainTip = do
  void getChainTip

testWaitUntilSlot :: QueryM Unit
testWaitUntilSlot = do
  void $ getChainTip >>= case _ of
    TipAtGenesis -> throwError $ error "Tip is at genesis"
    Tip (ChainTip { slot }) -> do
      waitUntilSlot $ over Slot
        (fromMaybe (BigNum.fromInt 0) <<< BigNum.add (BigNum.fromInt 10))
        slot

testFromOgmiosAddress :: OgmiosAddress -> QueryM Unit
testFromOgmiosAddress testAddr = do
  case ogmiosAddressToAddress testAddr of
    Nothing -> throwError $ error "Failed Address loop"
    Just addr -> addressToOgmiosAddress addr `shouldEqual` testAddr

testGetEraSummaries :: QueryM Unit
testGetEraSummaries = do
  void getEraSummaries

testSubmitTxFailure :: QueryM Unit
testSubmitTxFailure = do
  void $ submitTxOgmios (wrap $ hexToByteArrayUnsafe "00")

testGetProtocolParameters :: QueryM Unit
testGetProtocolParameters = do
  void getProtocolParameters

testGetCurrentEpoch :: QueryM Unit
testGetCurrentEpoch = do
  void getCurrentEpoch

testGetSystemStart :: QueryM Unit
testGetSystemStart = do
  void getSystemStart
