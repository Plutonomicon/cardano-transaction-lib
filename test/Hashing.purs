module Test.Hashing (suite) where

import Prelude

import Data.Maybe (Maybe(Just), fromJust)
import Data.Newtype (wrap)
import Hashing
  ( blake2b256Hash
  , blake2b256HashHex
  , hashDatum
  , hashPlutusScript
  ) as Hashing
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Serialization.Hash (ScriptHash, scriptHashFromBytes)
import TestM (TestPlanM)
import Test.Fixtures (plutusDataFixture7)
import Test.Spec.Assertions (shouldEqual)
import Types.ByteArray (ByteArray, byteArrayFromAscii, hexToByteArrayUnsafe)
import Types.RawBytes (hexToRawBytesUnsafe)
import Types.Scripts (PlutusScript)
import Types.Transaction (DataHash)

suite :: TestPlanM Unit
suite = do
  group "Hashing" do
    test "blake2b256 hash of an arbitrary byte array" do
      Hashing.blake2b256Hash arbitraryDataFixture
        `shouldEqual` arbitraryDataHashFixture

    test "blake2b256 hash of an arbitrary byte array as a hex string" do
      Hashing.blake2b256HashHex arbitraryDataFixture
        `shouldEqual` arbitraryDataHashHexStringFixture

    test "blake2b256 hash of Plutus data" do
      Hashing.hashDatum (wrap plutusDataFixture7)
        `shouldEqual` Just datumHashFixture

    test "blake2b224 hash of a Plutus script" do
      Hashing.hashPlutusScript plutusScriptFixture
        `shouldEqual` Just plutusScriptHashFixture

arbitraryDataFixture :: ByteArray
arbitraryDataFixture =
  unsafePartial $ fromJust $ byteArrayFromAscii "some message"

arbitraryDataHashHexStringFixture :: String
arbitraryDataHashHexStringFixture =
  "e04294b4b3a1b6e031af82eca8847d2c7be14f588fe326b78a7cc5060da35480"

arbitraryDataHashFixture :: ByteArray
arbitraryDataHashFixture =
  hexToByteArrayUnsafe arbitraryDataHashHexStringFixture

datumHashFixture :: DataHash
datumHashFixture =
  wrap $
    hexToByteArrayUnsafe
      "0ba47e574456db8938e56f889d4c30099256f96008e0d4b6c4688f47ec342c9d"

plutusScriptFixture :: PlutusScript
plutusScriptFixture =
  wrap $
    hexToByteArrayUnsafe "4d01000033222220051200120011"

plutusScriptHashFixture :: ScriptHash
plutusScriptHashFixture =
  unsafePartial $ fromJust $ scriptHashFromBytes $
    hexToRawBytesUnsafe
      "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
