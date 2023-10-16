module Test.Ctl.Hashing (suite) where

import Prelude

import Ctl.Internal.Hashing
  ( blake2b256Hash
  , blake2b256HashHex
  , datumHash
  , plutusScriptHash
  , sha256Hash
  , sha256HashHex
  , sha3_256Hash
  , sha3_256HashHex
  ) as Hashing
import Ctl.Internal.Serialization.Hash (ScriptHash, scriptHashFromBytes)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.ByteArray
  ( ByteArray
  , byteArrayFromAscii
  , hexToByteArrayUnsafe
  )
import Ctl.Internal.Types.PlutusData (PlutusData(Integer))
import Ctl.Internal.Types.Scripts (PlutusScript, plutusV1Script, plutusV2Script)
import Ctl.Internal.Types.Transaction (DataHash)
import Data.Maybe (fromJust)
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import JS.BigInt (fromInt)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Fixtures (plutusDataFixture7)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "Hashing" do
    test "blake2b256 hash of an arbitrary byte array" do
      Hashing.blake2b256Hash inputDataFixture
        `shouldEqual` (hexToByteArrayUnsafe blake2b256HexDigestFixture)

    test "blake2b256 hash of an arbitrary byte array as a hex string" do
      Hashing.blake2b256HashHex inputDataFixture
        `shouldEqual` blake2b256HexDigestFixture

    test "blake2b224 hash of a PlutusV1 script" do
      Hashing.plutusScriptHash plutusV1ScriptFixture
        `shouldEqual` plutusV1ScriptHashFixture

    test "blake2b224 hash of a PlutusV2 script" do
      Hashing.plutusScriptHash plutusV2ScriptFixture
        `shouldEqual` plutusV2ScriptHashFixture

    test "blake2b256 hash of Plutus data" do
      Hashing.datumHash (wrap plutusDataFixture7)
        `shouldEqual` datumHashFixture
    test
      "blake2b256 hash of Plutus data - Integer 0 (regression to \
      \https://github.com/Plutonomicon/cardano-transaction-lib/issues/488 ?)"
      do
        Hashing.datumHash (wrap $ Integer (fromInt 0))
          `shouldEqual` zeroIntDatumHashFixture

    test "sha256 hash of an arbitrary byte array" do
      Hashing.sha256Hash inputDataFixture
        `shouldEqual` (hexToByteArrayUnsafe sha256HexDigestFixture)

    test "sha256 hash of an arbitrary byte array as a hex string" do
      Hashing.sha256HashHex inputDataFixture
        `shouldEqual` sha256HexDigestFixture

    test "sha3_256 hash of an arbitrary byte array" do
      Hashing.sha3_256Hash inputDataFixture
        `shouldEqual` (hexToByteArrayUnsafe sha3_256HexDigestFixture)

    test "sha3_256 hash of an arbitrary byte array as a hex string" do
      Hashing.sha3_256HashHex inputDataFixture
        `shouldEqual` sha3_256HexDigestFixture

inputDataFixture :: ByteArray
inputDataFixture =
  unsafePartial $ fromJust $ byteArrayFromAscii "some message"

blake2b256HexDigestFixture :: String
blake2b256HexDigestFixture =
  "e04294b4b3a1b6e031af82eca8847d2c7be14f588fe326b78a7cc5060da35480"

sha256HexDigestFixture :: String
sha256HexDigestFixture =
  "c47757abe4020b9168d0776f6c91617f9290e790ac2f6ce2bd6787c74ad88199"

sha3_256HexDigestFixture :: String
sha3_256HexDigestFixture =
  "832978c1119fe706ddfe32de31f1986ba3654198b3b7656b050e2ab3b1fe3de3"

-- Checked that it corresponds to blake2b256(\00) ie. Integer 0
zeroIntDatumHashFixture :: DataHash
zeroIntDatumHashFixture =
  wrap $
    hexToByteArrayUnsafe
      "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314"

datumHashFixture :: DataHash
datumHashFixture =
  wrap $
    hexToByteArrayUnsafe
      "0ba47e574456db8938e56f889d4c30099256f96008e0d4b6c4688f47ec342c9d"

plutusV1ScriptFixture :: PlutusScript
plutusV1ScriptFixture =
  plutusV1Script $
    hexToByteArrayUnsafe "4d01000033222220051200120011"

plutusV1ScriptHashFixture :: ScriptHash
plutusV1ScriptHashFixture =
  unsafePartial $ fromJust $ scriptHashFromBytes $
    hexToByteArrayUnsafe
      "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"

plutusV2ScriptFixture :: PlutusScript
plutusV2ScriptFixture =
  plutusV2Script
    $ hexToByteArrayUnsafe "4d01000033222220051200120011"

plutusV2ScriptHashFixture :: ScriptHash
plutusV2ScriptHashFixture =
  unsafePartial $ fromJust $ scriptHashFromBytes $
    hexToByteArrayUnsafe
      "793f8c8cffba081b2a56462fc219cc8fe652d6a338b62c7b134876e7"
