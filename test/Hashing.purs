module Test.Ctl.Hashing (suite) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types (ScriptHash)
import Cardano.Types.DataHash (DataHash)
import Cardano.Types.PlutusData (PlutusData(Integer))
import Cardano.Types.PlutusScript (plutusV1Script, plutusV2Script)
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Hashing (datumHash) as Hashing
import Contract.Scripts (PlutusScript)
import Data.ByteArray (hexToByteArrayUnsafe)
import Data.Maybe (fromJust)
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import JS.BigInt (fromInt)
import Mote (group, test)
import Mote.TestPlanM (TestPlanM)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Fixtures (plutusDataFixture7)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "Hashing" do

    test "blake2b224 hash of a PlutusV1 script" do
      PlutusScript.hash plutusV1ScriptFixture
        `shouldEqual` plutusV1ScriptHashFixture

    test "blake2b224 hash of a PlutusV2 script" do
      PlutusScript.hash plutusV2ScriptFixture
        `shouldEqual` plutusV2ScriptHashFixture

    test "blake2b256 hash of Plutus data" do
      Hashing.datumHash plutusDataFixture7
        `shouldEqual` datumHashFixture
    test
      "blake2b256 hash of Plutus data - Integer 0 (regression to \
      \https://github.com/Plutonomicon/cardano-transaction-lib/issues/488 ?)"
      do
        Hashing.datumHash (Integer (fromInt 0))
          `shouldEqual` zeroIntDatumHashFixture

-- Checked that it corresponds to blake2b256(\00) ie. Integer 0
zeroIntDatumHashFixture :: DataHash
zeroIntDatumHashFixture =
  unsafePartial $ fromJust $ decodeCbor $ wrap $
    hexToByteArrayUnsafe
      "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314"

datumHashFixture :: DataHash
datumHashFixture =
  unsafePartial $ fromJust $ decodeCbor $ wrap $
    hexToByteArrayUnsafe
      "0ba47e574456db8938e56f889d4c30099256f96008e0d4b6c4688f47ec342c9d"

plutusV1ScriptFixture :: PlutusScript
plutusV1ScriptFixture =
  plutusV1Script $ wrap $
    hexToByteArrayUnsafe "4d01000033222220051200120011"

plutusV1ScriptHashFixture :: ScriptHash
plutusV1ScriptHashFixture =
  unsafePartial $ fromJust $ decodeCbor $ wrap $
    hexToByteArrayUnsafe
      "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"

plutusV2ScriptFixture :: PlutusScript
plutusV2ScriptFixture =
  plutusV2Script $ wrap
    $ hexToByteArrayUnsafe "4d01000033222220051200120011"

plutusV2ScriptHashFixture :: ScriptHash
plutusV2ScriptHashFixture =
  unsafePartial $ fromJust $ decodeCbor $ wrap $
    hexToByteArrayUnsafe
      "793f8c8cffba081b2a56462fc219cc8fe652d6a338b62c7b134876e7"
