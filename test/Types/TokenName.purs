module Test.Types.TokenName (suite) where

import Prelude

import Data.Traversable (for_)
import Mote (group)
import Test.Utils (toFromAesonTest)
import TestM (TestPlanM)
import Types.ByteArray (hexToByteArrayUnsafe)
import Types.TokenName (mkTokenName)

suite :: TestPlanM Unit
suite = do
  group "Types.TokenName" $ do
    group "Aeson tests" $ do
      group "Roundtrip tests for invalid UTF byte sequences"
        $ for_ tkNamesWithInvalidUtf8
        $ \tkn -> toFromAesonTest
            ( show tkn <>
                " should roundtrip successfully (regression to https://github.com/Plutonomicon/cardano-transaction-lib/issues/544 ?)"
            )
            tkn
  where
  tkNamesWithInvalidUtf8 = mkTokenName <<< hexToByteArrayUnsafe <$>
    [ "388178ead6628e2ff3faae2148ec906eb686b3661549c8581cd427433ffd9cf3"
    -- NOTE: Adopted from https://www.php.net/manual/en/reference.pcre.pattern.modifiers.php#54805
    , "c328"
    , "a0a1"
    , "e228a1"
    , "e28228"
    , "f0288cbc"
    , "f09028bc"
    , "f8a1a1a1a1"
    , "fca1a1a1a1a1"
    ]
