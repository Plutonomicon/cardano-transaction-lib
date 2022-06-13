module Test.Types.TokenName (suite) where

import Prelude

import Data.Traversable (for_)
import Mote (group, skip, test)
import Test.Utils (toFromAesonTest)
import TestM (TestPlanM)
import Types.ByteArray (hexToByteArrayUnsafe)
import Types.TokenName (mkTokenName)

suite :: TestPlanM Unit
suite = do
  group "Types.TokenName" $ do
    skip $ test "fromTokenName" (pure unit)
    group "Aeson tests" $ do
      group "Roundtrip tests"
        $ for_ tkNames
        $ toFromAesonTest "Address"
  where
  tkNames =
    [ mkTokenName $ hexToByteArrayUnsafe
        "388178ead6628e2ff3faae2148ec906eb686b3661549c8581cd427433ffd9cf3"
    ]
