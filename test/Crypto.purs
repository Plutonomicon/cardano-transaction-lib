module Test.Crypto (suite) where

import Prelude

import Contract.Prim.ByteArray (byteArrayToHex)
import Data.Maybe (Maybe)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.Class (liftAff)
import Mote (group, test)
import QueryM (QueryM, runQueryM, traceQueryConfig)
import QueryM.Crypto (plutusHash, HashMethod(..))
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (errMaybe)
import TestM (TestPlanM)
import Types.ByteArray (ByteArray, hexToByteArray)

suite :: TestPlanM Unit
suite = do
  group "Contract.Crypto"
    $ test "plutusHash"
    $ do
        qcf <- liftAff $ traceQueryConfig
        liftAff $ runQueryM qcf $ do
          mkReqs sourceHashPairs
  where
  mkReqs
    :: Array (Tuple (Maybe ByteArray) (Array (Tuple HashMethod String)))
    -> QueryM Unit
  mkReqs = traverse_
    ( \(Tuple origBS hashtups) ->
        traverse_
          ( \(Tuple meth goal) -> do
              qbs <- errMaybe "bytestring is not hex format" origBS
              res <- errMaybe "hashing method was not successfull" =<<
                plutusHash meth qbs
              shouldEqual goal $ byteArrayToHex res
          )
          hashtups
    )

sourceHashPairs
  :: Array (Tuple (Maybe ByteArray) (Array (Tuple HashMethod String)))
sourceHashPairs =
  [ Tuple (hexToByteArray "68656C6C6F20776F726C64")
      [ Tuple Sha3_256
          "644bcc7e564373040999aac89e7622f3ca71fba1d972fd94a31c3bfbf24e3938"
      , Tuple Sha2_256
          "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
      , Tuple Blake2b_256
          "256c83b297114d201b30179f3f0ef0cace9783622da5974326b436178aeef610"
      ]
  ]
