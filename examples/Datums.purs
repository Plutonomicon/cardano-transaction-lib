-- | An example of fetching datums from `ogmios-datum-cache`. Helpful to test
-- | out the datum-cache integration
module Examples.Datums (main) where

import Contract.Prelude

import Contract.Monad ( defaultContractConfig, launchAff_, logInfo, runContract_)
import Contract.PlutusData (DatumHash, getDatumByHash, getDatumsByHashes)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Data.Map as Map
import Data.Newtype (wrap)

main :: Effect Unit
main = launchAff_ $ do
  cfg <- defaultContractConfig
  runContract_ cfg $ do
    logInfo Map.empty <<< show =<< getDatumByHash
      ( mkDatumHash
          "42be572a6d9a8a2ec0df04f14b0d4fcbe4a7517d74975dfff914514f12316252"
      )
    logInfo Map.empty <<< show =<< getDatumsByHashes
      [ mkDatumHash
          "777093fe6dfffdb3bd2033ad71745f5e2319589e36be4bc9c8cca65ac2bfeb8f"
      , mkDatumHash
          "e8cb7d18e81b0be160c114c563c020dcc7bf148a1994b73912db3ea1318d488b"
      ]
  where
  mkDatumHash :: String -> DatumHash
  mkDatumHash = wrap <<< hexToByteArrayUnsafe
