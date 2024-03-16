module Contract.Hashing where

import Cardano.Types (DataHash, Ed25519KeyHash, PlutusData, PublicKey)
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.PublicKey as PublicKey
import Prim.TypeError (class Warn, Text)

publicKeyHash
  :: Warn (Text "deprecated: publicKeyHash. use Cardano.Types.PublicKey.hash")
  => PublicKey
  -> Ed25519KeyHash
publicKeyHash = PublicKey.hash

datumHash
  :: Warn
       (Text "deprecated: datumHash. use Cardano.Types.DataHash.hashPlutusData")
  => PlutusData
  -> DataHash
datumHash = hashPlutusData
