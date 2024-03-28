module Contract.Hashing where

import Cardano.Types
  ( DataHash
  , Ed25519KeyHash
  , NativeScript
  , PlutusData
  , PublicKey
  )
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.PublicKey as PublicKey
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.ScriptRef (ScriptRef(PlutusScriptRef, NativeScriptRef))
import Prim.TypeError (class Warn, Text)

nativeScriptHash
  :: Warn (Text "deprecated: publicKeyHash. use Cardano.Types.PublicKey.hash")
  => NativeScript
  -> ScriptHash
nativeScriptHash = NativeScript.hash

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

scriptRefHash :: ScriptRef -> ScriptHash
scriptRefHash (PlutusScriptRef ps) = PlutusScript.hash ps
scriptRefHash (NativeScriptRef ns) = NativeScript.hash ns
