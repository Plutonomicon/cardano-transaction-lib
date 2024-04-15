module Contract.Types.MintingPolicy
  ( MintingPolicy(PlutusMintingPolicy, NativeMintingPolicy)
  , hash
  , toScriptRef
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson)
import Cardano.Types (ScriptHash)
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptRef (ScriptRef(..))
import Control.Alt ((<|>))
import Ctl.Internal.Helpers (decodeTaggedNewtype)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | `MintingPolicy` is a sum type of `PlutusScript` and `NativeScript` which are used as
-- | validators for minting constraints.
data MintingPolicy
  = PlutusMintingPolicy PlutusScript
  | NativeMintingPolicy NativeScript

derive instance Generic MintingPolicy _
derive instance Eq MintingPolicy
derive instance Ord MintingPolicy

instance DecodeAeson MintingPolicy where
  decodeAeson aes =
    decodeTaggedNewtype "getPlutusMintingPolicy" PlutusMintingPolicy aes <|>
      decodeTaggedNewtype "getNativeMintingPolicy" NativeMintingPolicy aes

instance EncodeAeson MintingPolicy where
  encodeAeson (NativeMintingPolicy nscript) =
    encodeAeson { "getNativeMintingPolicy": nscript }
  encodeAeson (PlutusMintingPolicy script) =
    encodeAeson { "getPlutusMintingPolicy": script }

instance Show MintingPolicy where
  show = genericShow

hash :: MintingPolicy -> ScriptHash
hash (PlutusMintingPolicy ps) = PlutusScript.hash ps
hash (NativeMintingPolicy ns) = NativeScript.hash ns

toScriptRef :: MintingPolicy -> ScriptRef
toScriptRef = case _ of
  PlutusMintingPolicy ps -> PlutusScriptRef ps
  NativeMintingPolicy ns -> NativeScriptRef ns
