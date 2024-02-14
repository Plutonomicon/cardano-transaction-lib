module Cardano.Types.PlutusScript where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib
  ( fromBytes
  , plutusScript_bytes
  , plutusScript_languageVersion
  , plutusScript_newWithVersion
  , toBytes
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (class AsCbor)
import Cardano.Types.Language (Language(PlutusV1, PlutusV2))
import Cardano.Types.Language as Language
import Data.ByteArray (ByteArray)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))

-- | Corresponds to "Script" in Plutus
newtype PlutusScript = PlutusScript (ByteArray /\ Language)

derive instance Generic PlutusScript _
derive instance Newtype PlutusScript _
derive newtype instance Eq PlutusScript
derive newtype instance Ord PlutusScript
derive newtype instance DecodeAeson PlutusScript
derive newtype instance EncodeAeson PlutusScript

instance Show PlutusScript where
  show = genericShow

plutusV1Script :: ByteArray -> PlutusScript
plutusV1Script ba = PlutusScript (ba /\ PlutusV1)

plutusV2Script :: ByteArray -> PlutusScript
plutusV2Script ba = PlutusScript (ba /\ PlutusV2)

instance AsCbor PlutusScript where
  encodeCbor = toCsl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCsl

toCsl :: PlutusScript -> Csl.PlutusScript
toCsl (PlutusScript (bytes /\ lang)) = plutusScript_newWithVersion bytes $
  Language.toCsl lang

fromCsl :: Csl.PlutusScript -> PlutusScript
fromCsl ps = PlutusScript
  (plutusScript_bytes ps /\ Language.fromCsl (plutusScript_languageVersion ps))
