module Ctl.Internal.Types.Validator
  ( Validator(Validator)
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson)
import Cardano.Types.PlutusScript (PlutusScript(PlutusScript))
import Ctl.Internal.Helpers (decodeTaggedNewtype)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype Validator = Validator PlutusScript

derive instance Generic Validator _
derive instance Newtype Validator _
derive newtype instance Eq Validator
derive newtype instance Ord Validator

instance DecodeAeson Validator where
  decodeAeson = decodeTaggedNewtype "getValidator" Validator

instance EncodeAeson Validator where
  encodeAeson (Validator script) =
    encodeAeson { "getValidator": script }

instance Show Validator where
  show = genericShow
