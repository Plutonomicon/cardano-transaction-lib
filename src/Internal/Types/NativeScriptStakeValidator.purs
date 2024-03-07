module Ctl.Internal.Types.NativeScriptStakeValidator
  ( NativeScriptStakeValidator(NativeScriptStakeValidator)
  ) where

import Prelude

import Cardano.Types.NativeScript (NativeScript)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- | `NativeScriptStakeValidator`s are used as validators for withdrawals and
-- | stake address certificates.
newtype NativeScriptStakeValidator = NativeScriptStakeValidator NativeScript

derive instance Newtype NativeScriptStakeValidator _
derive instance Generic NativeScriptStakeValidator _
derive instance Eq NativeScriptStakeValidator

instance Show NativeScriptStakeValidator where
  show = genericShow
