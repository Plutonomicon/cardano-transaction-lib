module Ctl.Internal.Types.Epoch (Epoch(Epoch)) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Ctl.Internal.Helpers (showWithParens)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import JS.BigInt (BigInt)

-- | An epoch number or length with greater precision for Ogmios than
-- | `Cardano.Types.Epoch`. [ 0 .. 18446744073709552000 ]
newtype Epoch = Epoch BigInt

derive instance Generic Epoch _
derive instance Newtype Epoch _
derive newtype instance Eq Epoch
derive newtype instance Ord Epoch
derive newtype instance DecodeAeson Epoch
derive newtype instance EncodeAeson Epoch

instance Show Epoch where
  show (Epoch e) = showWithParens "Epoch" e

