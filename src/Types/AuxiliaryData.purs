module Cardano.Types.AuxiliaryData where

import Prelude

import Aeson (class EncodeAeson)
import Control.Apply (lift2)
import Ctl.Internal.Cardano.Types.NativeScript (NativeScript)
import Ctl.Internal.Types.Scripts (PlutusScript)
import Ctl.Internal.Types.TransactionMetadata (GeneralTransactionMetadata)
import Data.Array (union)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype AuxiliaryData = AuxiliaryData
  { metadata :: Maybe GeneralTransactionMetadata
  , nativeScripts :: Maybe (Array NativeScript)
  , plutusScripts :: Maybe (Array PlutusScript)
  }

derive instance Generic AuxiliaryData _
derive instance Newtype AuxiliaryData _
derive newtype instance Eq AuxiliaryData
derive newtype instance EncodeAeson AuxiliaryData

instance Show AuxiliaryData where
  show = genericShow

instance Semigroup AuxiliaryData where
  append (AuxiliaryData ad) (AuxiliaryData ad') =
    AuxiliaryData
      { metadata: ad.metadata <> ad'.metadata
      , nativeScripts: lift2 union ad.nativeScripts ad'.nativeScripts
      , plutusScripts: lift2 union ad.plutusScripts ad'.plutusScripts
      }

instance Monoid AuxiliaryData where
  mempty = AuxiliaryData
    { metadata: Nothing
    , nativeScripts: Nothing
    , plutusScripts: Nothing
    }
