module Cardano.Types.AuxiliaryData where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.GeneralTransactionMetadata (GeneralTransactionMetadata(..))
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.PlutusScript (PlutusScript(..))
import Control.Apply (lift2)
import Ctl.Internal.Helpers (notImplemented)
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

toCsl :: AuxiliaryData -> Csl.AuxiliaryData
toCsl
  (AuxiliaryData { metadata, nativeScripts, plutusScripts }) =
  notImplemented

-- ad <- newAuxiliaryData
-- for_ metadata $
--   convertGeneralTransactionMetadata >=>
--     setAuxiliaryDataGeneralTransactionMetadata ad
-- for_ nativeScripts $
--   convertNativeScripts >>> setAuxiliaryDataNativeScripts ad
-- for_ plutusScripts \ps -> do
--   scripts <- newPlutusScripts
--   for_ ps (convertPlutusScript >>> addPlutusScript scripts)
--   setAuxiliaryDataPlutusScripts ad scripts
-- pure ad

fromCsl :: Csl.AuxiliaryData -> AuxiliaryData
fromCsl = notImplemented
