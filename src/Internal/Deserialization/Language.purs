module Ctl.Internal.Deserialization.Language
  ( _convertLanguage
  , convertLanguage
  ) where

import Ctl.Internal.Serialization.Types (Language) as Csl
import Ctl.Internal.Types.Scripts (Language(PlutusV1, PlutusV2, PlutusV3)) as T

convertLanguage :: Csl.Language -> T.Language
convertLanguage = _convertLanguage
  { plutusV1: T.PlutusV1
  , plutusV2: T.PlutusV2
  , plutusV3: T.PlutusV3
  }

foreign import _convertLanguage
  :: { plutusV1 :: T.Language, plutusV2 :: T.Language, plutusV3 :: T.Language }
  -> Csl.Language
  -> T.Language

