module Ctl.Internal.Deserialization.Language
  ( _convertLanguage
  , convertLanguage
  ) where

import Ctl.Internal.Deserialization.Error
  ( FromCslRepError
  , _fromCslRepError
  )
import Ctl.Internal.Error (E)
import Ctl.Internal.FfiHelpers
  ( ErrorFfiHelper
  , errorHelper
  )
import Ctl.Internal.Serialization.Types (Language) as Csl
import Ctl.Internal.Types.Scripts (Language(PlutusV1, PlutusV2)) as T
import Data.Variant (inj)
import Type.Row (type (+))

convertLanguage
  :: forall (r :: Row Type)
   . Csl.Language
  -> E (FromCslRepError + r) T.Language
convertLanguage = _convertLanguage
  (errorHelper (inj _fromCslRepError))
  { plutusV1: T.PlutusV1
  , plutusV2: T.PlutusV2
  }

foreign import _convertLanguage
  :: forall (r :: Row Type)
   . ErrorFfiHelper r
  -> { plutusV1 :: T.Language, plutusV2 :: T.Language }
  -> Csl.Language
  -> E r T.Language

