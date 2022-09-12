module Deserialization.Language
  ( _convertLanguage
  , convertLanguage
  ) where

import Data.Variant (inj)
import Deserialization.Error
  ( FromCslRepError
  , _fromCslRepError
  )
import Error (E)
import FfiHelpers
  ( ErrorFfiHelper
  , errorHelper
  )
import Serialization.Types (Language) as Csl
import Type.Row (type (+))
import Types.Scripts (Language(PlutusV1, PlutusV2)) as T

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

