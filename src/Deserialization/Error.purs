module Deserialization.Error where

import Contract.Prelude (Either(..), (<<<))
import Data.Variant (inj)
import Error (E, ErrorType)
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

type FromCslRepError r = (fromCslRepError :: String | r)

_fromCslRepError =
  { name: "fromCslRepError", proxy: Proxy } :: ErrorType "fromCslRepError"

fromCslRepError
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> E (FromCslRepError + r) a
fromCslRepError = Left <<< inj _fromCslRepError.proxy
