-- | Error-centered types and functions used by Deserialization modules.
module Ctl.Internal.Deserialization.Error
  ( Err
  , FromBytesError
  , FromCslRepError
  , _fromCslRepError
  , addErrTrace
  , cslErr
  , fromBytesErrorHelper
  , fromBytesError
  , fromCslRepError
  , toError
  ) where

import Prelude

import Ctl.Internal.Error (E, NotImplementedError, _notImplementedError, noteE)
import Ctl.Internal.FfiHelpers (ErrorFfiHelper, errorHelper)
import Data.Either (Either(Left))
import Data.Maybe (Maybe)
import Data.Variant (Variant, default, inj, match, onMatch)
import Effect.Exception (Error, error)
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

-- | Error type returned by Deserialization modules
type Err r a = E (FromBytesError + NotImplementedError + FromCslRepError + r) a

-- | Error type for errors returned when conversion from CSL representations fails.
type FromCslRepError r = (fromCslRepError :: String | r)

_fromCslRepError = Proxy :: Proxy "fromCslRepError"

-- | Creates an error E value from a string.
fromCslRepError
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> E (FromCslRepError + r) a
fromCslRepError = Left <<< inj _fromCslRepError

-- | Annotates Maybe's Nothing as a CSL error.
cslErr
  :: forall a r
   . String
  -> Maybe a
  -> E (FromCslRepError + r) a
cslErr = noteE <<< fromCslRepError

-- TODO make it so that it detects non-exhaustiveness
-- | Add trace string to an error.
addErrTrace :: forall r a. String -> Err r a -> Err r a
addErrTrace s (Left e) =
  Left $ e #
    ( default e # onMatch
        { fromCslRepError: \orig -> inj _fromCslRepError (s <> ": " <> orig)
        , notImplementedError: \orig -> inj _notImplementedError
            (s <> ": " <> orig)
        , fromBytesError: \orig -> inj _fromBytesError (s <> ": " <> orig)
        }
    )
addErrTrace _ a = a

-- | Convert a deserialization error into an `Effect` error
toError
  :: Variant (FromBytesError + NotImplementedError + FromCslRepError + ())
  -> Error
toError = error <<< match
  { fromCslRepError: \err -> "FromCslRepError: " <> err
  , fromBytesError: \err -> "FromBytesError: " <> err
  , notImplementedError: \err -> "NotImplementedError: " <> err
  }

-- | FromBytesError row alias
type FromBytesError r = (fromBytesError :: String | r)

-- | Needed to craate a variant type
_fromBytesError = Proxy :: Proxy "fromBytesError"

-- | An error to use
fromBytesError
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> E (FromBytesError + r) a
fromBytesError = Left <<< inj _fromBytesError

-- | An internal helper to shorten code
fromBytesErrorHelper
  :: forall (r :: Row Type)
   . ErrorFfiHelper (FromBytesError + r)
fromBytesErrorHelper = errorHelper (inj _fromBytesError)
