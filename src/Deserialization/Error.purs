-- | Error-centered types and functions used by Deserialization modules.
module Deserialization.Error
  ( Err
  , FromCslRepError
  , _fromCslRepError
  , addErrTrace
  , cslErr
  , fromCslRepError
  , toError
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either(Left))
import Data.Variant (Variant, default, inj, onMatch, match)
import Deserialization.FromBytes (FromBytesError, _fromBytesError)
import Error (E, NotImplementedError, _notImplementedError, noteE)
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
