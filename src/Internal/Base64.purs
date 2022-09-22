module Ctl.Internal.Base64
  ( Base64String
  , mkBase64String
  , unBase64String
  , toByteArray
  , fromByteArray
  , decodeBase64
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  )
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Data.Either (Either(Left))
import Data.Maybe (Maybe, maybe)
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype Base64String = Base64String String

derive newtype instance Eq Base64String
derive newtype instance Ord Base64String

instance Show Base64String where
  show str = "(fromByteArray " <> show (toByteArray str) <> ")"

instance DecodeAeson Base64String where
  decodeAeson json = do
    decodeAeson json >>= mkBase64String >>>
      maybe (Left $ TypeMismatch "Base64String") pure

derive newtype instance EncodeAeson Base64String

-- | Wraps a `String` into `Base64String` if it is valid.
mkBase64String :: String -> Maybe Base64String
mkBase64String str = decodeBase64 str $> Base64String str

unBase64String :: Base64String -> String
unBase64String (Base64String str) = str

foreign import toByteArray :: Base64String -> ByteArray

foreign import fromByteArray :: ByteArray -> Base64String

foreign import _decodeBase64 :: MaybeFfiHelper -> String -> Maybe ByteArray

decodeBase64 :: String -> Maybe ByteArray
decodeBase64 = _decodeBase64 maybeFfiHelper

instance Arbitrary Base64String where
  arbitrary = do
    fromByteArray <$> arbitrary
