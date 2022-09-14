-- | This module includes a string type that is used in CIP-25 standard.
module CTL.Internal.Metadata.Cip25.Cip25String
  ( Cip25String
  , mkCip25String
  , unCip25String
  , fromMetadataString
  , toMetadataString
  , toCip25Strings
  , fromCip25Strings
  , fromDataString
  , toDataString
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  )
import CTL.Internal.FromData (class FromData, fromData)
import CTL.Internal.Metadata.FromMetadata (class FromMetadata, fromMetadata)
import CTL.Internal.Metadata.ToMetadata (class ToMetadata, toMetadata)
import CTL.Internal.ToData (class ToData, toData)
import CTL.Internal.Types.ByteArray (ByteArray, byteLength)
import CTL.Internal.Types.PlutusData (PlutusData)
import CTL.Internal.Types.TransactionMetadata (TransactionMetadatum)
import Control.Alt ((<|>))
import Data.Array ((:))
import Data.Array as Array
import Data.Either (hush, note)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(Nothing, Just), isJust)
import Data.Newtype (unwrap, wrap)
import Data.String.CodePoints as String
import Data.TextDecoder (decodeUtf8)
import Data.TextEncoder (encodeUtf8)
import Data.Tuple.Nested (type (/\), (/\))

-- | A string type that is used in CIP-25 standard.
-- | String length in bytes (in UTF-8) is limited by 64, because PlutusData
-- | bytes have this length limit.
newtype Cip25String = Cip25String String

derive newtype instance Eq Cip25String
derive newtype instance Ord Cip25String
derive newtype instance ToMetadata Cip25String
derive newtype instance FromMetadata Cip25String
derive newtype instance ToData Cip25String
derive newtype instance FromData Cip25String
derive newtype instance EncodeAeson Cip25String

instance Show Cip25String where
  show (Cip25String str) = "(unsafePartial (fromJust (mkCip25String "
    <> show str
    <> ")))"

instance DecodeAeson Cip25String where
  decodeAeson = decodeAeson >=> mkCip25String >>> note
    (TypeMismatch "Cip25String")

unCip25String :: Cip25String -> String
unCip25String (Cip25String str) = str

-- | A smart constructor for `Cip25String`
mkCip25String :: String -> Maybe Cip25String
mkCip25String str
  | byteLength (wrap (encodeUtf8 str)) <= 64 = Just $ Cip25String str
  | otherwise = Nothing

takeCip25String :: String -> Maybe { init :: Cip25String, rest :: Maybe String }
takeCip25String str =
  -- > https://www.rfc-editor.org/rfc/rfc3629
  --
  -- In UTF-8, characters from the U+0000..U+10FFFF range (the UTF-16
  --    accessible range) are encoded using sequences of 1 to 4 octets
  --
  -- Hence we start at 64/4 = 16 (worst case, all code points take 4 bytes),
  -- with a step equal to (64 - 16) / 2 = 24.
  case
    forwardSearch
      { minBound: 16
      , maxBound: 64
      , step: 24
      , takeN: \ix -> mkCip25String (String.take ix str)
      }
    of
    Nothing /\ _ -> Nothing
    Just cip25String /\ ix -> Just
      { init: cip25String
      , rest:
          let
            rest = String.drop ix str
          in
            if rest == "" then Nothing else Just rest
      }

forwardSearch
  :: forall (a :: Type)
   . { step :: Int, minBound :: Int, maxBound :: Int, takeN :: Int -> Maybe a }
  -> Maybe a /\ Int
forwardSearch { minBound, maxBound, takeN, step }
  | isJust (takeN $ minBound + step) =
      if minBound + step <= maxBound then forwardSearch
        { minBound: minBound + step, maxBound, takeN, step }
      else takeN maxBound /\ maxBound
  | otherwise =
      if step == 1 then
        takeN minBound /\ minBound
      else
        forwardSearch
          { minBound: minBound, maxBound, takeN, step: step `div` 2 }

toCip25Strings :: String -> Array Cip25String
toCip25Strings str = case takeCip25String str of
  Nothing -> []
  Just { init: cip25String, rest } ->
    case rest of
      Just restString -> cip25String : toCip25Strings restString
      Nothing -> [ cip25String ]

fromCip25Strings :: Array Cip25String -> String
fromCip25Strings = foldMap unCip25String

toDataString :: String -> PlutusData
toDataString str = case toCip25Strings str of
  [ singleStr ] -> toData singleStr
  strings -> toData $ toData <$> strings

fromDataString :: PlutusData -> Maybe String
fromDataString datum = do
  (fromCip25Strings <$> (Array.singleton <$> fromData datum)) <|> do
    bytes :: Array ByteArray <- fromData datum
    hush $ decodeUtf8 $ unwrap $ fold bytes

toMetadataString :: String -> TransactionMetadatum
toMetadataString str = case toCip25Strings str of
  [ singleStr ] -> toMetadata singleStr
  strings -> toMetadata $ toMetadata <$> strings

fromMetadataString :: TransactionMetadatum -> Maybe String
fromMetadataString datum = do
  fromCip25Strings <$> (Array.singleton <$> fromMetadata datum) <|> do
    bytes :: Array ByteArray <- fromMetadata datum
    hush $ decodeUtf8 $ unwrap $ fold bytes
