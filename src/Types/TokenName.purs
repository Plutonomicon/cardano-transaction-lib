module Types.TokenName
  ( TokenName
  , adaToken
  , getTokenName
  , mkTokenName
  , mkTokenNames
  , tokenNameFromAssetName
  , assetNameName
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , encodeAeson'
  , getField
  )
import Data.BigInt (BigInt)
import Data.Bitraversable (ltraverse)
import Data.Char (toCharCode)
import Data.Either (Either(Left))
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (wrap)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested (type (/\))
import FromData (class FromData)
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata)
import Serialization.Types (AssetName) as CSL
import ToData (class ToData)
import Types.ByteArray
  ( ByteArray
  , byteArrayFromInt16ArrayUnsafe
  , byteArrayToUTF16le
  , byteLength
  )
import Types.CborBytes (CborBytes, cborBytesToByteArray)
import Data.String.CodeUnits (toCharArray)

newtype TokenName = TokenName CborBytes

derive newtype instance Eq TokenName
derive newtype instance FromData TokenName
derive newtype instance FromMetadata TokenName
derive newtype instance ToMetadata TokenName
derive newtype instance Ord TokenName
derive newtype instance ToData TokenName

instance DecodeAeson TokenName where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    ( \aes -> do
        tkstr <- getField aes "unTokenName"
        case
          ( mkTokenName <<<
              (byteArrayFromInt16ArrayUnsafe <<< map toCharCode <<< toCharArray)
          ) tkstr
          of
          Nothing -> Left $ TypeMismatch "Invalid TokenName"
          Just tknm -> pure tknm
    )

--  note (TypeMismatch "Invalid TokenName") <<< mkTokenName
--  <=< note (TypeMismatch "Invalid ByteArray") <<< (hexToByteArray <<< )
--  <=< flip getField "unTokenName"
--  )

instance EncodeAeson TokenName where
  encodeAeson' (TokenName ba) = encodeAeson'
    { "unTokenName": byteArrayToUTF16le <<< cborBytesToByteArray $ ba }

instance Show TokenName where
  show (TokenName tn) = "(TokenName" <> show tn <> ")"

getTokenName :: TokenName -> CborBytes
getTokenName (TokenName tokenName) = tokenName

-- | The empty token name.
adaToken :: TokenName
adaToken = TokenName mempty

-- | Create a `TokenName` from a `ByteArray` since TokenName data constructor is
-- | not exported
mkTokenName :: ByteArray -> Maybe TokenName
mkTokenName byteArr =
  if byteLength byteArr <= 32 then pure $ TokenName (wrap byteArr) else Nothing

foreign import assetNameName :: CSL.AssetName -> ByteArray

tokenNameFromAssetName :: CSL.AssetName -> TokenName
tokenNameFromAssetName = TokenName <<< wrap <<< assetNameName

-- | Creates a Map of `TokenName` and Big Integers from a `Traversable` of 2-tuple
-- | `ByteArray` and Big Integers with the possibility of failure
mkTokenNames
  :: forall (t :: Type -> Type)
   . Traversable t
  => t (ByteArray /\ BigInt)
  -> Maybe (Map TokenName BigInt)
mkTokenNames = traverse (ltraverse mkTokenName) >>> map Map.fromFoldable
