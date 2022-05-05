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

import Contract.Prelude (Either(Left), note)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArray)
import Data.Argonaut
  ( getField
  , class DecodeJson
  , class EncodeJson
  , JsonDecodeError(TypeMismatch)
  , caseJsonObject
  , encodeJson
  )
import Data.BigInt (BigInt)
import Data.Bitraversable (ltraverse)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested (type (/\))
import FromData (class FromData)
import Serialization.Types (AssetName) as CSL
import ToData (class ToData)
import Types.ByteArray (ByteArray, byteLength)

newtype TokenName = TokenName ByteArray

derive newtype instance Eq TokenName
derive newtype instance FromData TokenName
derive newtype instance Ord TokenName
derive newtype instance ToData TokenName

instance DecodeJson TokenName where
  decodeJson = caseJsonObject
    (Left $ TypeMismatch "Expected object")
    ( note (TypeMismatch "Invalid TokenName") <<< mkTokenName
        <=< note (TypeMismatch "Invalid ByteArray") <<< hexToByteArray
        <=< flip getField "unTokenName"
    )

instance EncodeJson TokenName where
  encodeJson (TokenName ba) = encodeJson
    { "unTokenName": byteArrayToHex ba }

instance Show TokenName where
  show (TokenName tn) = "(TokenName" <> show tn <> ")"

getTokenName :: TokenName -> ByteArray
getTokenName (TokenName tokenName) = tokenName

-- | The empty token name.
adaToken :: TokenName
adaToken = TokenName mempty

-- | Create a `TokenName` from a `ByteArray` since TokenName data constructor is
-- | not exported
mkTokenName :: ByteArray -> Maybe TokenName
mkTokenName byteArr =
  if byteLength byteArr <= 32 then pure $ TokenName byteArr else Nothing

foreign import assetNameName :: CSL.AssetName -> ByteArray

tokenNameFromAssetName :: CSL.AssetName -> TokenName
tokenNameFromAssetName = TokenName <<< assetNameName

-- | Creates a Map of `TokenName` and Big Integers from a `Traversable` of 2-tuple
-- | `ByteArray` and Big Integers with the possibility of failure
mkTokenNames
  :: forall (t :: Type -> Type)
   . Traversable t
  => t (ByteArray /\ BigInt)
  -> Maybe (Map TokenName BigInt)
mkTokenNames = traverse (ltraverse mkTokenName) >>> map Map.fromFoldable
