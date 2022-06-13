module Types.TokenName
  ( TokenName
  , adaToken
  , getTokenName
  , mkTokenName
  , mkTokenNames
  , tokenNameFromAssetName
  , assetNameName
  , fromTokenName
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
import Contract.Prim.ByteArray (ByteArray(..))
import Data.BigInt (BigInt)
import Data.Bitraversable (ltraverse)
import Data.Either (Either(Right, Left), either)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap)
import Data.String.CodePoints (drop, take)
import Data.TextDecoding (decodeUtf8)
import Data.TextEncoding (encodeUtf8)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested (type (/\))
import FromData (class FromData)
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata)
import Serialization.Types (AssetName) as CSL
import ToData (class ToData)
import Types.ByteArray (ByteArray, byteArrayToHex, byteLength)

newtype TokenName = TokenName ByteArray

derive newtype instance Eq TokenName
derive newtype instance FromData TokenName
derive newtype instance FromMetadata TokenName
derive newtype instance ToMetadata TokenName
derive newtype instance Ord TokenName
derive newtype instance ToData TokenName

asBase16 :: ByteArray -> String
asBase16 ba = "0x" <> byteArrayToHex ba

fromTokenName :: forall r. (ByteArray -> r) -> (String -> r) -> TokenName -> r
fromTokenName arrayHandler stringHandler (TokenName ba) = either
  (const $ arrayHandler $ ba)
  stringHandler
  (decodeUtf8 (unwrap ba))

-- | Corresponds to following Haskell instance:
-- |
-- | ```
-- | toJSON = JSON.object . Haskell.pure . (,) "unTokenName" . JSON.toJSON .
-- |   fromTokenName
-- |       (\bs -> Text.cons '\NUL' (asBase16 bs))

-- |       (\t -> case Text.take 1 t of "\NUL" -> Text.concat ["\NUL\NUL", t]; _ -> t)
-- | ```
instance DecodeAeson TokenName where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \aes -> do
      tkstr <- getField aes "unTokenName"
      case take 3 tkstr of
        """\NUL0x""" -> Right $ tkFromStr (drop 3 tkstr)
        """\NUL\NUL\NUL""" -> Right $ tkFromStr (drop 2 tkstr)
        _ -> Right $ tkFromStr tkstr
    where
    tkFromStr :: String -> TokenName
    tkFromStr = TokenName <<< ByteArray <<< encodeUtf8

-- FIXME: what if the tokenname is actually \0\0\0? haskell will break this assuming it
-- comes from purescript side
-- also we will break assuming it comes from haskell
-- this issue has to be fixed on the haskell side
instance EncodeAeson TokenName where
  encodeAeson' = encodeAeson' <<< { "unTokenName": _ } <<< fromTokenName
    (("""\NUL""" <> _) <<< asBase16)
    ( \t -> case take 1 t of
        """\NUL""" -> """\NUL\NUL""" <> t
        _ -> t
    )

instance Show TokenName where
  show (TokenName tn) = "(TokenName " <> show tn <> ")"

getTokenName :: TokenName -> ByteArray
getTokenName (TokenName tokenName) = tokenName

-- | The empty token name.
adaToken :: TokenName
adaToken = TokenName mempty

-- | Create a `TokenName` from a `ByteArray` since TokenName data constructor is
-- | not exported
mkTokenName :: ByteArray -> Maybe TokenName
mkTokenName byteArr
  | byteLength byteArr <= 32 = pure $ TokenName $ byteArr
  | otherwise = Nothing

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
