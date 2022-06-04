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
import Data.Either (Either(Left, Right), note, either)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (wrap, unwrap)
import Data.String.CodePoints (drop, take)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (class Traversable, traverse)
import Data.TextDecoding (decodeUtf8)
import Data.Tuple.Nested (type (/\))
import FromData (class FromData)
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata)
import Serialization.Types (AssetName) as CSL
import ToData (class ToData)
import Types.ByteArray
  ( ByteArray
  , byteArrayFromIntArray
  , byteArrayToHex
  , byteLength
  )
import Types.CborBytes (CborBytes, cborBytesToByteArray)

newtype TokenName = TokenName CborBytes

derive newtype instance Eq TokenName
derive newtype instance FromData TokenName
derive newtype instance FromMetadata TokenName
derive newtype instance ToMetadata TokenName
derive newtype instance Ord TokenName
derive newtype instance ToData TokenName

asBase16 :: ByteArray -> String
asBase16 ba = "0x" <> byteArrayToHex ba

fromTokenName :: forall r. (ByteArray -> r) -> (String -> r) -> TokenName -> r
fromTokenName arrayHandler stringHandler (TokenName cba) = either
  (\_ -> arrayHandler $ cborBytesToByteArray cba)
  stringHandler
  (decodeUtf8 <<< unwrap <<< cborBytesToByteArray $ cba)

instance DecodeAeson TokenName where
  {-
      toJSON = JSON.object . Haskell.pure . (,) "unTokenName" . JSON.toJSON .
        fromTokenName
            (\bs -> Text.cons '\NUL' (asBase16 bs))

            (\t -> case Text.take 1 t of "\NUL" -> Text.concat ["\NUL\NUL", t]; _ -> t)
  -}
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    ( \aes -> do
        tkstr :: String <- getField aes "unTokenName"
        case take 3 tkstr of
          """\NUL0x""" -> case tkFromStr (drop 3 tkstr) of
            Nothing -> Left $ TypeMismatch ("Invalid TokenName E1: " <> tkstr)
            Just tk -> Right tk

          """\NUL\NUL\NUL""" ->
            note (TypeMismatch $ "Invalid TokenName E2: " <> tkstr)
              $ tkFromStr (drop 2 tkstr)

          _ -> note (TypeMismatch $ "Invalid TokenName E3: " <> tkstr)
            $ tkFromStr tkstr
    )
    where
    tkFromStr :: String -> Maybe TokenName
    tkFromStr str = (TokenName <<< wrap) <$>
      (byteArrayFromIntArray <<< map toCharCode <<< toCharArray $ str)

-- FIXME: what if the tokenname is actually \0\0\0? haskell will break this assuming it
-- comes from purescript side
-- also we will break assuming it comes from haskell
-- this issue has to be fixed on the haskell side
instance EncodeAeson TokenName where
  encodeAeson' tk =
    let
      tkstr = fromTokenName
        (\ba -> """\NUL""" <> asBase16 ba)
        ( \t -> case take 1 t of
            """\NUL""" -> """\NUL\NUL""" <> t
            _ -> t
        )
        tk
    in
      encodeAeson' { "unTokenName": tkstr }

instance Show TokenName where
  show (TokenName tn) = "(TokenName " <> show tn <> ")"

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
