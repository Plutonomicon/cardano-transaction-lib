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
import Control.Monad.Error.Class (throwError)
import Data.Array (drop)
import Data.BigInt (BigInt)
import Data.Bitraversable (ltraverse)
import Data.Char (toCharCode, fromCharCode)
import Data.Either (Either(Left), note)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (wrap)
import Data.String.CodePoints (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (class Traversable, traverse)
import Data.Bifunctor (lmap)
import Data.Tuple.Nested (type (/\))
import FromData (class FromData)
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata)
import Serialization.Types (AssetName) as CSL
import ToData (class ToData)
import Types.ByteArray
  ( ByteArray
  , byteArrayFromAscii
  , byteArrayFromInt16ArrayUnsafe
  , byteArrayFromIntArray
  , byteArrayFromIntArrayUnsafe
  , byteArrayToUTF16le
  , byteLength
  , hexToByteArray
  , hexToByteArrayUnsafe
  )
import Types.CborBytes
  ( CborBytes
  , cborBytesFromByteArray
  , cborBytesToByteArray
  , cborBytesToHex
  )

newtype TokenName = TokenName CborBytes

derive newtype instance Eq TokenName
derive newtype instance FromData TokenName
derive newtype instance FromMetadata TokenName
derive newtype instance ToMetadata TokenName
derive newtype instance Ord TokenName
derive newtype instance ToData TokenName

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
        let
          t = map toCharCode $ toCharArray tkstr

          cleanedTkstr :: Either String ByteArray
          cleanedTkstr =
            note "could not convert from hex to bytestring" <<< pure <<< hexToByteArrayUnsafe
              <<< fromCharArray
              =<< note "could not convetr from charcode"
                ( traverse fromCharCode
                    case t of
                      [ 0, 48, 120 ] -> drop 3 t
                      [ 0, 0, 0 ] -> drop 2 t
                      _ -> t
                )
        -- FIXME: what if the tokenname is actually \0\0\0? haskell will break this assuming it 
        -- comes from purescript side
        -- also we will break assuming it comes from haskell
        -- this issue has to be fixed on the haskell side
        -- ~= \NUL\NUL\NUL
        -- see https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#line-170
        lmap TypeMismatch
          (note "failed to make tokenname" <<< mkTokenName =<< cleanedTkstr)
    )

--  note (TypeMismatch "Invalid TokenName") <<< mkTokenName
--  <=< note (TypeMismatch "Invalid ByteArray") <<< (hexToByteArray <<< )
--  <=< flip getField "unTokenName"
--  )

instance EncodeAeson TokenName where
  {- 
instance FromJSON TokenName where
    parseJSON =
        JSON.withObject "TokenName" $ \object -> do
        raw <- object .: "unTokenName"
        fromJSONText raw
        where
            fromJSONText t = case Text.take 3 t of
                -- in case we get something prefixed with \0 *and* it also starts with "0x" (it's hexencoded) THEN
                -- => encodes in utf8 and then converts from base16 to whatever
                -- then 
                "\NUL0x"       -> either Haskell.fail (Haskell.pure . tokenName) . JSON.tryDecode . Text.drop 3 $ t
                "\NUL\NUL\NUL" -> Haskell.pure . fromText . Text.drop 2 $ t
                _              -> Haskell.pure . fromText $ t
  -}
  encodeAeson' (TokenName ba) =
    let
      finalBs :: ByteArray
      finalBs = byteArrayFromIntArrayUnsafe [ 0, 48, 120 ] <>
        (cborBytesToByteArray ba)
    -- FIXME: what if the tokenname starts with \0 => put another two \0 in front of that
    in
      encodeAeson' "fuck!" -- { "unTokenName": finalBs }

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
