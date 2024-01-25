module Ctl.Internal.Types.TokenName
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
  , encodeAeson
  , getField
  )
import Contract.Prim.ByteArray (hexToByteArray)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata)
import Ctl.Internal.Serialization.Types (AssetName) as CSL
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex, byteLength)
import Ctl.Internal.Types.RawBytes (RawBytes(RawBytes))
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Bitraversable (ltraverse)
import Data.Either (Either(Right, Left), either, note)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.String.CodePoints (drop, take)
import Data.TextEncoder (encodeUtf8)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested (type (/\))
import JS.BigInt (BigInt)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (resize)

newtype TokenName = TokenName RawBytes

derive newtype instance Eq TokenName
derive newtype instance FromData TokenName
derive newtype instance FromMetadata TokenName
derive newtype instance ToMetadata TokenName
derive newtype instance Ord TokenName
derive newtype instance ToData TokenName

instance Arbitrary TokenName where
  arbitrary = unsafePartial fromJust <<< mkTokenName <$> resize 32 arbitrary

foreign import _decodeUtf8
  :: forall (r :: Type). Uint8Array -> (String -> r) -> (String -> r) -> r

fromTokenName
  :: forall (r :: Type). (ByteArray -> r) -> (String -> r) -> TokenName -> r
fromTokenName arrayHandler stringHandler (TokenName (RawBytes ba)) = either
  (const $ arrayHandler $ ba)
  stringHandler
  (_decodeUtf8 (unwrap ba) Left Right)

-- | Corresponds to the Haskell instance at https://github.com/input-output-hk/plutus/blob/4fd86930f1dc628a816adf5f5d854b3fec578312/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs#L155:
instance DecodeAeson TokenName where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \aes -> do
      tkstr <- getField aes "unTokenName"
      case take 3 tkstr of
        "\x0000000x" -> do -- this is 3 characters '\NUL' '0' 'x'
          let stripped = drop 3 tkstr -- strip the \NUL followed by "0x"
          ba <-
            note
              (TypeMismatch $ "Expected base16 encoded string got " <> stripped)
              $ hexToByteArray stripped
          pure $ TokenName (wrap ba)
        "\x0\x0\x0" -> Right $ tkFromStr (drop 2 tkstr) -- if the original started with \NUL, we prepended 2 additional \NULs
        _ -> Right $ tkFromStr tkstr
    where
    tkFromStr :: String -> TokenName
    tkFromStr = TokenName <<< wrap <<< wrap <<< encodeUtf8

instance EncodeAeson TokenName where
  encodeAeson = encodeAeson <<< { "unTokenName": _ } <<< fromTokenName
    (\ba -> "\x0" <> "0x" <> byteArrayToHex ba)
    ( \s -> case take 1 s of
        "\x0" -> "\x0\x0" <> s
        _ -> s
    )

instance Show TokenName where
  show (TokenName tn) = "(TokenName " <> show tn <> ")"

getTokenName :: TokenName -> ByteArray
getTokenName (TokenName tokenName) = unwrap tokenName

-- | The empty token name.
adaToken :: TokenName
adaToken = TokenName mempty

-- | Create a `TokenName` from a `ByteArray` since TokenName data constructor is
-- | not exported
mkTokenName :: ByteArray -> Maybe TokenName
mkTokenName byteArr
  | byteLength byteArr <= 32 = pure $ TokenName $ wrap byteArr
  | otherwise = Nothing

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
