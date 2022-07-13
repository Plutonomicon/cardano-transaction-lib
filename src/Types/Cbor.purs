module Types.Cbor where

import Prelude

import Contract.Prelude (foldl)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State.Trans (StateT, evalStateT, get, put)
import Data.Array.Partial (head)
import Data.Either (Either)
import Data.Int (pow)
import Data.Int.Bits (zshr, shl, (.&.), (.|.))
import Data.Newtype (class Newtype)
import Partial.Unsafe (unsafePartial)
import Types.ByteArray (ByteArray, byteArrayToIntArray, subarray, byteLength)
import Types.CborBytes (CborBytes(CborBytes))

newtype Cbor = Cbor CborBytes

derive instance Newtype Cbor _
derive newtype instance Show Cbor
derive newtype instance Eq Cbor
derive newtype instance Ord Cbor
derive newtype instance Semigroup Cbor
derive newtype instance Monoid Cbor

data CborType = ByteString Int

data CborParseError
  = UnknownType Int
  | UnknownBytesType Int
  | ByteArrayTooShort Int

instance Show CborParseError where
  show = case _ of
    UnknownType ty -> "UnknownType " <> show ty
    UnknownBytesType minor -> "UnknownBytesType " <> show minor
    ByteArrayTooShort extra -> "ByteArrayTooShort " <> show extra

type Parser a = StateT ByteArray (Except CborParseError) a

takeN :: Int -> Parser (Array Int)
takeN n = byteArrayToIntArray <$> takeN' n

takeN' :: Int -> Parser ByteArray
takeN' n = do
  ba <- get
  unless (byteLength ba >= n) $ throwError $ ByteArrayTooShort
    (n - byteLength ba)
  put $ subarray n (byteLength ba) ba
  pure $ subarray 0 n ba

readType :: Parser Int
readType = unsafePartial head <$> takeN 1

decodeType :: Int -> Parser CborType
decodeType rawCborType = do
  let
    majorType = rawCborType `zshr` 5
    minorType = rawCborType .&. 31
  unless (majorType == 2) $ throwError $ UnknownType majorType
  length <- case minorType - 24 of
    v | v < 0 -> pure minorType
    v | v >= 0 && v < 4 -> do
      lengthBytes <- takeN (2 `pow` v)
      pure $ foldl (\acc b -> shl acc 8 .|. b) 0 lengthBytes
    _ -> throwError $ UnknownBytesType minorType
  pure $ ByteString length

-- | Extract a `ByteArray` if the `Cbor` was a byte string
toByteArray :: Cbor -> Either CborParseError ByteArray
toByteArray (Cbor (CborBytes ba)) = runExcept $ flip evalStateT ba do
  rawCborType <- readType
  decodeType rawCborType >>= case _ of
    ByteString length -> takeN' length
