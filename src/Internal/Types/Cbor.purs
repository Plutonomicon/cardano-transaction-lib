-- | A partial CBOR decoder, based on [RFC 8949](https://www.rfc-editor.org/rfc/rfc8949.html)
module CTL.Internal.Types.Cbor
  ( Cbor(Cbor)
  , RawCborType
  , CborType(ByteStringType)
  , CborParseError
      ( UnknownType
      , UnknownAdditionalInformation
      , ByteArrayTooShort
      )
  , Parser
  , runParser
  , cborType
  , takeN
  , takeN'
  , toByteArray
  , fromBytes
  ) where

import Prelude

import CTL.Contract.Prelude (foldl)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State.Trans (StateT, evalStateT, get, put)
import Data.Either (Either)
import Data.UInt (UInt, zshr, shl, (.&.), (.|.))
import Data.UInt as UInt
import Data.Newtype (class Newtype)
import CTL.Internal.Types.ByteArray (ByteArray, byteArrayToIntArray, subarray, byteLength)
import CTL.Internal.Types.CborBytes (CborBytes(CborBytes))

-- | A CBOR data item is encoded as a byte string
newtype Cbor = Cbor CborBytes

derive instance Newtype Cbor _
derive newtype instance Show Cbor
derive newtype instance Eq Cbor
derive newtype instance Ord Cbor
derive newtype instance Semigroup Cbor
derive newtype instance Monoid Cbor

-- | The initial byte of the head of the data item
type RawCborType = UInt

-- | A structured representation of the head of the data item
data CborType = ByteStringType UInt

data CborParseError
  = UnknownType UInt
  | UnknownAdditionalInformation UInt
  | ByteArrayTooShort ByteArray Int

instance Show CborParseError where
  show = case _ of
    UnknownType ty -> "(UnknownType " <> show ty <> ")"
    UnknownAdditionalInformation info -> "(UnknownAdditionalInformation "
      <> show
        info
      <> ")"
    ByteArrayTooShort bs extra -> "(ByteArrayTooShort " <> show bs
      <> " "
      <> show extra
      <> ")"

type Parser :: Type -> Type
type Parser a = StateT ByteArray (Except CborParseError) a

-- | Same as `takeN'`, except returns `Array UInt`
takeN :: UInt -> Parser (Array UInt)
takeN n = map UInt.fromInt <<< byteArrayToIntArray <$> takeN' n

-- | Consume and return n bytes from the input, throwing an exception if there
-- | is not enough
takeN' :: UInt -> Parser ByteArray
takeN' = UInt.toInt >>> \n -> do
  ba <- get
  when (n > byteLength ba) do
    throwError $ ByteArrayTooShort ba n
  put $ subarray n (byteLength ba) ba
  pure $ subarray 0 n ba

-- | Convert an array of bytes in network order into an unsigned integer
fromBytes :: Array UInt -> UInt
fromBytes = foldl (\acc b -> shl acc (UInt.fromInt 8) .|. b) zero

-- | Read the initial byte of the head of the data item
readType :: Parser RawCborType
readType = fromBytes <$> takeN one

-- | The initial byte of the head of the data item contains a major type and
-- | additional information
-- |
-- | https://www.rfc-editor.org/rfc/rfc8949.html#section-3-2
partitionType
  :: RawCborType -> { majorType :: UInt, additionalInformation :: UInt }
partitionType u =
  { -- The 3 most signficant bits
    majorType: u `zshr` UInt.fromInt 5
  -- The 5 least signficant bits
  , additionalInformation: u .&. UInt.fromInt 31
  }

-- | Decode the rest of the head of the data item, given the first byte, into a
-- | `CborType`
-- |
-- | https://www.rfc-editor.org/rfc/rfc8949.html#name-specification-of-the-cbor-e
decodeType :: RawCborType -> Parser CborType
decodeType rawCborType =
  -- https://www.rfc-editor.org/rfc/rfc8949.html#name-major-types
  case UInt.toInt majorType of
    2 -> ByteStringType <$> decodeByteStringLength
    _ -> throwError $ UnknownType majorType
  where
  { majorType
  , additionalInformation
  } = partitionType rawCborType

  -- Below 24, the additional information is directly used as the length.
  -- Otherwise, it represents varying sizes of unsigned integers to read from
  -- the head.
  decodeByteStringLength :: Parser UInt
  decodeByteStringLength =
    case UInt.toInt additionalInformation of
      v | v < 24 -> pure additionalInformation
      24 -> takeN (UInt.fromInt 1) <#> fromBytes
      25 -> takeN (UInt.fromInt 2) <#> fromBytes
      26 -> takeN (UInt.fromInt 4) <#> fromBytes
      27 -> takeN (UInt.fromInt 8) <#> fromBytes
      _ -> throwError $ UnknownAdditionalInformation additionalInformation

-- | Decodes the head of the data item into `CborType`
cborType :: Parser CborType
cborType = readType >>= decodeType

runParser :: forall (a :: Type). Parser a -> Cbor -> Either CborParseError a
runParser parser (Cbor (CborBytes ba)) = runExcept $ flip evalStateT ba $ parser

-- | Extract a `ByteArray` if the `Cbor` was a byte string
toByteArray :: Cbor -> Either CborParseError ByteArray
toByteArray = runParser $ cborType >>= case _ of
  ByteStringType length -> takeN' length
