module Seabug.Types
  ( NftCollection(..)
  , NftData(..)
  , NftId(..)
  ) where

import Contract.Prelude
import Contract.Address (PaymentPubKeyHash)
import Contract.Aeson (caseAesonObject, getField, jsonToAeson) as Aeson
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Contract.Numeric.Natural (Natural)
import Contract.Scripts
  ( ValidatorHash
  , ed25519KeyHashToBytes
  , scriptHashToBytes
  )
import Contract.Time (Slot)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , getCurrencySymbol
  , getTokenName
  )
import Data.Argonaut
  ( class DecodeJson
  , JsonDecodeError(TypeMismatch)
  , caseJsonObject
  , getField
  ) as Json
import Data.BigInt (BigInt, toInt)

-- Field names have been simplified due to row polymorphism. Please let me know
-- if the field names must be exact.
newtype NftId = NftId
  { collectionNftTn :: TokenName
  , price :: Natural
  , owner :: PaymentPubKeyHash
  }

derive instance Generic NftId _
derive instance Newtype NftId _
derive newtype instance Eq NftId
derive newtype instance Ord NftId

instance Show NftId where
  show = genericShow

instance FromData NftId where
  fromData (Constr n [ cnt, pr, own ]) | n == zero =
    NftId <$>
      ( { collectionNftTn: _, price: _, owner: _ }
          <$> fromData cnt
          <*> fromData pr
          <*> fromData own
      )
  fromData _ = Nothing

instance ToData NftId where
  toData (NftId { collectionNftTn, price, owner }) =
    Constr zero [ toData collectionNftTn, toData price, toData owner ]

-- Field names have been simplified due to row polymorphism. Please let me know
-- if the field names must be exact.
newtype NftCollection = NftCollection
  { collectionNftCs :: CurrencySymbol
  , lockLockup :: BigInt
  , lockLockupEnd :: Slot
  , lockingScript :: ValidatorHash
  , author :: PaymentPubKeyHash
  , authorShare :: Natural
  , daoScript :: ValidatorHash
  , daoShare :: Natural
  }

derive instance Generic NftCollection _
derive instance Newtype NftCollection _
derive newtype instance Eq NftCollection
derive newtype instance Ord NftCollection

-- Note the renaming of fields from their Plutus equivalents, e.g.
-- "nftCollection'collectionNftCs" to "collectionNftCs".
instance Json.DecodeJson NftCollection where
  decodeJson j = Json.caseJsonObject
    (Left $ Json.TypeMismatch "Expected Json Object")
    ( \o -> do
        collectionNftCs <- Json.getField o "nftCollection'collectionNftCs"
        lockLockupEnd <- Json.getField o "nftCollection'lockLockupEnd"
        lockingScript <- Json.getField o "nftCollection'lockingScript"
        author <- Json.getField o "nftCollection'author"
        authorShare <- Json.getField o "nftCollection'authorShare"
        daoScript <- Json.getField o "nftCollection'daoScript"
        daoShare <- Json.getField o "nftCollection'daoShare"
        -- Is the more efficient way to do this? Leave this until the end incase
        -- we fail earlier.
        let aeson = Aeson.jsonToAeson j
        lockLockup <- Aeson.caseAesonObject
          (Left $ Json.TypeMismatch "Expected Aeson Object")
          (flip Aeson.getField "nftCollection'lockLockup")
          aeson
        pure $ NftCollection
          { collectionNftCs
          , lockLockup
          , lockLockupEnd
          , lockingScript
          , author
          , authorShare
          , daoScript
          , daoShare
          }
    )
    j

instance Show NftCollection where
  show = genericShow

newtype NftData = NftData
  { nftCollection :: NftCollection
  , nftId :: NftId
  }

derive instance Generic NftData _
derive instance Newtype NftData _
derive newtype instance Eq NftData
derive newtype instance Ord NftData

instance Show NftData where
  show = genericShow

class Hashable a where
  hash :: a -> ByteArray -- Actually a Plutus BuiltinByteString

-- TO DO:
instance Hashable ByteArray where
  hash = undefined -- blake2b_256 https://github.com/dcposch/blakejs/blob/master/blake2b.js#L327

-- instance Hashable Natural where
--   hash = hash <<< toBin <<< toInt
--   where
--   toBin :: Int -> ByteArray
--   toBin n = toBin' n mempty
--     where
--       toBin' n' rest
--         | n' < 256 = byteArrayFromIntArrayUnsafe [n'] <> rest
--         | otherwise = toBin' (n' `divide` 256) (consByteString (n' `modulo` 256) rest)

instance Hashable CurrencySymbol where
  hash = hash <<< getCurrencySymbol

instance Hashable TokenName where
  hash = hash <<< getTokenName

instance Hashable ValidatorHash where
  hash = hash <<< scriptHashToBytes <<< unwrap

instance Hashable PaymentPubKeyHash where
  hash = hash <<< ed25519KeyHashToBytes <<< unwrap <<< unwrap

instance (Hashable a, Hashable b) => Hashable (a /\ b) where
  hash (a /\ b) = hash (hash a <> hash b)