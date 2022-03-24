module Seabug.Types
  ( MintAct(..)
  , NftCollection(..)
  , NftData(..)
  , NftId(..)
  , class Hashable
  , hash
  )
  where

import Contract.Prelude
import Contract.Monad (Contract)
import Contract.Address (PaymentPubKeyHash)
import Contract.Aeson (caseAesonObject, getField, jsonToAeson) as Aeson
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray
  ( ByteArray
  , blake2bHash
  , byteArrayFromIntArrayUnsafe
  )
import Contract.Numeric.Natural (Natural, toBigInt)
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
import Data.BigInt (BigInt, fromInt, toInt)

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

data MintAct
  = MintToken NftId
  | ChangePrice NftId Natural
  | ChangeOwner NftId PaymentPubKeyHash
  | BurnToken NftId
derive instance Generic MintAct _

instance Show MintAct where
  show = genericShow

instance ToData MintAct where
  toData (MintToken nft) = Constr zero [ toData nft ]
  toData (ChangePrice nft price) = Constr one [ toData nft, toData price ]
  toData (ChangeOwner nft pkh) = Constr (fromInt 2) [ toData nft, toData pkh ]
  toData (BurnToken nft) =  Constr (fromInt 3) [ toData nft ]

instance FromData MintAct where
  fromData (Constr n [ nft ])
    | n == zero = MintToken <$> fromData nft
    | n == (fromInt 3) = BurnToken <$> fromData nft
  fromData (Constr n [ nft, pd ])
    | n == one = ChangePrice <$> fromData nft <*> fromData pd
    | n == (fromInt 2) = ChangeOwner <$> fromData nft <*> fromData pd
  fromData _ = Nothing

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

-- This differs from Plutus because of `Natural` when we convert from a `BigInt`
-- to an `Int`. Otherwise, the rest should not fail.
class Hashable a where
  hash :: a -> Contract (Maybe ByteArray) -- Plutus BuiltinByteString

instance Hashable ByteArray where
  hash = blake2bHash

instance Hashable Natural where
  hash = maybe (pure Nothing) (hash <<< toBin) <<< toInt <<< toBigInt
    where
    toBin :: Int -> ByteArray
    toBin n = toBin' n mempty

    -- Should be safe to use `byteArrayFromIntArrayUnsafe` since in both
    -- cases, n' < 256.
    toBin' :: Int -> ByteArray -> ByteArray
    toBin' n' rest
      | n' < 256 = byteArrayFromIntArrayUnsafe [ n' ] <> rest
      | otherwise =
          toBin'
            (n' `div` 256)
            (byteArrayFromIntArrayUnsafe [ n' `mod` 256 ] <> rest)

instance Hashable CurrencySymbol where
  hash = hash <<< getCurrencySymbol

instance Hashable TokenName where
  hash = hash <<< getTokenName

instance Hashable ValidatorHash where
  hash = hash <<< scriptHashToBytes <<< unwrap

instance Hashable PaymentPubKeyHash where
  hash = hash <<< ed25519KeyHashToBytes <<< unwrap <<< unwrap

instance (Hashable a, Hashable b) => Hashable (a /\ b) where
  hash (a /\ b) = ((<>) <$> hash a <*> hash b) >>= maybe (pure Nothing) hash

instance Hashable NftId where
  hash (NftId { collectionNftTn, price, owner }) =
    op3 <$> hash collectionNftTn <*> hash price <*> hash owner
      >>= maybe (pure Nothing) hash
    where
    op3 :: forall (a :: Type). Semigroup a => a -> a -> a -> a
    op3 a b c = a <> b <> c
