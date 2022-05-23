module QueryM.Crypto
  ( hashData
  , hashScript
  , datumHash
  , plutusHash
  , HashedData(..)
  , HashMethod(..)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonString
  , decodeAeson
  , encodeAeson
  , parseJsonStringToAeson
  , stringifyAeson
  , toStringifiedNumbersJson
  )
import Affjax as Affjax
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (asks)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(Left), hush, note, either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just), maybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import QueryM
  ( ClientError(ClientHttpError, ClientDecodeJsonError)
  , QueryM
  , mkServerEndpointUrl
  , scriptToAeson
  )
import QueryM.ServerConfig (mkHttpUrl)
import Serialization (toBytes) as Serialization
import Serialization.Hash (ScriptHash)
import Serialization.PlutusData (convertPlutusData) as Serialization
import Types.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Types.Datum (Datum, DataHash)
import Types.Scripts (PlutusScript)
import Types.Transaction as Transaction
import Untagged.Union (asOneOf)

data HashMethod
  = Blake2b_256
  | Sha2_256
  | Sha3_256

derive instance Generic HashMethod _

instance Show HashMethod where
  show = genericShow

plutusHash
  :: HashMethod -> ByteArray -> QueryM (Either String ByteArray)
plutusHash meth bytes = do
  url <- asks $ (_ <> "/" <> "plutus-hash") <<< mkHttpUrl <<< _.serverConfig
  let
    methJson :: String
    methJson = case meth of
      Blake2b_256 -> "blake2b_256"
      Sha2_256 -> "sha2_256"
      Sha3_256 -> "sha3_256"

    requestJson :: Aeson
    requestJson = encodeAeson
      { bytes: byteArrayToHex bytes
      , method: methJson
      }

    reqBody :: Affjax.RequestBody.RequestBody
    reqBody = Affjax.RequestBody.Json $ toStringifiedNumbersJson $ encodeAeson
      requestJson
  response <- liftAff
    (Affjax.post Affjax.ResponseFormat.string url (pure reqBody))
  pure $ do
    responseJson :: ({ method :: String, hash :: ByteArray }) <-
      lmap Affjax.printError response >>= _.body
        >>> (parseJsonStringToAeson >=> decodeAeson)
        >>> lmap
          show
    unless (responseJson.method == methJson) $
      throwError "responseJson wasn't hashed with the method requested"
    pure responseJson.hash

hashData :: Datum -> QueryM (Maybe HashedData)
hashData datum = do
  body <-
    liftEffect $ byteArrayToHex <<< Serialization.toBytes <<< asOneOf
      <$> maybe' (\_ -> throw $ "Failed to convert plutus data: " <> show datum)
        pure
        (Serialization.convertPlutusData $ unwrap datum)
  url <- mkServerEndpointUrl "hash-data"
  -- get response json
  jsonBody <-
    liftAff
      ( Affjax.post Affjax.ResponseFormat.string url
          (Just $ Affjax.RequestBody.String $ stringifyAeson $ encodeAeson body)
      ) <#> map \x -> x.body
  -- decode
  pure $ hush <<< (decodeAeson <=< parseJsonStringToAeson) =<< hush jsonBody

-- | Hashes an Plutus-style Datum
datumHash :: Datum -> QueryM (Maybe DataHash)
datumHash = map (map (Transaction.DataHash <<< unwrap)) <<< hashData

newtype HashedData = HashedData ByteArray

derive instance Newtype HashedData _
derive instance Generic HashedData _

instance Show HashedData where
  show = genericShow

instance DecodeAeson HashedData where
  decodeAeson =
    map HashedData <<<
      caseAesonString (Left err) (note err <<< hexToByteArray)
    where
    err :: JsonDecodeError
    err = TypeMismatch "Expected hex bytes (raw) of hashed data"

hashScript
  :: forall (a :: Type) (b :: Type)
   . Newtype a PlutusScript
  => Newtype b ScriptHash
  => a
  -> QueryM (Either ClientError b)
hashScript script = do
  url <- mkServerEndpointUrl "hash-script"
  let
    reqBody :: Maybe Affjax.RequestBody.RequestBody
    reqBody = Just
      $ Affjax.RequestBody.String
      $ stringifyAeson
      $ scriptToAeson
      $ unwrap script
  liftAff (Affjax.post Affjax.ResponseFormat.string url reqBody)
    <#> either
      (Left <<< ClientHttpError)
      ( bimap ClientDecodeJsonError wrap
          <<< (decodeAeson <=< parseJsonStringToAeson)
          <<< _.body
      )

