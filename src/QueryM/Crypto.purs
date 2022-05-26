module QueryM.Crypto
  ( plutusHash
  , HashMethod(..)
  ) where

import Prelude

import Aeson
  ( Aeson
  , decodeAeson
  , encodeAeson
  , parseJsonStringToAeson
  , toStringifiedNumbersJson
  )
import Affjax as Affjax
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (asks)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff.Class (liftAff)
import QueryM
  ( QueryM
  )
import QueryM.ServerConfig (mkHttpUrl)
import Types.ByteArray (ByteArray, byteArrayToHex)

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
      -- FIXME: if this breaks something it might be because the numbers are put in quotes? 
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
