module Types (
  AppM (AppM),
  Env (..),
  Cbor (..),
  Fee (..),
  FeeEstimateError (..),
  CardanoBrowserServerError (..),
  newEnvIO,
) where

import Cardano.Api.Shelley qualified as Shelley
import Cardano.Binary qualified as Cbor
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson.Types (withText)
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Paths_cardano_browser_tx_server (getDataFileName)
import Servant (FromHttpApiData, ToHttpApiData)
import Text.Read (readMaybe)
import Utils (tshow)

newtype AppM (a :: Type) = AppM (ReaderT Env IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadThrow
    )

newtype Env = Env
  { protocolParams :: Shelley.ProtocolParameters
  }
  deriving stock (Generic)

newEnvIO :: IO (Either String Env)
newEnvIO =
  getDataFileName "config/pparams.json"
    >>= Aeson.eitherDecodeFileStrict @Shelley.ProtocolParameters
    <&> second Env

newtype Cbor = Cbor Text
  deriving stock (Show)
  deriving newtype (Eq, FromHttpApiData, ToHttpApiData)

newtype Fee = Fee Integer
  deriving stock (Show, Generic)
  deriving newtype (Eq)

instance ToJSON Fee where
  -- to avoid issues with integer parsing in PS, we should probably return
  -- a JSON string, and not a number
  toJSON (Fee int) = Aeson.String $ tshow int

  toEncoding (Fee int) = Aeson.integerText int

instance FromJSON Fee where
  parseJSON =
    withText "Fee" $
      maybe (fail "Expected quoted integer") (pure . Fee)
        . readMaybe @Integer
        . Text.unpack

-- We'll probably extend this with more error types over time
newtype CardanoBrowserServerError = FeeEstimate FeeEstimateError
  deriving stock (Show)

instance Exception CardanoBrowserServerError

data FeeEstimateError
  = InvalidCbor Cbor.DecoderError
  | InvalidHex String
  deriving stock (Show)

instance Exception FeeEstimateError
