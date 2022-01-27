module Types (
  AppM (AppM),
  Env (..),
  Cbor (..),
  Fee (..),
  FeeEstimateError (..),
  CardanoBrowserServerError (..),
) where

import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as Cbor
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (FromHttpApiData, ToHttpApiData)
import Utils (tshow)

newtype AppM a = AppM (ReaderT Env IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadThrow
    )

newtype Env = Env
  { protocolParams :: C.ProtocolParameters
  }
  deriving stock (Generic)

newtype Cbor = Cbor Text
  deriving stock (Show)
  deriving newtype (Eq, FromHttpApiData, ToHttpApiData)

newtype Fee = Fee C.Lovelace
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON)

instance ToJSON Fee where
  -- to avoid issues with integer parsing in PS, we should probably return
  -- a JSON string, and not a number
  toJSON (Fee (C.Lovelace int)) = Aeson.String $ tshow int

  toEncoding (Fee (C.Lovelace int)) = Aeson.integerText int

-- We'll probably extend this with more error types over time
newtype CardanoBrowserServerError = FeeEstimate FeeEstimateError
  deriving stock (Show)

instance Exception CardanoBrowserServerError

data FeeEstimateError
  = InvalidCbor Cbor.DecoderError
  | InvalidHex String
  deriving stock (Show)

instance Exception FeeEstimateError
