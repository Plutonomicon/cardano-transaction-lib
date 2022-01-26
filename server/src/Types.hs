module Types (
  AppM (AppM),
  Env (..),
  Cbor (..),
  Fee (..),
  FeeError (..),
  CardanoBrowserServerError (..),
) where

import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as Cbor
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Aeson (ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (FromHttpApiData)

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
  deriving newtype (Eq, FromHttpApiData)

newtype Fee = Fee C.Lovelace
  deriving stock (Show, Generic)
  deriving newtype (Eq)

instance ToJSON Fee where
  toJSON = undefined -- TODO
  -- to avoid issues with integer parsing in PS,
  -- this should probably return a JSON string,
  -- and not a number

newtype CardanoBrowserServerError = FeeEstimate FeeError
  deriving stock (Show)

instance Exception CardanoBrowserServerError

data FeeError
  = InvalidCbor Cbor.DecoderError
  | InvalidHex String
  deriving stock (Show)

instance Exception FeeError
