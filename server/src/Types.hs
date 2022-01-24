module Types (
  AppM (AppM),
  Env (..),
  Cbor (..),
  FeeEstimate (..),
) where

import Cardano.Api.Shelley qualified as C
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
    )

newtype Env = Env
  { protocolParams :: C.ProtocolParameters
  }
  deriving stock (Generic)

newtype Cbor = Cbor Text
  deriving stock (Show)
  deriving newtype (Eq, FromHttpApiData)

newtype FeeEstimate = FeeEstimate Integer
  deriving stock (Show, Generic)
  deriving newtype (Eq)

instance ToJSON FeeEstimate where
  toJSON = undefined -- TODO
  -- to avoid issues with integer parsing in PS,
  -- this should probably return a JSON string,
  -- and not a number
