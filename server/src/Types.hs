module Types (
  AppM (AppM),
  Env (..),
  Cbor (..),
  Fee (..),
  ApplyArgsRequest (..),
  AppliedScript (..),
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
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.Aeson.Types (withText)
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Paths_cardano_browser_tx_server (getDataFileName)
import Plutus.V1.Ledger.Api qualified as Ledger
import Servant (FromHttpApiData, QueryParam', Required, ToHttpApiData)
import Servant.Docs qualified as Docs
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

  toEncoding (Fee int) = Aeson.Encoding.integerText int

instance FromJSON Fee where
  parseJSON =
    withText "Fee" $
      maybe (fail "Expected quoted integer") (pure . Fee)
        . readMaybe @Integer
        . Text.unpack

data ApplyArgsRequest = ApplyArgsRequest
  { script :: Ledger.Script
  -- FIXME
  -- Need to use a newtype around @Data@ with its own @FromJSON@ instance
  --
  -- If the client sends regular JSON, there will be precision loss in integral
  -- values; instead numbers must be stringified during encoding (the PS @Json@
  -- type is just a wrapper around the JS value)
  , args :: [Ledger.Data]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype AppliedScript = AppliedScript Ledger.Script
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

-- We'll probably extend this with more error types over time
newtype CardanoBrowserServerError = FeeEstimate FeeEstimateError
  deriving stock (Show)

instance Exception CardanoBrowserServerError

data FeeEstimateError
  = InvalidCbor Cbor.DecoderError
  | InvalidHex String
  deriving stock (Show)

instance Exception FeeEstimateError

-- API doc stuff
instance Docs.ToParam (QueryParam' '[Required] "tx" Cbor) where
  toParam _ =
    Docs.DocQueryParam
      "tx"
      [sampleTx]
      "A CBOR-encoded `Tx AlonzoEra`; should be sent as a hexadecimal string"
      Docs.Normal
    where
      sampleTx =
        mconcat
          [ "84a300818258205d677265fa5bb21ce6d8c7502aca70b93"
          , "16d10e958611f3c6b758f65ad9599960001818258390030"
          , "fb3b8539951e26f034910a5a37f22cb99d94d1d409f69dd"
          , "baea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba"
          , "3e96550504d5336100021a0002b569a0f5f6"
          ]

instance Docs.ToSample Fee where
  toSamples _ =
    [
      ( "The `Fee` will be returned encoded as a JSON string"
      , Fee 160265
      )
    ]

instance Docs.ToSample ApplyArgsRequest where
  toSamples _ = [] -- TODO

instance Docs.ToSample AppliedScript where
  toSamples _ = [] -- TODO
