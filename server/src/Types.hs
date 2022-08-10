{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types (
  AppM (AppM),
  ServerOptions (..),
  Env (..),
  ApplyArgsRequest (..),
  AppliedScript (..),
  CtlServerError (..),
  newEnvIO,
  unsafeDecode,
) where

import Cardano.Api.Shelley qualified as Shelley
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, SomeException)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Port)
import Ogmios.Parser (decodeProtocolParameters)
import Ogmios.Query qualified
import Plutus.V1.Ledger.Api qualified as Ledger
import Plutus.V1.Ledger.Scripts qualified as Ledger.Scripts
import Servant.Docs qualified as Docs

newtype AppM (a :: Type) = AppM (ReaderT Env IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadThrow
    )

newtype Env = Env {protocolParams :: Shelley.ProtocolParameters}
  deriving stock (Generic)

data ServerOptions = ServerOptions
  { port :: Port
  , ogmiosHost :: String
  , ogmiosPort :: Int
  }
  deriving stock (Generic)

newEnvIO :: ServerOptions -> IO (Either String Env)
newEnvIO ServerOptions {ogmiosHost, ogmiosPort} =
  let ogmiosServerParams =
        Ogmios.Query.defaultServerParameters
          { Ogmios.Query.port = ogmiosPort
          , Ogmios.Query.host = ogmiosHost
          }
      queryProtocolParams = Ogmios.Query.makeRequest ogmiosServerParams
      maxConnectionAttempts = 300
   in Ogmios.Query.tryQueryUntilZero queryProtocolParams maxConnectionAttempts
        >>= \case
          Right response ->
            pure $
              Env <$> decodeProtocolParameters response
          Left msg ->
            pure . Left $
              "Can't get protocol parameters from Ogmios: \n" <> msg

data ApplyArgsRequest = ApplyArgsRequest
  { script :: Ledger.Script
  , args :: [Ledger.Data]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype AppliedScript = AppliedScript Ledger.Script
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

data CtlServerError
  = ErrorCall SomeException
  deriving stock (Show)

instance Exception CtlServerError

instance Docs.ToSample ApplyArgsRequest where
  toSamples _ =
    [
      ( "Both the `script` and each of its `args` should be hex-encoded CBOR"
      , exampleRequest
      )
    ]
    where
      exampleRequest :: ApplyArgsRequest
      exampleRequest =
        ApplyArgsRequest
          { script = exampleScript
          , args = [unsafeDecode "Data" "\"01\""]
          }

instance Docs.ToSample AppliedScript where
  toSamples _ =
    [
      ( "The applied script will be returned as hex-encoded CBOR"
      , AppliedScript exampleScript
      )
    ]

-- For decoding test fixtures, samples, etc...
unsafeDecode :: forall (a :: Type). FromJSON a => String -> LC8.ByteString -> a
unsafeDecode name = fromMaybe (error errorMsg) . Aeson.decode
  where
    errorMsg :: String
    errorMsg = "Failed to decode `" <> name <> "`"

-- TODO
-- Replace this with a simpler script
exampleScript :: Ledger.Script
exampleScript = unsafeDecode "Script" "\"4d01000033222220051200120011\""
