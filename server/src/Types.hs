module Types (
  AppM (AppM),
  ServerOptions (..),
  Env (..),
  Cbor (..),
  Fee (..),
  FinalizeRequest(..),
  FinalizedTransaction(..),
  ApplyArgsRequest (..),
  AppliedScript (..),
  HashScriptRequest (..),
  HashedScript (..),
  FeeEstimateError (..),
  FinalizeTxError(..),
  CardanoBrowserServerError (..),
  hashLedgerScript,
  newEnvIO,
  unsafeDecode,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Binary qualified as Cbor
import Codec.Serialise (serialise)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.Aeson.Types (withText)
import Data.Bifunctor (second)
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.ByteString.Short qualified as SBS
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Port)
import Paths_cardano_browser_tx_server (getDataFileName)
import Plutus.V1.Ledger.Api qualified as Ledger
import Plutus.V1.Ledger.Scripts qualified as Ledger.Scripts
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

newtype ServerOptions = ServerOptions
  { port :: Port
  }
  deriving stock (Generic)

newEnvIO :: IO (Either String Env)
newEnvIO =
  getDataFileName "config/pparams.json"
    >>= Aeson.eitherDecodeFileStrict @Shelley.ProtocolParameters
    <&> second Env

newtype Cbor = Cbor Text
  deriving stock (Show)
  deriving newtype (Eq, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

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
  , args :: [Ledger.Data]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype AppliedScript = AppliedScript Ledger.Script
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

data FinalizeRequest = FinalizeRequest
  { tx :: Cbor
  , datums :: [Cbor]
  , redeemers :: Cbor
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- This is only to avoid an orphan instance for @ToDocs@
newtype FinalizedTransaction = FinalizedTransaction Cbor
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

-- This is only to avoid an orphan instance for @ToDocs@
newtype HashScriptRequest = HashScriptRequest Ledger.Script
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

-- This is a newtype to avoid the orphan instance
newtype HashedScript = HashedScript Ledger.Scripts.ScriptHash
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

-- Adapted from `plutus-apps` implementation
-- rev. d637b1916522e4ec20b719487a8a2e066937aceb
hashLedgerScript :: Ledger.Script -> Ledger.Scripts.ScriptHash
hashLedgerScript =
  Ledger.Scripts.ScriptHash
    . Ledger.toBuiltin
    . C.serialiseToRawBytes
    . C.hashScript
    . toCardanoApiScript

-- Adapted from `plutus-apps` implementation
-- rev. d637b1916522e4ec20b719487a8a2e066937aceb
toCardanoApiScript :: Ledger.Script -> C.Script C.PlutusScriptV1
toCardanoApiScript =
  C.PlutusScript C.PlutusScriptV1
    . Shelley.PlutusScriptSerialised
    . SBS.toShort
    . LC8.toStrict
    . serialise

data CardanoBrowserServerError
  = FeeEstimate FeeEstimateError
  | FinalizeTx FinalizeTxError
  deriving stock (Show)

instance Exception CardanoBrowserServerError

data FeeEstimateError
  = FEInvalidCbor Cbor.DecoderError
  | FEInvalidHex String
  deriving stock (Show)

instance Exception FeeEstimateError

data FinalizeTxError
  = FTInvalidCbor Cbor.DecoderError
  | FTInvalidHex String
  deriving stock (Show)

instance Exception FinalizeTxError

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

instance Docs.ToSample HashScriptRequest where
  toSamples _ =
    [ ("The script should be CBOR-encoded hex", HashScriptRequest exampleScript)
    ]

instance Docs.ToSample HashedScript where
  toSamples _ =
    [
      ( "The hashed script will be returned as a JSON object with a \
        \`getScriptHash` field containing the script hash"
      , unsafeDecode
          "HashedScript"
          "{\"getScriptHash\":\
          \\"67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656\"}"
      )
    ]

instance Docs.ToSample FinalizeRequest where
  toSamples _ =
    [
      ( "The input should contain CBOR of Tx, Redeemers and individual plutus datums"
      , FinalizeRequest (Cbor "00") [Cbor "00"] (Cbor "00")
      )
    ]

instance Docs.ToSample FinalizedTransaction where
  toSamples _ =
    [ ("The output is CBOR-encoded Tx", exampleTx)
    ]
    where
      exampleTx = FinalizedTransaction . Cbor $ mconcat
        [ "84a300818258205d677265fa5bb21ce6d8c7502aca70b93"
        , "16d10e958611f3c6b758f65ad9599960001818258390030"
        , "fb3b8539951e26f034910a5a37f22cb99d94d1d409f69dd"
        , "baea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba"
        , "3e96550504d5336100021a0002b569a0f5f6"
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
