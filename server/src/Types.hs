{-# LANGUAGE DerivingVia #-}

module Types (
  AppM (AppM),
  ServerOptions (..),
  Env (..),
  Cbor (..),
  ExecutionUnitsMap (..),
  Fee (..),
  WitnessCount (..),
  ApplyArgsRequest (..),
  AppliedScript (..),
  BytesToHash (..),
  Blake2bHash (..),
  FinalizeRequest (..),
  FinalizedTransaction (..),
  HashDataRequest (..),
  HashedData (..),
  HashScriptRequest (..),
  HashedScript (..),
  CardanoError (..),
  CborDecodeError (..),
  CtlServerError (..),
  hashLedgerScript,
  newEnvIO,
  getNodeConnectInfo,
  unsafeDecode,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Binary qualified as Cbor
import Codec.Serialise (serialise)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.Aeson.Types (withText)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.ByteString.Short qualified as SBS
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Port)
import Paths_ctl_server (getDataFileName)
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

data Env = Env
  { serverOptions :: ServerOptions
  , protocolParams :: Shelley.ProtocolParameters
  }
  deriving stock (Generic)

data ServerOptions = ServerOptions
  { port :: Port
  , nodeSocket :: FilePath
  , networkId :: C.NetworkId
  }
  deriving stock (Generic)

newEnvIO :: ServerOptions -> IO (Either String Env)
newEnvIO serverOptions =
  getDataFileName "config/pparams.json"
    >>= Aeson.eitherDecodeFileStrict @Shelley.ProtocolParameters
    <&> second (Env serverOptions)

getNodeConnectInfo :: AppM (C.LocalNodeConnectInfo C.CardanoMode)
getNodeConnectInfo =
  asks serverOptions <&> \opts ->
    C.LocalNodeConnectInfo
      { localConsensusModeParams =
          -- FIXME: Calc Byron epoch length based on Genesis params.
          C.CardanoModeParams (C.EpochSlots 21600)
      , localNodeNetworkId = networkId opts
      , localNodeSocketPath = nodeSocket opts
      }

newtype Cbor = Cbor Text
  deriving stock (Show)
  deriving newtype (Eq, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

newtype ExecutionUnitsMap = ExecutionUnitsMap ([(Cbor, Cbor)])
  deriving stock (Show, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Fee = Fee Integer
  deriving stock (Show, Generic)
  deriving newtype (Eq)

newtype WitnessCount = WitnessCount Word
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromHttpApiData, ToHttpApiData)

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

newtype BytesToHash = BytesToHash ByteString
  deriving stock (Show, Generic)
  deriving newtype (Eq)
  deriving (FromJSON, ToJSON) via JsonHexString

newtype Blake2bHash = Blake2bHash ByteString
  deriving stock (Show, Generic)
  deriving newtype (Eq)
  deriving (FromJSON, ToJSON) via JsonHexString

data FinalizeRequest = FinalizeRequest
  { tx :: Cbor
  , datums :: [Cbor]
  , redeemers :: Cbor
  , exUnitsMap :: [(Cbor, Cbor)]
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

newtype HashDataRequest = HashDataRequest Cbor
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

newtype HashedData = HashedData ByteString
  deriving stock (Show, Generic)
  deriving newtype (Eq)
  deriving (FromJSON, ToJSON) via JsonHexString

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

data CtlServerError
  = CardanoError CardanoError
  | CborDecode CborDecodeError
  deriving stock (Show)

instance Exception CtlServerError

data CardanoError
  = AcquireFailure String
  | ScriptExecutionError C.ScriptExecutionError
  | TxValidityIntervalError String
  | EraMismatchError
  deriving stock (Show)

instance Exception CardanoError

data CborDecodeError
  = InvalidCbor Cbor.DecoderError
  | InvalidHex String
  | OtherDecodeError String
  deriving stock (Show)

instance Exception CborDecodeError

-- API doc stuff
instance Docs.ToParam (QueryParam' '[Required] "tx" Cbor) where
  toParam _ =
    Docs.DocQueryParam
      "tx"
      [sampleTx]
      "A CBOR-encoded `Tx AlonzoEra`; should be sent as a hexadecimal string"
      Docs.Normal
    where
      sampleTx :: String
      sampleTx =
        mconcat
          [ "84a300818258205d677265fa5bb21ce6d8c7502aca70b93"
          , "16d10e958611f3c6b758f65ad9599960001818258390030"
          , "fb3b8539951e26f034910a5a37f22cb99d94d1d409f69dd"
          , "baea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba"
          , "3e96550504d5336100021a0002b569a0f5f6"
          ]

instance Docs.ToParam (QueryParam' '[Required] "count" WitnessCount) where
  toParam _ =
    Docs.DocQueryParam
      "wit-count"
      ["1"]
      "A natural number representing the intended number of key witnesses\
      \for the transaction"
      Docs.Normal

instance Docs.ToSample ExecutionUnitsMap where
  toSamples _ = [] -- TODO:

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

instance Docs.ToSample HashDataRequest where
  toSamples _ =
    [
      ( "The input should contain CBOR of a single datum"
      , HashDataRequest (Cbor "00")
      )
    ]

instance Docs.ToSample HashedData where
  toSamples _ =
    [ ("The data will be returned as hex-encoded CBOR", HashedData exampleData)
    ]
    where
      -- Fix this with an actual hash
      exampleData :: ByteString
      exampleData =
        mconcat
          [ "84a300818258205d677265fa5bb21ce6d8c7502aca70b93"
          , "16d10e958611f3c6b758f65ad9599960001818258390030"
          , "fb3b8539951e26f034910a5a37f22cb99d94d1d409f69dd"
          , "baea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba"
          , "3e96550504d5336100021a0002b569a0f5f6"
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
      ( "The input should contain CBOR of tx, redeemers, individual Plutus\
        \datums, Plutus script hashes, and redeemer pointers with the \
        \corresponding execution units"
      , FinalizeRequest
          (Cbor "00")
          [Cbor "00"]
          (Cbor "00")
          [(Cbor "00", Cbor "00")]
      )
    ]

instance Docs.ToSample FinalizedTransaction where
  toSamples _ =
    [ ("The output is CBOR-encoded Tx", exampleTx)
    ]
    where
      exampleTx :: FinalizedTransaction
      exampleTx =
        FinalizedTransaction . Cbor $
          mconcat
            [ "84a300818258205d677265fa5bb21ce6d8c7502aca70b93"
            , "16d10e958611f3c6b758f65ad9599960001818258390030"
            , "fb3b8539951e26f034910a5a37f22cb99d94d1d409f69dd"
            , "baea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba"
            , "3e96550504d5336100021a0002b569a0f5f6"
            ]

instance Docs.ToSample BytesToHash where
  toSamples _ = [("Bytes to hash as hexadecimal string", BytesToHash "foo")]

instance Docs.ToSample Blake2bHash where
  toSamples _ =
    [
      ( "Hash bytes are returned as hexidecimal string"
      , Blake2bHash
          "\184\254\159\DELbU\166\250\b\246h\171c*\
          \\141\b\SUB\216y\131\199|\210t\228\140\228P\240\179I\253"
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

newtype JsonHexString = JsonHexString ByteString

instance FromJSON JsonHexString where
  parseJSON =
    withText "JsonHexString" $
      either (const $ fail "Couldn't decode hex string") (pure . JsonHexString)
        . Base16.decode
        . Text.Encoding.encodeUtf8

instance ToJSON JsonHexString where
  toJSON (JsonHexString jhs) =
    Aeson.String
      . Text.Encoding.decodeUtf8
      $ Base16.encode jhs
