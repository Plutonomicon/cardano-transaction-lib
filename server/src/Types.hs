{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types (
  AppM (AppM),
  ServerOptions (..),
  Env (..),
  Cbor (..),
  ExecutionUnitsMap (..),
  RdmrPtrExUnits (..),
  Fee (..),
  WitnessCount (..),
  FeesRequest (..),
  ApplyArgsRequest (..),
  AppliedScript (..),
  EvalExUnitsRequest (..),
  FinalizeRequest (..),
  FinalizedTransaction (..),
  CardanoError (..),
  CborDecodeError (..),
  CtlServerError (..),
  newEnvIO,
  getNodeConnectInfo,
  unsafeDecode,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Binary qualified as Cbor
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.Aeson.Types (withText)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Port)
import Numeric.Natural (Natural)
import Ogmios.Parser (decodeProtocolParameters)
import Ogmios.Query qualified
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
  , ogmiosHost :: String
  , ogmiosPort :: Int
  }
  deriving stock (Generic)

newEnvIO :: ServerOptions -> IO (Either String Env)
newEnvIO serverOptions@ServerOptions {ogmiosHost, ogmiosPort} =
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
              Env serverOptions <$> decodeProtocolParameters response
          Left msg ->
            pure . Left $ "Can't get protocol parameters from Ogmios: \n" <> msg

getNodeConnectInfo :: AppM (C.LocalNodeConnectInfo C.CardanoMode)
getNodeConnectInfo =
  asks serverOptions <&> \opts ->
    C.LocalNodeConnectInfo
      { Shelley.localConsensusModeParams =
          -- FIXME: Calc Byron epoch length based on Genesis params.
          C.CardanoModeParams (C.EpochSlots 21600)
      , Shelley.localNodeNetworkId = networkId opts
      , Shelley.localNodeSocketPath = nodeSocket opts
      }

newtype Cbor = Cbor Text
  deriving stock (Show)
  deriving newtype (Eq, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

newtype ExecutionUnitsMap = ExecutionUnitsMap [RdmrPtrExUnits]
  deriving stock (Show)
  deriving newtype (FromJSON, ToJSON)

data RdmrPtrExUnits = RdmrPtrExUnits
  { rdmrPtrTag :: Word8
  , rdmrPtrIdx :: Word64
  , exUnitsMem :: Natural
  , exUnitsSteps :: Natural
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FeesRequest = FeesRequest
  { count :: WitnessCount
  , tx :: Cbor
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Fee = Fee Integer
  deriving stock (Show, Generic)
  deriving newtype (Eq)

newtype WitnessCount = WitnessCount Word
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

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

data EvalExUnitsRequest = EvalExUnitsRequest
  { tx :: Cbor
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

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

instance Docs.ToSample FeesRequest where
  toSamples _ =
    [
      ( "The input should contain the intended number of witnesses and the\
         \CBOR of the tx"
      , FeesRequest (WitnessCount 1) (Cbor "00")
      )
    ]

instance Docs.ToSample EvalExUnitsRequest where
  toSamples _ =
    [
      ( "The input should contain the CBOR of the tx"
      , EvalExUnitsRequest (Cbor "00")
      )
    ]

instance Docs.ToSample ExecutionUnitsMap where
  toSamples _ =
    [
      ( "The `(RdmrPtr -> ExUnits)` map will be returned as a list of \
        \`RdmrPtrExUnits` objects with the following structure"
      , ExecutionUnitsMap [RdmrPtrExUnits 0 0 0 0]
      )
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



instance Docs.ToSample FinalizeRequest where
  toSamples _ =
    [
      ( "The input should contain CBOR of tx, redeemers, individual Plutus\
        \datums, and Plutus script hashes"
      , FinalizeRequest (Cbor "00") [Cbor "00"] (Cbor "00")
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
