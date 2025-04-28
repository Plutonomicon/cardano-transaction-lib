module Ctl.Internal.Testnet.Types
  ( CardanoTestnetStartupParams
  , Era(Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway)
  , LoggingFormat(LogAsJson, LogAsText)
  , TestnetPaths
  , NodeLocation
  , Node
  , GenesisUtxoKeyLocation
  , OptionalStartupParams
  , TestnetRuntime
  , TestnetClusterConfig
  , TestnetConfig
  , LogParams
  , defaultOptionalStartupParams
  , defaultStartupParams
  ) where

import Contract.Prelude

import Contract.Config as Config
import Ctl.Internal.Contract.Hooks (Hooks)
import Ctl.Internal.ServerConfig (ServerConfig)
import Data.Log.Message (Message)
import Data.Time.Duration (Milliseconds, Seconds)
import Data.UInt (UInt)
import Node.Path (FilePath)
import Record as Record

type TestnetConfig =
  { logLevel :: LogLevel
  -- Server configs are used to deploy the corresponding services:
  , ogmiosConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  , hooks :: Hooks
  , clusterConfig :: TestnetClusterConfig
  }

type TestnetClusterConfig =
  { testnetMagic :: Int
  , era :: Era
  , slotLength :: Seconds
  , epochSize :: Maybe UInt
  -- FIXME: , maxTxSize :: Maybe UInt
  -- FIXME: , raiseExUnitsToMax :: Boolean
  }

data Era
  = Byron
  | Shelley
  | Allegra
  | Mary
  | Alonzo
  | Babbage
  | Conway

instance Show Era where
  show = case _ of
    Byron -> "byron-era"
    Shelley -> "shelley-era"
    Allegra -> "allegra-era"
    Mary -> "mary-era"
    Alonzo -> "alonzo-era"
    Babbage -> "babbage-era"
    Conway -> "conway-era"

data LoggingFormat = LogAsJson | LogAsText

instance Show LoggingFormat where
  show = case _ of
    LogAsJson -> "json"
    LogAsText -> "text"

type OptionalStartupParams r =
  ( numPoolNodes :: Maybe Int
  , era :: Maybe Era
  , epochLength :: Maybe Milliseconds
  , slotLength :: Maybe Seconds
  , activeSlotsCoeff :: Maybe Number
  , enableP2p :: Maybe Boolean
  , nodeLoggingFormat :: Maybe LoggingFormat
  | r
  )

-- | Command line params for the cardano-testnet executable
type CardanoTestnetStartupParams r =
  ( testnetMagic :: Int
  | OptionalStartupParams r
  )

defaultStartupParams
  :: { testnetMagic :: Int } -> Record (CardanoTestnetStartupParams ())
defaultStartupParams necessaryParams =
  defaultOptionalStartupParams `Record.union` necessaryParams

defaultOptionalStartupParams :: Record (OptionalStartupParams ())
defaultOptionalStartupParams =
  { numPoolNodes: Nothing
  , era: Just Babbage
  , epochLength: Nothing
  , slotLength: Nothing
  , activeSlotsCoeff: Nothing
  , enableP2p: Nothing
  , nodeLoggingFormat: Nothing
  }

type TestnetRuntime =
  { nodes :: Array Node
  , paths :: TestnetPaths
  }

type TestnetPaths =
  { testnetDirectory :: FilePath
  , genesisKeys :: Array GenesisUtxoKeyLocation
  , nodeConfigPath :: FilePath
  , nodeSocketPath :: FilePath
  , nodeDirs :: Array NodeLocation
  }

type Node =
  { socket :: FilePath
  , port :: UInt
  , location :: NodeLocation
  }

type NodeLocation =
  { idx :: Int
  , workdir :: FilePath
  }

type GenesisUtxoKeyLocation =
  { path :: FilePath
  , idx :: Int
  }

type LogParams r =
  ( logLevel :: LogLevel
  , customLogger :: Maybe (LogLevel -> Config.Message -> Aff Unit)
  , suppressLogs :: Boolean
  | r
  )
