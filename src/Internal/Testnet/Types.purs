module Ctl.Internal.Testnet.Types
  ( TestnetPaths
  , NodeLocation
  , Node
  , GenesisUtxoKeyLocation
  , TestnetRuntime
  , TestnetClusterConfig
  , TestnetConfig
  , LogParams
  ) where

import Contract.Prelude

import Contract.Config as Config
import Ctl.Internal.Contract.Hooks (Hooks)
import Ctl.Internal.ServerConfig (ServerConfig)
import Data.Log.Message (Message)
import Data.Time.Duration (Seconds)
import Data.UInt (UInt)
import Node.Path (FilePath)

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
  , slotLength :: Seconds
  , epochSize :: Maybe UInt
  -- FIXME: , maxTxSize :: Maybe UInt
  -- FIXME: , raiseExUnitsToMax :: Boolean
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
