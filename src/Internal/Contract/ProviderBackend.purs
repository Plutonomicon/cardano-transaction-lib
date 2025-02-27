module Ctl.Internal.Contract.ProviderBackend
  ( BlockfrostBackendParams
  , CtlBackend
  , CtlBackendParams
  , ProviderBackend(BlockfrostBackend, CtlBackend)
  , ProviderBackendParams(BlockfrostBackendParams, CtlBackendParams)
  , defaultConfirmTxDelay
  , getBlockfrostBackend
  , getCtlBackend
  , mkBlockfrostBackendParams
  , mkSelfHostedBlockfrostBackendParams
  , mkCtlBackendParams
  ) where

import Prelude

import Cardano.Blockfrost.BlockfrostBackend (BlockfrostBackend)
import Ctl.Internal.ServerConfig (ServerConfig)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Time.Duration (Seconds(Seconds))

--------------------------------------------------------------------------------
-- ProviderBackend
--------------------------------------------------------------------------------

data ProviderBackend
  = CtlBackend CtlBackend (Maybe BlockfrostBackend)
  | BlockfrostBackend BlockfrostBackend (Maybe CtlBackend)

type CtlBackend =
  { ogmiosConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  }

getCtlBackend :: ProviderBackend -> Maybe CtlBackend
getCtlBackend (CtlBackend backend _) = Just backend
getCtlBackend (BlockfrostBackend _ backend) = backend

getBlockfrostBackend :: ProviderBackend -> Maybe BlockfrostBackend
getBlockfrostBackend (CtlBackend _ backend) = backend
getBlockfrostBackend (BlockfrostBackend backend _) = Just backend

--------------------------------------------------------------------------------
-- ProviderBackendParams
--------------------------------------------------------------------------------

data ProviderBackendParams
  = CtlBackendParams CtlBackendParams (Maybe BlockfrostBackendParams)
  | BlockfrostBackendParams BlockfrostBackendParams (Maybe CtlBackendParams)

type CtlBackendParams =
  { ogmiosConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  }

type BlockfrostBackendParams =
  { blockfrostConfig :: ServerConfig
  , blockfrostApiKey :: Maybe String
  , confirmTxDelay :: Maybe Seconds
  }

defaultConfirmTxDelay :: Maybe Seconds
defaultConfirmTxDelay = Just $ Seconds 30.0

mkCtlBackendParams :: CtlBackendParams -> ProviderBackendParams
mkCtlBackendParams = flip CtlBackendParams Nothing

mkBlockfrostBackendParams :: BlockfrostBackendParams -> ProviderBackendParams
mkBlockfrostBackendParams = flip BlockfrostBackendParams Nothing

mkSelfHostedBlockfrostBackendParams
  :: BlockfrostBackendParams -> CtlBackendParams -> ProviderBackendParams
mkSelfHostedBlockfrostBackendParams bf ctl = BlockfrostBackendParams bf
  (Just ctl)
