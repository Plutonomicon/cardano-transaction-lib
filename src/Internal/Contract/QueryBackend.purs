module Ctl.Internal.Contract.QueryBackend
  ( BlockfrostBackend
  , BlockfrostBackendParams
  , CtlBackend
  , CtlBackendParams
  , QueryBackend(BlockfrostBackend, CtlBackend)
  , QueryBackendParams(BlockfrostBackendParams, CtlBackendParams)
  , defaultConfirmTxDelay
  , getBlockfrostBackend
  , getCtlBackend
  , mkBlockfrostBackendParams
  , mkSelfHostedBlockfrostBackendParams
  , mkCtlBackendParams
  ) where

import Prelude

import Ctl.Internal.QueryM (OgmiosWebSocket)
import Ctl.Internal.ServerConfig (ServerConfig)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Time.Duration (Seconds(Seconds))

--------------------------------------------------------------------------------
-- QueryBackend
--------------------------------------------------------------------------------

data QueryBackend
  = CtlBackend CtlBackend (Maybe BlockfrostBackend)
  | BlockfrostBackend BlockfrostBackend (Maybe CtlBackend)

type CtlBackend =
  { ogmios ::
      { config :: ServerConfig
      , ws :: OgmiosWebSocket
      }
  , kupoConfig :: ServerConfig
  }

type BlockfrostBackend =
  { blockfrostConfig :: ServerConfig
  , blockfrostApiKey :: Maybe String
  , confirmTxDelay :: Maybe Seconds
  }

getCtlBackend :: QueryBackend -> Maybe CtlBackend
getCtlBackend (CtlBackend backend _) = Just backend
getCtlBackend (BlockfrostBackend _ backend) = backend

getBlockfrostBackend :: QueryBackend -> Maybe BlockfrostBackend
getBlockfrostBackend (CtlBackend _ backend) = backend
getBlockfrostBackend (BlockfrostBackend backend _) = Just backend

--------------------------------------------------------------------------------
-- QueryBackendParams
--------------------------------------------------------------------------------

data QueryBackendParams
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

mkCtlBackendParams :: CtlBackendParams -> QueryBackendParams
mkCtlBackendParams = flip CtlBackendParams Nothing

mkBlockfrostBackendParams :: BlockfrostBackendParams -> QueryBackendParams
mkBlockfrostBackendParams = flip BlockfrostBackendParams Nothing

mkSelfHostedBlockfrostBackendParams
  :: BlockfrostBackendParams -> CtlBackendParams -> QueryBackendParams
mkSelfHostedBlockfrostBackendParams bf ctl = BlockfrostBackendParams bf
  (Just ctl)
