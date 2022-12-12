module Ctl.Internal.Contract.QueryBackend
  ( BlockfrostBackend
  , BlockfrostBackendParams
  , CtlBackend
  , CtlBackendParams
  , QueryBackend(BlockfrostBackend, CtlBackend)
  , QueryBackendParams(BlockfrostBackendParams, CtlBackendParams)
  , getBlockfrostBackend
  , getCtlBackend
  , mkBlockfrostBackendParams
  , mkCtlBackendParams
  ) where

import Prelude

import Ctl.Internal.QueryM (OgmiosWebSocket)
import Ctl.Internal.QueryM.ServerConfig (ServerConfig)
import Data.Maybe (Maybe(Just, Nothing))

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
  }

mkCtlBackendParams :: CtlBackendParams -> QueryBackendParams
mkCtlBackendParams = flip CtlBackendParams Nothing

mkBlockfrostBackendParams :: BlockfrostBackendParams -> QueryBackendParams
mkBlockfrostBackendParams = flip BlockfrostBackendParams Nothing
