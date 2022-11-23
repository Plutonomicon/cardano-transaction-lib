module Ctl.Internal.Contract.QueryBackend
  ( BlockfrostBackend
  , CtlBackend
  , QueryBackend(BlockfrostBackend, CtlBackend)
  , QueryBackendLabel(BlockfrostBackendLabel, CtlBackendLabel)
  , QueryBackendParams(BlockfrostBackendParams, CtlBackendParams)
  , QueryBackends
  , class HasQueryBackendLabel
  , backendLabel
  , defaultBackend
  , lookupBackend
  , mkBackendParams
  , mkSingletonBackendParams
  ) where

import Prelude

import Ctl.Internal.QueryM (OgmiosWebSocket)
import Ctl.Internal.QueryM.ServerConfig (ServerConfig)
import Data.Array (filter, nub) as Array
import Data.Array ((:))
import Data.Foldable (foldl, length)
import Data.Map (Map)
import Data.Map (empty, insert, lookup, singleton) as Map
import Data.Maybe (Maybe(Just))
import Effect (Effect)
import Effect.Exception (throw)

--------------------------------------------------------------------------------
-- QueryBackends
--------------------------------------------------------------------------------

data QueryBackends (backend :: Type) =
  QueryBackends backend (Map QueryBackendLabel backend)

derive instance Functor QueryBackends

mkSingletonBackendParams
  :: QueryBackendParams -> QueryBackends QueryBackendParams
mkSingletonBackendParams = flip QueryBackends Map.empty

mkBackendParams
  :: QueryBackendParams
  -> Array QueryBackendParams
  -> Effect (QueryBackends QueryBackendParams)
mkBackendParams defaultBackend backends =
  case length backends + 1 /= numUniqueBackends of
    true ->
      throw "mkBackendParams: multiple configs for the same service"
    false ->
      pure $ QueryBackends defaultBackend $
        foldl (\mp b -> Map.insert (backendLabel b) b mp) Map.empty backends
  where
  numUniqueBackends :: Int
  numUniqueBackends =
    length $ Array.nub $ map backendLabel (defaultBackend : backends)

defaultBackend :: forall (backend :: Type). QueryBackends backend -> backend
defaultBackend (QueryBackends backend _) = backend

lookupBackend
  :: forall (backend :: Type)
   . HasQueryBackendLabel backend
  => QueryBackendLabel
  -> QueryBackends backend
  -> Maybe backend
lookupBackend key (QueryBackends defaultBackend backends)
  | key == backendLabel defaultBackend = Just defaultBackend
  | otherwise = Map.lookup key backends

--------------------------------------------------------------------------------
-- QueryBackendLabel
--------------------------------------------------------------------------------

data QueryBackendLabel = CtlBackendLabel | BlockfrostBackendLabel

derive instance Eq QueryBackendLabel
derive instance Ord QueryBackendLabel

class HasQueryBackendLabel (t :: Type) where
  backendLabel :: t -> QueryBackendLabel

instance HasQueryBackendLabel QueryBackend where
  backendLabel (CtlBackend _) = CtlBackendLabel
  backendLabel (BlockfrostBackend _) = BlockfrostBackendLabel

instance HasQueryBackendLabel QueryBackendParams where
  backendLabel (CtlBackendParams _) = CtlBackendLabel
  backendLabel (BlockfrostBackendParams _) = BlockfrostBackendLabel

--------------------------------------------------------------------------------
-- QueryBackend
--------------------------------------------------------------------------------

type CtlBackend =
  { ogmiosConfig :: ServerConfig
  , ogmiosWs :: OgmiosWebSocket
  , kupoConfig :: ServerConfig
  }

type BlockfrostBackend =
  { blockfrostConfig :: ServerConfig
  }

data QueryBackend
  = CtlBackend CtlBackend
  | BlockfrostBackend BlockfrostBackend

--------------------------------------------------------------------------------
-- QueryBackendParams
--------------------------------------------------------------------------------

data QueryBackendParams
  = CtlBackendParams
      { ogmiosConfig :: ServerConfig
      , kupoConfig :: ServerConfig
      }
  | BlockfrostBackendParams
      { blockfrostConfig :: ServerConfig
      }

