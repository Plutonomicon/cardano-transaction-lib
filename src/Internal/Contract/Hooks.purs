module Ctl.Internal.Contract.Hooks
  ( Hooks
  , ClusterParameters
  , emptyHooks
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction (Transaction)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Exception (Error)

type Hooks =
  { beforeSign :: Maybe (Effect Unit)
  , beforeInit :: Maybe (Effect Unit)
  , onSuccess :: Maybe (Effect Unit)
  , onError :: Maybe (Error -> Effect Unit)
  , onSubmit :: Maybe (Transaction -> Effect Unit)
  , onClusterStartup :: Maybe (ClusterParameters -> Effect Unit)
  }

type ClusterParameters =
  { privateKeys :: Array PrivateKey
  , nodeSocketPath :: String
  , nodeConfigPath :: String
  , privateKeysDirectory :: String
  }

emptyHooks :: Hooks
emptyHooks =
  { beforeSign: Nothing
  , beforeInit: Nothing
  , onSuccess: Nothing
  , onError: Nothing
  , onSubmit: Nothing
  , onClusterStartup: Nothing
  }
