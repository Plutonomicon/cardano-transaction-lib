module Ctl.Internal.Contract.Hooks
  ( Hooks
  , emptyHooks
  ) where

import Prelude

import Cardano.Types.Transaction (Transaction)
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Exception (Error)

type Hooks =
  { beforeSign :: Maybe (Effect Unit)
  , beforeInit :: Maybe (Effect Unit)
  , onSuccess :: Maybe (Effect Unit)
  , onError :: Maybe (Error -> Effect Unit)
  , onSubmit :: Maybe (Transaction -> Effect Unit)
  }

emptyHooks :: Hooks
emptyHooks =
  { beforeSign: Nothing
  , beforeInit: Nothing
  , onSuccess: Nothing
  , onError: Nothing
  , onSubmit: Nothing
  }
