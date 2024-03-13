-- | A module for various script types, most of which are newtype wrappers
-- | over `PlutusScript`. Corresponding hashes are also included as newtype
-- | wrappers over `ScriptHash`.
module Contract.Scripts
  ( getScriptByHash
  , getScriptsByHashes
  ) where

import Prelude

import Cardano.Types (ScriptHash)
import Cardano.Types.ScriptRef (ScriptRef)
import Contract.Monad (Contract)
import Control.Parallel (parTraverse)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Service.Error (ClientError)
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.Class (liftAff)

-- | Retrieve a `ScriptRef` given the hash
getScriptByHash :: ScriptHash -> Contract (Either ClientError (Maybe ScriptRef))
getScriptByHash hash = do
  queryHandle <- getQueryHandle
  liftAff $ queryHandle.getScriptByHash hash

-- | Retrieve `ScriptRef`s given their hashes
getScriptsByHashes
  :: Array ScriptHash
  -> Contract (Map ScriptHash (Either ClientError (Maybe ScriptRef)))
getScriptsByHashes hashes = do
  queryHandle <- getQueryHandle
  liftAff $ Map.fromFoldable <$> flip parTraverse hashes
    \sh -> queryHandle.getScriptByHash sh <#> Tuple sh
