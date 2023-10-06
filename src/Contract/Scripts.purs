-- | A module for various script types, most of which are newtype wrappers
-- | over `PlutusScript`. Corresponding hashes are also included as newtype
-- | wrappers over `ScriptHash`.
module Contract.Scripts
  ( getScriptByHash
  , getScriptsByHashes
  , module ExportScripts
  , module Hash
  , module NativeScript
  , module TypesScripts
  , module X
  ) where

import Prelude

import Contract.Monad (Contract)
import Control.Parallel (parTraverse)
import Ctl.Internal.ApplyArgs (ApplyArgsError(ApplyArgsError), applyArgs) as X
import Ctl.Internal.Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  ) as NativeScript
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.NativeScripts (NativeScriptHash(NativeScriptHash)) as X
import Ctl.Internal.Scripts
  ( mintingPolicyHash
  , nativeScriptStakeValidatorHash
  , plutusScriptStakeValidatorHash
  , validatorHash
  ) as ExportScripts
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Serialization.Hash (ScriptHash) as Hash
import Ctl.Internal.Service.Error (ClientError)
import Ctl.Internal.Types.Scripts
  ( MintingPolicy(PlutusMintingPolicy, NativeMintingPolicy)
  , MintingPolicyHash(MintingPolicyHash)
  , NativeScriptStakeValidator(NativeScriptStakeValidator)
  , PlutusScript(PlutusScript)
  , PlutusScriptStakeValidator(PlutusScriptStakeValidator)
  , StakeValidatorHash(StakeValidatorHash)
  , Validator(Validator)
  , ValidatorHash(ValidatorHash)
  ) as TypesScripts
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
