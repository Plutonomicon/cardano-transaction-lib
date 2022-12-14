-- | A module for various script types, most of which are newtype wrappers
-- | over `PlutusScript`. Corresponding hashes are also included as newtype
-- | wrappers over `ScriptHash`.
module Contract.Scripts
  ( module ApplyArgs
  , module ExportScripts
  , module Hash
  , module NativeScript
  , module TypedValidator
  , module TypesScripts
  , module X
  ) where

import Ctl.Internal.ApplyArgs (ApplyArgsError(ApplyArgsError), applyArgs) as ApplyArgs
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
import Ctl.Internal.NativeScripts (NativeScriptHash(NativeScriptHash)) as X
import Ctl.Internal.Scripts
  ( mintingPolicyHash
  , nativeScriptStakeValidatorHash
  , plutusScriptStakeValidatorHash
  , validatorHash
  ) as ExportScripts
import Ctl.Internal.Serialization.Hash (ScriptHash) as Hash
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
import Ctl.Internal.Types.TypedValidator
  ( class DatumType
  , class RedeemerType
  , class ValidatorTypes
  , TypedValidator(TypedValidator)
  , ValidatorType
  , WrappedValidatorType
  , forwardingMintingPolicy
  , generalise
  , typedValidatorHash
  , typedValidatorScript
  ) as TypedValidator
