-- | A module for various script types, most of which are newtype wrappers
-- | over `PlutusScript`. Corresponding hashes are also included as newtype
-- | wrappers over `ScriptHash`.
module Contract.Scripts
  ( applyArgs
  , applyArgsM
  , module ExportQueryM
  , module ExportScripts
  , module Hash
  , module NativeScript
  , module TypedValidator
  , module TypesScripts
  , module X
  ) where

import Prelude

import Contract.Monad (Contract, wrapContract)
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
import Ctl.Internal.QueryM
  ( ClientError
      ( ClientHttpError
      , ClientDecodeJsonError
      , ClientEncodingError
      )
  ) as ExportQueryM
import Ctl.Internal.QueryM (applyArgs) as QueryM
import Ctl.Internal.Scripts
  ( mintingPolicyHash
  , nativeScriptStakeValidatorHash
  , plutusScriptStakeValidatorHash
  , validatorHash
  ) as ExportScripts
import Ctl.Internal.Serialization.Hash (ScriptHash) as Hash
import Ctl.Internal.Types.PlutusData (PlutusData)
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
import Ctl.Internal.Types.Scripts (PlutusScript)
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
import Data.Either (Either, hush)
import Data.Maybe (Maybe)

-- | Apply `PlutusData` arguments to any type isomorphic to `PlutusScript`,
-- | returning an updated script with the provided arguments applied
applyArgs
  :: forall (r :: Row Type)
   . PlutusScript
  -> Array PlutusData
  -> Contract r (Either ExportQueryM.ClientError PlutusScript)
applyArgs a = wrapContract <<< QueryM.applyArgs a

-- | Same as `applyArgs` with arguments hushed.
applyArgsM
  :: forall (r :: Row Type)
   . PlutusScript
  -> Array PlutusData
  -> Contract r (Maybe PlutusScript)
applyArgsM a = map hush <<< applyArgs a
