-- | A module for Plutus-style `Credential`s
module Contract.Credential (module Credential) where

import Ctl.Internal.Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  ) as Credential