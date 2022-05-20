-- | A module for Plutus-style `Credential`s
module Contract.Credential (module Credential) where

import Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  ) as Credential