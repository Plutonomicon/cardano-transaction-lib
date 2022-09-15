-- | A module for Plutus-style `Credential`s
module CTL.Contract.Credential (module Credential) where

import CTL.Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  ) as Credential