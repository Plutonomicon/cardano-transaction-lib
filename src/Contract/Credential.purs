-- | A module for Plutus-style `Credential`s
module Contract.Credential (module X) where

import Ctl.Internal.Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  ) as X
