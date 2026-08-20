module Contract.Governance
  ( queryProposalById
  ) where

import Prelude

import Cardano.Provider (Proposal) as Provider
import Cardano.Types (GovernanceActionId)
import Contract.Monad (Contract, liftedE)
import Ctl.Internal.Contract.Monad (getProvider)
import Data.Maybe (Maybe)
import Effect.Aff.Class (liftAff)

queryProposalById :: GovernanceActionId -> Contract (Maybe Provider.Proposal)
queryProposalById proposalRef = do
  provider <- getProvider
  liftedE $ liftAff $ provider.getProposalById proposalRef
