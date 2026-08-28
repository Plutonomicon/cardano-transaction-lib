module Contract.Governance
  ( queryProposalById
  , queryVotesOnProposal
  ) where

import Prelude

import Cardano.Provider (Proposal, VoteOnProposal) as Provider
import Cardano.Types (GovernanceActionId)
import Contract.Monad (Contract, liftedE)
import Ctl.Internal.Contract.Monad (getProvider)
import Data.Maybe (Maybe)
import Effect.Aff.Class (liftAff)

queryProposalById :: GovernanceActionId -> Contract (Maybe Provider.Proposal)
queryProposalById proposalRef = do
  provider <- getProvider
  liftedE $ liftAff $ provider.getProposalById proposalRef

queryVotesOnProposal
  :: GovernanceActionId
  -> Contract (Array Provider.VoteOnProposal)
queryVotesOnProposal proposalRef = do
  provider <- getProvider
  liftedE $ liftAff $ provider.getVotesOnProposal proposalRef
