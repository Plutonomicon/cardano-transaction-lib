module Ctl.Examples.Gov.SubmitVote
  ( contract
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( TransactionBuilderStep(SubmitProposal, SubmitVotingProcedure)
  )
import Cardano.Types
  ( Credential(PubKeyHashCredential)
  , GovernanceActionId
  , Vote(VoteYes)
  , Voter(Drep)
  , VotingProcedure(VotingProcedure)
  , VotingProposal(VotingProposal)
  )
import Cardano.Types (GovernanceAction(Info)) as GovAction
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction
  ( awaitTxConfirmed
  , defaultBalancer
  , emptyBalancerCtx
  , submitTxFromBlueprint
  )
import Contract.Wallet (getRewardAddresses, ownDrepPubKeyHash)
import Ctl.Examples.Gov.Internal.Common (asRewardAddress, dummyAnchor)
import Ctl.Examples.Gov.ManageDrep (ContractPath(RegDrep), contractStep) as ManageDrep
import Data.Array (head) as Array
import Data.Map (singleton) as Map

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: true }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.Gov.SubmitVote"
  void $ ManageDrep.contractStep ManageDrep.RegDrep
  govActionId <- submitProposal
  logInfo' $ "Successfully submitted voting proposal. Action id: " <> show
    govActionId
  submitVote govActionId
  logInfo' "Successfully voted on the proposal."

submitProposal :: Contract GovernanceActionId
submitProposal = do
  govActionDeposit <- _.govActionDeposit <<< unwrap <$> getProtocolParameters
  rewardAddr <- liftedM "Could not get reward address" $
    map (asRewardAddress <=< Array.head)
      getRewardAddresses

  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        [ SubmitProposal
            ( VotingProposal
                { govAction: GovAction.Info
                , anchor: dummyAnchor
                , deposit: unwrap govActionDeposit
                , returnAddr: rewardAddr
                }
            )
            Nothing
        ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }
  awaitTxConfirmed txHash
  pure $ wrap { transactionId: txHash, index: zero }

submitVote :: GovernanceActionId -> Contract Unit
submitVote govActionId = do
  drepCred <- PubKeyHashCredential <$> ownDrepPubKeyHash

  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        [ SubmitVotingProcedure (Drep drepCred)
            ( Map.singleton govActionId $
                VotingProcedure { vote: VoteYes, anchor: Nothing }
            )
            Nothing
        ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }
  awaitTxConfirmed txHash
