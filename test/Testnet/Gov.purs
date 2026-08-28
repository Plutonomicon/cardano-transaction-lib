module Test.Ctl.Testnet.Gov
  ( suite
  ) where

import Prelude

import Cardano.Provider (getGovActionType)
import Cardano.Transaction.Builder
  ( TransactionBuilderStep
      ( IssueCertificate
      , SubmitProposal
      , SubmitVotingProcedure
      )
  )
import Cardano.Types
  ( Certificate(RegDrepCert, StakeRegistration)
  , Credential(PubKeyHashCredential)
  , GovernanceActionId
  , Vote(VoteYes)
  , Voter(Drep)
  , VotingProcedure(VotingProcedure)
  , VotingProposal(VotingProposal)
  )
import Cardano.Types (GovernanceAction(Info)) as GovAction
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.PublicKey (hash) as PublicKey
import Contract.Governance (queryProposalById, queryVotesOnProposal)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Test (ContractTest)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Testnet (withKeyWallet, withWallets)
import Contract.Transaction
  ( awaitTxConfirmed
  , defaultBalancer
  , emptyBalancerCtx
  , submitTxFromBlueprint
  )
import Contract.Wallet
  ( getRewardAddresses
  , ownDrepPubKeyHash
  , ownUnregisteredPubStakeKeys
  )
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Gov.DelegateVoteAbstain (contract) as Gov.DelegateVoteAbstain
import Ctl.Examples.Gov.Internal.Common (asRewardAddress, dummyAnchor)
import Ctl.Examples.Gov.ManageDrep (contract) as Gov.ManageDrep
import Ctl.Examples.Gov.ManageDrepScript (contract) as Gov.ManageDrepScript
import Ctl.Examples.Gov.SubmitVote (contract) as Gov.SubmitVote
import Ctl.Examples.Gov.SubmitVoteScript (contract) as Gov.SubmitVoteScript
import Ctl.Internal.Test.UtxoDistribution (TestWalletSpec)
import Data.Array (concat, find, head) as Array
import Data.Map (singleton) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Effect.Exception (error)
import Mote (group, test)
import Test.Ctl.Testnet.Common (privateDrepKey, privateStakeKey)
import Test.Spec.Assertions (shouldEqual)

walletSpec :: TestWalletSpec
walletSpec = wrap
  { utxos:
      [ BigNum.fromInt 1_000_000_000
      , BigNum.fromInt 50_000_000
      ]
  , stakeKey: Just privateStakeKey
  , drepKey: Just privateDrepKey
  }

-- FIXME: Gov.SubmitVote and Gov.SubmitVoteScript tests are not self-contained:
-- they submit proposals without first registering the stake key used as
-- returnAddr, so they rely on it being already registered on-chain.
suite :: TestPlanM ContractTest Unit
suite = do
  group "Governance" do
    group "Examples" do
      test "Gov.DelegateVoteAbstain" do
        withWallets walletSpec \alice ->
          withKeyWallet alice Gov.DelegateVoteAbstain.contract

      test "Gov.ManageDrep example" do
        withWallets walletSpec \alice ->
          withKeyWallet alice Gov.ManageDrep.contract

      test "Gov.ManageDrepScript example" do
        withWallets walletSpec \alice ->
          withKeyWallet alice Gov.ManageDrepScript.contract

      test "Gov.SubmitVote example" do
        withWallets walletSpec \alice ->
          withKeyWallet alice Gov.SubmitVote.contract

      test "Gov.SubmitVoteScript example" do
        withWallets walletSpec \alice ->
          withKeyWallet alice Gov.SubmitVoteScript.contract

    test "Vote on a proposal and then discover the submitted vote" do
      withWallets walletSpec \alice ->
        withKeyWallet alice do
          { proposalRef } <- submitProposal
            { registerStakeAddress: true
            }
          let vote = VoteYes
          voter <- voteOnProposal { registerDrep: true } proposalRef vote
          votes <- queryVotesOnProposal proposalRef
          discoveredVote <- liftMaybe (error "Could not find vote") $
            Array.find (eq voter <<< _.voter) votes
          discoveredVote.vote `shouldEqual` vote

    test "Submit Info proposal and then query it by reference" do
      withWallets walletSpec \alice ->
        withKeyWallet alice do
          { proposalRef, proposal: VotingProposal proposal } <- submitProposal
            { registerStakeAddress: true
            }
          discoveredProposal <- liftedM "Could not get proposal" $
            queryProposalById proposalRef
          discoveredProposal.deposit `shouldEqual` wrap proposal.deposit
          discoveredProposal.returnAddress `shouldEqual` proposal.returnAddr
          discoveredProposal.proposalType `shouldEqual` getGovActionType
            proposal.govAction

voteOnProposal
  :: { registerDrep :: Boolean
     }
  -> GovernanceActionId
  -> Vote
  -> Contract Voter
voteOnProposal { registerDrep } proposalRef vote = do
  drepPkh <- ownDrepPubKeyHash
  let drepCred = PubKeyHashCredential drepPkh
  drepDeposit <- _.drepDeposit <<< unwrap <$> getProtocolParameters
  let
    registerDrepStep =
      if registerDrep then
        [ IssueCertificate (RegDrepCert drepCred drepDeposit Nothing)
            Nothing
        ]
      else
        mempty
  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        Array.concat
          [ registerDrepStep
          , [ SubmitVotingProcedure (Drep drepCred)
                ( Map.singleton proposalRef $
                    VotingProcedure { vote, anchor: Nothing }
                )
                Nothing
            ]
          ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }
  awaitTxConfirmed txHash
  pure $ Drep drepCred

submitProposal
  :: { registerStakeAddress :: Boolean
     }
  -> Contract
       { proposalRef :: GovernanceActionId
       , proposal :: VotingProposal
       }
submitProposal { registerStakeAddress } = do
  stakeCred <-
    if registerStakeAddress then do
      pubStakeKey <- Array.head <$> ownUnregisteredPubStakeKeys
      pure $ wrap <<< PubKeyHashCredential <<< PublicKey.hash <$>
        pubStakeKey
    else
      pure Nothing
  govActionDeposit <- _.govActionDeposit <<< unwrap <$>
    getProtocolParameters
  rewardAddress <- liftedM "Could not get reward address" $
    map (asRewardAddress <=< Array.head)
      getRewardAddresses
  let
    govAction = GovAction.Info
    proposal =
      VotingProposal
        { govAction
        , anchor: dummyAnchor
        , deposit: unwrap govActionDeposit
        , returnAddr: rewardAddress
        }
    registerStakeAddressStep =
      maybe
        mempty
        (\x -> [ IssueCertificate (StakeRegistration x) Nothing ])
        stakeCred
  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        Array.concat
          [ registerStakeAddressStep
          , [ SubmitProposal proposal Nothing ]
          ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }
  logInfo' "Successfully submitted voting proposal"
  awaitTxConfirmed txHash
  pure
    { proposalRef:
        wrap
          { transactionId: txHash
          , index: zero
          }
    , proposal
    }
