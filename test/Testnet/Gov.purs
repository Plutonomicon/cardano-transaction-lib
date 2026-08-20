module Test.Ctl.Testnet.Gov
  ( suite
  ) where

import Prelude

import Cardano.Provider (getGovActionType)
import Cardano.Transaction.Builder
  ( TransactionBuilderStep(IssueCertificate, SubmitProposal)
  )
import Cardano.Types
  ( Certificate(StakeRegistration)
  , Credential(PubKeyHashCredential)
  , VotingProposal(VotingProposal)
  )
import Cardano.Types (GovernanceAction(Info)) as GovAction
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.PublicKey (hash) as PublicKey
import Contract.Governance (queryProposalById)
import Contract.Log (logInfo')
import Contract.Monad (liftedM)
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
import Contract.Wallet (getRewardAddresses, ownUnregisteredPubStakeKeys)
import Ctl.Examples.Gov.DelegateVoteAbstain (contract) as Gov.DelegateVoteAbstain
import Ctl.Examples.Gov.Internal.Common (asRewardAddress, dummyAnchor)
import Ctl.Examples.Gov.ManageDrep (ContractPath(RegDrep))
import Ctl.Examples.Gov.ManageDrep (contract) as Gov.ManageDrep
import Ctl.Examples.Gov.ManageDrep (contractStep) as ManageDrep
import Ctl.Examples.Gov.ManageDrepScript (contract) as Gov.ManageDrepScript
import Ctl.Examples.Gov.SubmitVote (contract) as Gov.SubmitVote
import Ctl.Examples.Gov.SubmitVoteScript (contract) as Gov.SubmitVoteScript
import Ctl.Internal.Test.UtxoDistribution (TestWalletSpec)
import Data.Array (head) as Array
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
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

    test "Submit Info proposal and then query it by reference" do
      withWallets walletSpec \alice ->
        withKeyWallet alice do
          drepPkh <- ManageDrep.contractStep RegDrep
          logInfo' $ "Successfully registered DRep. PKH: " <> show drepPkh

          pubStakeKey <- Array.head <$> ownUnregisteredPubStakeKeys
          let
            stakeCred =
              wrap <<< PubKeyHashCredential <<< PublicKey.hash <$>
                pubStakeKey
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
          { txHash } <- submitTxFromBlueprint
            { buildSteps:
                [ SubmitProposal proposal Nothing ] <>
                  maybe
                    mempty
                    (\x -> [ IssueCertificate (StakeRegistration x) Nothing ])
                    stakeCred
            , balancer: defaultBalancer
            , balancerCtx: emptyBalancerCtx
            }
          logInfo' "Successfully submitted voting proposal"
          awaitTxConfirmed txHash

          discoveredProposal <-
            liftedM "Could not get proposal" $ queryProposalById $ wrap
              { transactionId: txHash
              , index: zero
              }
          discoveredProposal.deposit `shouldEqual` govActionDeposit
          discoveredProposal.returnAddress `shouldEqual` rewardAddress
          discoveredProposal.proposalType `shouldEqual` getGovActionType
            govAction
