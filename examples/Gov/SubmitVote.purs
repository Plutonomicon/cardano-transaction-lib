module Ctl.Examples.Gov.SubmitVote
  ( contract
  , example
  , main
  ) where

import Contract.Prelude

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
import Cardano.Types.PublicKey (hash) as PublicKey
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Governance (queryRegisteredDrepInfo)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.ProtocolParameters (getProtocolParameters)
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
import Ctl.Examples.Gov.Internal.Common (asRewardAddress, dummyAnchor)
import Data.Array (head, singleton) as Array
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
  govActionId <- submitProposal
  logInfo' $ "Successfully submitted voting proposal. Action id: " <> show
    govActionId
  submitVote govActionId
  logInfo' "Successfully voted on the proposal."

submitProposal :: Contract GovernanceActionId
submitProposal = do
  pubStakeKey <- Array.head <$> ownUnregisteredPubStakeKeys
  let
    stakeCred =
      wrap <<< PubKeyHashCredential <<< PublicKey.hash <$>
        pubStakeKey
  govActionDeposit <- _.govActionDeposit <<< unwrap <$> getProtocolParameters
  rewardAddr <- liftedM "Could not get reward address" $
    map (asRewardAddress <=< Array.head)
      getRewardAddresses
  let
    registerStakeAddress =
      maybe
        mempty
        (\x -> Array.singleton $ IssueCertificate (StakeRegistration x) Nothing)
        stakeCred
  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        registerStakeAddress <>
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
  drepInfo <- queryRegisteredDrepInfo drepCred
  drepDeposit <- _.drepDeposit <<< unwrap <$> getProtocolParameters
  let
    registerDrep =
      maybe
        ( Array.singleton $ IssueCertificate
            (RegDrepCert drepCred drepDeposit Nothing)
            Nothing
        )
        (const mempty)
        drepInfo
  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        registerDrep <>
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
