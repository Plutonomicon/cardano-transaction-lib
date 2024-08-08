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
import Cardano.Types.Transaction (hash) as Transaction
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
import Contract.Transaction (awaitTxConfirmed, submitTxFromBuildPlan)
import Contract.Wallet (getRewardAddresses, ownDrepPubKeyHash)
import Ctl.Examples.Gov.Internal.Common (asRewardAddress, dummyAnchor)
import Ctl.Examples.Gov.ManageDrep (ContractPath(RegDrep), contractStep) as ManageDrep
import Data.Array (head) as Array
import Data.Map (empty, singleton) as Map

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

{-
  { transactionId: unsafePartial fromJust $ decodeCbor $ wrap $
      hexToByteArrayUnsafe
        "fec3c9c4c8bf9b02237bbdccca9460eee1e5b67a5052fdbd5eb1d7ec1719d9f0"
  , index: zero
  }
-}

submitProposal :: Contract GovernanceActionId
submitProposal = do
  govActionDeposit <- _.govActionDeposit <<< unwrap <$> getProtocolParameters
  rewardAddr <- liftedM "Could not get reward address" $
    map (asRewardAddress <=< Array.head)
      getRewardAddresses

  tx <- submitTxFromBuildPlan Map.empty mempty
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
  let txHash = Transaction.hash tx
  awaitTxConfirmed txHash
  pure $ wrap { transactionId: txHash, index: zero }

submitVote :: GovernanceActionId -> Contract Unit
submitVote govActionId = do
  drepCred <- PubKeyHashCredential <$> ownDrepPubKeyHash

  tx <- submitTxFromBuildPlan Map.empty mempty
    [ SubmitVotingProcedure (Drep drepCred)
        ( Map.singleton govActionId $
            VotingProcedure { vote: VoteYes, anchor: Nothing }
        )
        Nothing
    ]
  awaitTxConfirmed $ Transaction.hash tx
