module Ctl.Examples.Gov.SubmitProposal
  ( contract
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder (TransactionBuilderStep(SubmitProposal))
import Cardano.Types
  ( Address(RewardAddress)
  , RewardAddress
  , VotingProposal(VotingProposal)
  )
import Cardano.Types (GovernanceAction(Info)) as GovAction
import Cardano.Types.Transaction (hash) as Transaction
import Contract.Config
  ( ContractParams
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction (awaitTxConfirmed, submitTxFromBuildPlan)
import Contract.Wallet (getRewardAddresses)
import Ctl.Examples.Gov.Internal.Common (dummyAnchor)
import Data.Array (head) as Array
import Data.Map (empty) as Map

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec = Just $ ConnectToGenericCip30 "eternl" { cip95: true }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.Gov.SubmitProposal"

  govActionDeposit <- _.govActionDeposit <<< unwrap <$> getProtocolParameters
  rewardAddr <- liftedM "Could not get reward address" $
    map (asRewardAddress <=< Array.head)
      getRewardAddresses

  tx <- submitTxFromBuildPlan Map.empty mempty
    [ SubmitProposal $ VotingProposal
        { govAction: GovAction.Info
        , anchor: dummyAnchor
        , deposit: unwrap govActionDeposit
        , returnAddr: rewardAddr
        }
    ]

  awaitTxConfirmed $ Transaction.hash tx
  logInfo' "Successfully submitted voting proposal."

asRewardAddress :: Address -> Maybe RewardAddress
asRewardAddress = case _ of
  RewardAddress rewardAddr -> Just rewardAddr
  _ -> Nothing
