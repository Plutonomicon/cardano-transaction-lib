module Ctl.Examples.Gov.DelegateVoteAbstain
  ( contract
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder (TransactionBuilderStep(IssueCertificate))
import Cardano.Types.Certificate (Certificate(VoteRegDelegCert))
import Cardano.Types.Credential (Credential(PubKeyHashCredential))
import Cardano.Types.DRep (DRep(AlwaysAbstain))
import Cardano.Types.PublicKey (hash) as PublicKey
import Cardano.Types.Transaction (hash) as Transaction
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logDebug', logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction (awaitTxConfirmed, submitTxFromBuildPlan)
import Contract.Wallet (ownUnregisteredPubStakeKeys)
import Data.Array (head) as Array
import Data.Map (empty) as Map
import Effect.Exception (error)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: true }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.Gov.DelegateVoteAbstain"

  unregPubStakeKeys <- ownUnregisteredPubStakeKeys
  logDebug' $ "Unregistered public stake keys: " <> show unregPubStakeKeys

  pubStakeKey <- liftM (error "Failed to get unregistered pub stake key") $
    Array.head unregPubStakeKeys
  let stakeCred = wrap $ PubKeyHashCredential $ PublicKey.hash pubStakeKey

  stakeCredDeposit <- _.stakeAddressDeposit <<< unwrap <$>
    getProtocolParameters

  tx <- submitTxFromBuildPlan Map.empty mempty
    [ IssueCertificate
        (VoteRegDelegCert stakeCred AlwaysAbstain stakeCredDeposit)
        Nothing
    ]

  awaitTxConfirmed $ Transaction.hash tx
  logInfo' "Tx submitted successfully!"
