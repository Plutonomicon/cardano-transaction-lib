module Ctl.Examples.Gov.DelegateVoteAbstain
  ( contract
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder (TransactionBuilderStep(IssueCertificate))
import Cardano.Types.Certificate (Certificate(VoteDelegCert, VoteRegDelegCert))
import Cardano.Types.Credential (Credential(PubKeyHashCredential))
import Cardano.Types.DRep (DRep(AlwaysAbstain))
import Cardano.Types.PublicKey (hash) as PublicKey
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
import Contract.Transaction
  ( awaitTxConfirmed
  , defaultBalancer
  , emptyBalancerCtx
  , submitTxFromBlueprint
  )
import Contract.Wallet (ownRegisteredPubStakeKeys, ownUnregisteredPubStakeKeys)
import Control.Monad.Error.Class (throwError)
import Data.Array (head) as Array
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

  unregStakeKeys <- ownUnregisteredPubStakeKeys
  logDebug' $ "Unregistered stake keys: " <> show unregStakeKeys

  regStakeKeys <- ownRegisteredPubStakeKeys
  logDebug' $ "Registered stake keys: " <> show regStakeKeys

  { stakeKey, registered } <-
    case Array.head unregStakeKeys of
      Just stakeKey ->
        pure { stakeKey, registered: false }
      Nothing ->
        case Array.head regStakeKeys of
          Just stakeKey ->
            pure { stakeKey, registered: true }
          Nothing ->
            throwError $ error "Could to get pub stake key"

  let stakeCred = wrap $ PubKeyHashCredential $ PublicKey.hash stakeKey

  stakeCredDeposit <- _.stakeAddressDeposit <<< unwrap <$>
    getProtocolParameters

  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        [ IssueCertificate
            ( case registered of
                false ->
                  VoteRegDelegCert stakeCred AlwaysAbstain stakeCredDeposit
                true ->
                  VoteDelegCert stakeCred AlwaysAbstain
            )
            Nothing
        ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }

  awaitTxConfirmed txHash
  logInfo' "Tx submitted successfully!"
