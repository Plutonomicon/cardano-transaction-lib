module Ctl.Examples.Gov.ManageDrep
  ( ContractPath(RegDrep, UpdateDrep, UnregDrep)
  , contract
  , contractStep
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder (TransactionBuilderStep(IssueCertificate))
import Cardano.Types
  ( Anchor
  , Certificate(RegDrepCert, UpdateDrepCert, UnregDrepCert)
  , Credential(PubKeyHashCredential)
  , Ed25519KeyHash
  )
import Cardano.Types.Transaction (hash) as Transaction
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction (awaitTxConfirmed, submitTxFromBuildPlan)
import Contract.Wallet (ownDrepPubKeyHash)
import Control.Monad.Error.Class (catchError, throwError)
import Ctl.Examples.Gov.Internal.Common (dummyAnchor)
import Data.Map (empty) as Map
import Data.String (Pattern(Pattern))
import Data.String (contains) as String
import Effect.Exception (message)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: true }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.Gov.ManageDrep"
  drepPkh <- contractStep RegDrep
  logInfo' $ "Successfully registered DRep. DRepID: " <> show drepPkh
  void $ contractStep $ UpdateDrep dummyAnchor
  logInfo' "Successfully updated DRep metadata."
  void $ contractStep UnregDrep
  logInfo' "Successfully unregistered DRep."

data ContractPath
  = RegDrep
  | UpdateDrep Anchor
  | UnregDrep

contractStep :: ContractPath -> Contract Ed25519KeyHash
contractStep path = do
  drepPkh <- ownDrepPubKeyHash
  let drepCred = PubKeyHashCredential drepPkh
  drepDeposit <- _.drepDeposit <<< unwrap <$> getProtocolParameters

  let
    submitTx = do
      tx <- submitTxFromBuildPlan Map.empty mempty
        [ IssueCertificate
            ( case path of
                RegDrep ->
                  RegDrepCert drepCred drepDeposit Nothing
                UpdateDrep anchor ->
                  UpdateDrepCert drepCred $ Just anchor
                UnregDrep ->
                  UnregDrepCert drepCred drepDeposit
            )
            Nothing
        ]
      awaitTxConfirmed $ Transaction.hash tx

  submitTx `catchError` \err ->
    unless
      (String.contains (Pattern "knownDelegateRepresentative") $ message err)
      (throwError err)

  pure drepPkh
