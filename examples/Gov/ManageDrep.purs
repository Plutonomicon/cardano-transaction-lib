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
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Governance (queryRegisteredDrepInfo)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction
  ( awaitTxConfirmed
  , defaultBalancer
  , emptyBalancerCtx
  , submitTxFromBlueprint
  )
import Contract.Wallet (ownDrepPubKeyHash)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Gov.Internal.Common (dummyAnchor)
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
  drepInfo <- queryRegisteredDrepInfo drepCred
  case drepInfo, path of
    Just _, RegDrep ->
      logInfo' "DRep already registered. Skipping registration contract."
    _, _ -> do
      drepDeposit <-
        case path of
          RegDrep ->
            _.drepDeposit <<< unwrap <$> getProtocolParameters
          _ ->
            liftMaybe (error "Could not get DRep info")
              (_.deposit <$> drepInfo)
      { txHash } <- submitTxFromBlueprint
        { buildSteps:
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
        , balancer: defaultBalancer
        , balancerCtx: emptyBalancerCtx
        }
      awaitTxConfirmed txHash
  pure drepPkh
