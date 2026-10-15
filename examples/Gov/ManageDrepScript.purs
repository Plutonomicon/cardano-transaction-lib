module Ctl.Examples.Gov.ManageDrepScript
  ( ContractPath(RegDrep, UpdateDrep, UnregDrep)
  , contract
  , contractStep
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(IssueCertificate)
  )
import Cardano.Types
  ( Anchor
  , Certificate(RegDrepCert, UpdateDrepCert, UnregDrepCert)
  , Credential(ScriptHashCredential)
  , ScriptHash
  )
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.RedeemerDatum (unit) as RedeemerDatum
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
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Gov.Internal.Common (dummyAnchor)
import Ctl.Examples.PlutusV3.Scripts.AlwaysMints (alwaysMintsPolicyScriptV3)
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
  logInfo' "Running Examples.Gov.ManageDrepScript"
  drepScriptHash <- contractStep RegDrep
  logInfo' $ "Successfully registered DRep. DRepID: " <> show drepScriptHash
  void $ contractStep $ UpdateDrep dummyAnchor
  logInfo' "Successfully updated DRep metadata."
  void $ contractStep UnregDrep
  logInfo' "Successfully unregistered DRep."

data ContractPath
  = RegDrep
  | UpdateDrep Anchor
  | UnregDrep

contractStep :: ContractPath -> Contract ScriptHash
contractStep path = do
  drepScript <- alwaysMintsPolicyScriptV3
  let
    drepScriptHash = PlutusScript.hash drepScript
    drepCred = ScriptHashCredential drepScriptHash
    drepCredWitness = PlutusScriptCredential (ScriptValue drepScript)
      RedeemerDatum.unit
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
            [ case path of
                RegDrep ->
                  IssueCertificate (RegDrepCert drepCred drepDeposit Nothing)
                    (Just drepCredWitness)
                UpdateDrep anchor ->
                  IssueCertificate (UpdateDrepCert drepCred $ Just anchor)
                    (Just drepCredWitness)
                UnregDrep ->
                  IssueCertificate (UnregDrepCert drepCred drepDeposit)
                    (Just drepCredWitness)
            ]
        , balancer: defaultBalancer
        , balancerCtx: emptyBalancerCtx
        }
      awaitTxConfirmed txHash
  pure drepScriptHash
