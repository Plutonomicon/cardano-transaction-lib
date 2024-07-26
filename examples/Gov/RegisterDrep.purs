module Ctl.Examples.Gov.RegisterDrep
  ( contract
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Transaction.Builder (TransactionBuilderStep(IssueCertificate))
import Cardano.Types
  ( Anchor(Anchor)
  , Certificate(RegDrepCert, UpdateDrepCert, UnregDrepCert)
  , Credential(PubKeyHashCredential)
  , Ed25519KeyHash
  , URL(URL)
  )
import Cardano.Types.Transaction (hash) as Transaction
import Contract.Config
  ( ContractParams
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction (awaitTxConfirmed, submitTxFromBuildPlan)
import Contract.Wallet (ownDrepPubKeyHash)
import Data.Map (empty) as Map
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec = Just $ ConnectToGenericCip30 "eternl" { cip95: true }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.Gov.RegisterDrep"
  drepPkh <- contractStep RegDrep
  logInfo' $ "Successfully registered DRep. DRepID: " <> show drepPkh
  void $ contractStep $ UpdateDrep $ Anchor
    { url: URL "https://example.com/"
    , dataHash:
        unsafePartial $ fromJust $ decodeCbor $ wrap $
          hexToByteArrayUnsafe
            "94b8cac47761c1140c57a48d56ab15d27a842abff041b3798b8618fa84641f5a"
    }
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
  pure drepPkh
