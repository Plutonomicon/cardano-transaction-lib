module Examples.KeyWallet.SendsToken where

import Contract.Prelude

import Contract.Monad
  ( Contract
  , liftContractAffM
  , liftContractM
  , liftedE
  , liftedM
  , logInfo'
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import Contract.Transaction (balanceAndSignTx, submit, plutusV1Script)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)

main :: Effect Unit
main = runKeyWalletContract_ \pkh lovelace unlock -> do
  logInfo' "Running Examples.KeyWallet.SendsToken"

  mp <- alwaysMintsPolicy
  cs <- liftContractAffM "Cannot get cs" $ Value.scriptCurrencySymbol mp
  tn <- liftContractM "Cannot make token name"
    $ Value.mkTokenName
    =<< byteArrayFromAscii "TheToken"

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustPayToPubKey pkh
        (Value.lovelaceValueOf lovelace <> Value.singleton cs tn one)

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <- liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  liftEffect unlock

foreign import alwaysMints :: String

alwaysMintsPolicy :: Contract () MintingPolicy
alwaysMintsPolicy = wrap <<< plutusV1Script <$> textEnvelopeBytes alwaysMints
  PlutusScriptV1
