-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a token using the `AlwaysMints` policy and sends it along with
-- | the selected amount to the specified address.
module Examples.KeyWallet.MintsAndSendsToken (main) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractAffM
  , liftContractM
  , liftedE
  , liftedM
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import Contract.Transaction (balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Examples.AlwaysMints (alwaysMintsPolicy)
import Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)

main :: Effect Unit
main = runKeyWalletContract_ \pkh lovelace unlock -> do
  logInfo' "Running Examples.KeyWallet.MintsAndSendsToken"

  mp <- alwaysMintsPolicy
  cs <- liftContractAffM "Cannot get cs" $ Value.scriptCurrencySymbol mp
  tn <- liftContractM "Cannot make token name"
    $ Value.mkTokenName
    =<< byteArrayFromAscii "TheToken"

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = mconcat
      [ Constraints.mustMintValue (Value.singleton cs tn one)
      , Constraints.mustPayToPubKey pkh
          (Value.lovelaceValueOf lovelace <> Value.singleton cs tn one)
      ]

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <- liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  liftEffect unlock
