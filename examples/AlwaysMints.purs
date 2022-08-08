-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a value using the `AlwaysMints` policy
module Examples.AlwaysMints (main, alwaysMintsPolicy) where

import Contract.Prelude

import Contract.Config (testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractAffM
  , liftContractM
  , liftedE
  , liftedM
  , runContract
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceAndSignTxM
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt as BigInt
import Contract.Test.E2E (publishTestFeedback)

main :: Effect Unit
main = launchAff_ $ do
  runContract testnetNamiConfig $ do
    logInfo' "Running Examples.AlwaysMints"
    mp <- alwaysMintsPolicy
    cs <- liftContractAffM "Cannot get cs" $ Value.scriptCurrencySymbol mp
    tn <- liftContractM "Cannot make token name"
      $ Value.mkTokenName
      =<< byteArrayFromAscii "TheToken"

    let
      constraints :: Constraints.TxConstraints Void Void
      constraints = Constraints.mustMintValue
        $ Value.singleton cs tn
        $ BigInt.fromInt 100

      lookups :: Lookups.ScriptLookups Void
      lookups = Lookups.mintingPolicy mp

    ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    bsTx <-
      liftedM "Failed to balance/sign tx" $ balanceAndSignTxM ubTx
    txId <- submit bsTx
    logInfo' $ "Tx ID: " <> show txId

    awaitTxConfirmed txId
    logInfo' $ "Tx submitted successfully!"

  publishTestFeedback true

foreign import alwaysMints :: String

alwaysMintsPolicy :: Contract () MintingPolicy
alwaysMintsPolicy = wrap <<< wrap <$> textEnvelopeBytes alwaysMints
  PlutusScriptV1
