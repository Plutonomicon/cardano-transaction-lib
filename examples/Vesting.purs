module Ctl.Examples.Vesting (main, example, contract) where

import Contract.Prelude

import Contract.Address
  ( PaymentPubKeyHash
  , Slot
  , getNetworkId
  , nativeScriptHashEnterpriseAddress
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  )
import Contract.Chain (currentSlot, waitNSlots)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Hashing (nativeScriptHash)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractE
  , liftContractM
  , liftedM
  , runContract
  )
import Contract.Numeric.Natural (fromInt')
import Contract.ScriptLookups as Lookups
import Contract.Test.E2E (publishTestFeedback)
import Contract.Time
  ( from
  , futureSlot
  , getEraSummaries
  , getSystemStart
  , slotToPosixTime
  )
import Contract.Transaction
  ( NativeScript(ScriptAll, ScriptPubkey, TimelockStart)
  , TransactionHash
  , TransactionInput
  , _input
  , awaitTxConfirmed
  , lookupTxHash
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Examples.Helpers
  ( buildBalanceSignAndSubmitTx
  , mustPayToPubKeyStakeAddress
  ) as Helpers
import Data.Array ((!!))
import Data.BigInt as BigInt
import Data.Lens (view)
import Data.Map as Map
import Data.Newtype (unwrap)

main :: Effect Unit
main = example testnetNamiConfig

nativeScript ∷ PaymentPubKeyHash → Slot → NativeScript
nativeScript pkh slot = ScriptAll
  [ ScriptPubkey $ unwrap $ unwrap pkh
  , TimelockStart slot
  ]

lock :: Contract () (Slot /\ TransactionHash)
lock = do
  pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  slot <- currentSlot
  lockExpires <- liftContractM "Error in calculating lock period"
    $ futureSlot slot 10

  let ns = nativeScript pkh lockExpires

  nsHash <- liftContractM "Unable to hash NativeScript" $
    nativeScriptHash ns

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToNativeScript nsHash
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  txId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints
  pure (lockExpires /\ txId)

unlock :: Slot -> TransactionHash -> Contract () TransactionHash
unlock lockExpires txId = do
  pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  skh <- ownStakePubKeyHash
  es <- getEraSummaries
  ss <- getSystemStart
  networkId <- getNetworkId
  lockedRange' <- liftEffect $ slotToPosixTime es ss lockExpires
  lockedRange <- liftContractE $ lockedRange'

  let ns = nativeScript pkh lockExpires

  nsHash <- liftContractM "Unable to hash NativeScript" (nativeScriptHash ns)
  nsAddrPlutus <- liftContractM "Unable to convert to Plutus address" $
    nativeScriptHashEnterpriseAddress networkId nsHash
  utxos <- fromMaybe Map.empty <$> utxosAt nsAddrPlutus
  txInput <-
    liftContractM "Unable to get UTxO"
      $ view _input
      <$> lookupTxHash txId utxos !! 0

  let
    constraints :: TransactionInput -> Constraints.TxConstraints Void Void
    constraints tx =
      ( Helpers.mustPayToPubKeyStakeAddress pkh skh
          $ Value.lovelaceValueOf
          $ BigInt.fromInt 2_000_000
      )
        <> Constraints.mustSpendNativeScriptOutput tx ns
        <> Constraints.mustBeSignedBy pkh
        <> Constraints.mustValidateIn (from lockedRange)

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs utxos

  Helpers.buildBalanceSignAndSubmitTx lookups $ constraints txInput

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.LockUnlock"
  (expires /\ lockTxId) <- lock

  awaitTxConfirmed lockTxId
  logInfo' "Tx submitted successfully, Unlocking funds to own address"
  void $ waitNSlots $ fromInt' 10

  (unlock expires lockTxId) >>= awaitTxConfirmed
  logInfo' "Tx submitted successfully!"

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true
