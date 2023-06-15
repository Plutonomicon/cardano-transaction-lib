-- | This module demonstrates how various assertions from `Contract.Test.Utils`
-- | can be used to test `Contract`s. It creates a transaction with metadata
-- | that performs three actions: (1) sends some amount of Ada to the receiver's
-- | address, (2) mints the specified non-Ada value (3) then sends it to the
-- | owner's address with a datum attached.
module Ctl.Examples.ContractTestUtils
  ( ContractParams
  , mkContract
  , mkChecks
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  , PaymentPubKeyHash
  , StakePubKeyHash
  , getNetworkId
  , payPubKeyHashBaseAddress
  , payPubKeyHashEnterpriseAddress
  )
import Contract.AuxiliaryData (setTxMetadata)
import Contract.Hashing (datumHash)
import Contract.Log (logInfo')
import Contract.Metadata (Cip25Metadata)
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (Datum, OutputDatum(OutputDatumHash))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Test.Assert
  ( ContractCheck
  , assertOutputHasDatum
  , assertOutputHasRefScript
  , assertTxHasMetadata
  , assertionToCheck
  , checkExUnitsNotExceed
  , checkGainAtAddress'
  , checkLossAtAddress
  , checkTokenGainAtAddress'
  , label
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionOutputWithRefScript
  , TransactionUnspentOutput
  , _output
  , awaitTxConfirmed
  , balanceTx
  , getTxFinalFee
  , lookupTxHash
  , scriptRefFromMintingPolicy
  , signTransaction
  , submit
  )
import Contract.TxConstraints (DatumPresence(DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (lovelaceValueOf, singleton) as Value
import Contract.Wallet
  ( getWalletAddresses
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  )
import Ctl.Examples.Helpers (mustPayToPubKeyStakeAddress) as Helpers
import Data.Array (head)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Lens (view)
import Effect.Exception (throw)

type ContractParams =
  { receiverPkh :: PaymentPubKeyHash
  , receiverSkh :: Maybe StakePubKeyHash
  , adaToSend :: BigInt
  , mintingPolicy :: MintingPolicy
  , tokensToMint :: Tuple3 CurrencySymbol TokenName BigInt
  , datumToAttach :: Datum
  , txMetadata :: Cip25Metadata
  }

type ContractResult =
  { txHash :: TransactionHash
  , txFinalFee :: BigInt
  , txOutputUnderTest :: TransactionOutputWithRefScript
  }

mkChecks
  :: ContractParams
  -> Contract (Array (ContractCheck ContractResult))
mkChecks p = do
  senderAddress <-
    liftedM "Failed to get sender address" $ head <$> getWalletAddresses
  receiverAddress <-
    liftedM "Failed to get receiver address" (getReceiverAddress p)
  let dhash = datumHash p.datumToAttach
  pure
    [ checkGainAtAddress' (label receiverAddress "Receiver")
        p.adaToSend

    , checkLossAtAddress (label senderAddress "Sender")
        case _ of
          Just { txFinalFee } -> pure (p.adaToSend + txFinalFee)
          Nothing -> liftEffect $
            throw "Unable to estimate expected loss in wallet"

    , checkTokenGainAtAddress' (label senderAddress "Sender")
        ( uncurry3 (\cs tn amount -> cs /\ tn /\ amount)
            p.tokensToMint
        )

    , checkExUnitsNotExceed
        { mem: BigInt.fromInt 800, steps: BigInt.fromInt 161100 }

    , assertionToCheck "Sender's output has a datum"
        \{ txOutputUnderTest } ->
          assertOutputHasDatum (OutputDatumHash dhash)
            (label txOutputUnderTest "Sender's output with datum hash")

    , assertionToCheck "Output has a reference script"
        \{ txOutputUnderTest } ->
          assertOutputHasRefScript
            (scriptRefFromMintingPolicy p.mintingPolicy)
            (label txOutputUnderTest "Sender's output with reference script")

    , assertionToCheck "Contains CIP-25 metadata" \{ txHash } ->
        assertTxHasMetadata "CIP25 Metadata" txHash p.txMetadata
    ]

mkContract :: ContractParams -> Contract ContractResult
mkContract p = do
  logInfo' "Running Examples.ContractTestUtils"
  ownPkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  ownSkh <- join <<< head <$> ownStakePubKeyHashes
  let
    mustPayToPubKeyStakeAddressWithDatumAndScriptRef =
      ownSkh # maybe Constraints.mustPayToPubKeyWithDatumAndScriptRef
        \skh pkh ->
          Constraints.mustPayToPubKeyAddressWithDatumAndScriptRef pkh skh

    adaValue :: Value
    adaValue = Value.lovelaceValueOf p.adaToSend

    nonAdaValue :: Value
    nonAdaValue = uncurry3 Value.singleton p.tokensToMint

    constraints :: Constraints.TxConstraints Void Void
    constraints = mconcat
      [ Helpers.mustPayToPubKeyStakeAddress p.receiverPkh p.receiverSkh adaValue

      , Constraints.mustMintValue nonAdaValue

      , mustPayToPubKeyStakeAddressWithDatumAndScriptRef ownPkh p.datumToAttach
          DatumWitness
          (scriptRefFromMintingPolicy p.mintingPolicy)
          nonAdaValue
      ]

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy p.mintingPolicy

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  unbalancedTxWithMetadata <- setTxMetadata unbalancedTx p.txMetadata
  balancedTx <- liftedE $ balanceTx unbalancedTxWithMetadata
  balancedSignedTx <- signTransaction balancedTx

  txId <- submit balancedSignedTx
  logInfo' $ "Tx ID: " <> show txId

  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"

  senderAddress <- liftedM "Failed to get sender address" $ head <$>
    getWalletAddresses
  utxos <- utxosAt senderAddress

  txOutputUnderTest <-
    view _output <$>
      liftContractM "Could not find required unspent output with datum hash"
        (find hasDatumHash $ lookupTxHash txId utxos)

  pure
    { txHash: txId
    , txFinalFee: getTxFinalFee balancedSignedTx
    , txOutputUnderTest
    }
  where
  hasDatumHash :: TransactionUnspentOutput -> Boolean
  hasDatumHash = view _output >>> unwrap >>> _.output >>> unwrap >>> _.datum >>>
    case _ of
      OutputDatumHash _ -> true
      _ -> false

getReceiverAddress :: ContractParams -> Contract (Maybe Address)
getReceiverAddress { receiverPkh, receiverSkh } =
  getNetworkId <#> \networkId ->
    case receiverSkh of
      Just skh ->
        payPubKeyHashBaseAddress networkId receiverPkh skh
      Nothing ->
        payPubKeyHashEnterpriseAddress networkId receiverPkh
