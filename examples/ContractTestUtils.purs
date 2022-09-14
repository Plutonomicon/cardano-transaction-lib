-- | This module demonstrates how various assertions from `Contract.Test.Utils`
-- | can be used to test `Contract`s. It creates a transaction with metadata 
-- | that performs three actions: (1) sends some amount of Ada to the receiver's 
-- | address, (2) mints the specified non-Ada value (3) then sends it to the 
-- | owner's address with a datum attached. 
module CTL.Examples.ContractTestUtils
  ( ContractParams(ContractParams)
  , contract
  ) where

import CTL.Contract.Prelude

import CTL.Contract.Address
  ( Address
  , PaymentPubKeyHash
  , StakePubKeyHash
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , payPubKeyHashBaseAddress
  , payPubKeyHashEnterpriseAddress
  )
import CTL.Contract.AuxiliaryData (setTxMetadata)
import CTL.Contract.Hashing (datumHash)
import CTL.Contract.Log (logInfo')
import CTL.Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import CTL.Contract.PlutusData (Datum, OutputDatum(OutputDatumHash))
import CTL.Contract.ScriptLookups as Lookups
import CTL.Contract.Scripts (MintingPolicy)
import CTL.Contract.Test.Utils
  ( ContractBasicAssertion
  , ContractWrapAssertion
  , label
  )
import CTL.Contract.Test.Utils as TestUtils
import CTL.Contract.Transaction
  ( TransactionHash
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , balanceAndSignTxE
  , getTxFinalFee
  , lookupTxHash
  , submit
  )
import CTL.Contract.TxConstraints (DatumPresence(DatumWitness))
import CTL.Contract.TxConstraints as Constraints
import CTL.Contract.Utxos (utxosAt)
import CTL.Contract.Value (CurrencySymbol, TokenName, Value)
import CTL.Contract.Value (lovelaceValueOf, singleton) as Value
import CTL.Examples.Helpers
  ( mustPayToPubKeyStakeAddress
  , mustPayToPubKeyStakeAddressWithDatum
  ) as Helpers
import CTL.Internal.Metadata.Cip25.V2 (Cip25Metadata)
import CTL.Internal.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  , _output
  )
import Data.BigInt (BigInt)
import Data.Lens (view)
import Data.Map (empty) as Map

newtype ContractParams = ContractParams
  { receiverPkh :: PaymentPubKeyHash
  , receiverSkh :: Maybe StakePubKeyHash
  , adaToSend :: BigInt
  , mintingPolicy :: MintingPolicy
  , tokensToMint :: Tuple3 CurrencySymbol TokenName BigInt
  , datumToAttach :: Datum
  , txMetadata :: Cip25Metadata
  }

derive instance Newtype ContractParams _

type ContractResult =
  { txHash :: TransactionHash
  , txFinalFee :: BigInt
  , outputWithDatumHash :: TransactionOutputWithRefScript
  }

mkAssertions
  :: ContractParams
  -> Contract ()
       ( Array (ContractWrapAssertion () ContractResult)
           /\ Array (ContractBasicAssertion () ContractResult Unit)
       )
mkAssertions params@(ContractParams p) = do
  senderAddress <-
    liftedM "Failed to get sender address" getWalletAddress
  receiverAddress <-
    liftedM "Failed to get receiver address" (getReceiverAddress params)
  dhash <- liftContractM "Failed to hash datum" $ datumHash $ p.datumToAttach
  pure
    $
      [ TestUtils.assertGainAtAddress' (label receiverAddress "Receiver")
          p.adaToSend

      , TestUtils.assertLossAtAddress (label senderAddress "Sender")
          \{ txFinalFee } -> pure (p.adaToSend + txFinalFee)

      , TestUtils.assertTokenGainAtAddress' (label senderAddress "Sender")
          ( uncurry3 (\cs tn amount -> cs /\ tn /\ amount)
              p.tokensToMint
          )
      ]
    /\
      [ \{ outputWithDatumHash } -> do
          TestUtils.assertOutputHasDatum (OutputDatumHash dhash)
            (label outputWithDatumHash "Sender's output with datum hash")

      , \{ txHash } ->
          TestUtils.assertTxHasMetadata "CIP25 Metadata" txHash p.txMetadata
      ]

contract :: ContractParams -> Contract () Unit
contract params@(ContractParams p) = do
  logInfo' "Running Examples.ContractTestUtils"
  ownPkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  ownSkh <- ownStakePubKeyHash
  let
    adaValue :: Value
    adaValue = Value.lovelaceValueOf p.adaToSend

    nonAdaValue :: Value
    nonAdaValue = uncurry3 Value.singleton p.tokensToMint

    constraints :: Constraints.TxConstraints Void Void
    constraints = mconcat
      [ Helpers.mustPayToPubKeyStakeAddress p.receiverPkh p.receiverSkh adaValue

      , Constraints.mustMintValue nonAdaValue

      , Helpers.mustPayToPubKeyStakeAddressWithDatum ownPkh ownSkh
          p.datumToAttach
          DatumWitness
          nonAdaValue
      ]

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy p.mintingPolicy

  assertions <- mkAssertions params
  void $ TestUtils.withAssertions assertions do
    unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    unbalancedTxWithMetadata <- setTxMetadata unbalancedTx p.txMetadata
    balancedSignedTx <- liftedE $ balanceAndSignTxE unbalancedTxWithMetadata

    txId <- submit balancedSignedTx
    logInfo' $ "Tx ID: " <> show txId

    awaitTxConfirmed txId
    logInfo' "Tx submitted successfully!"

    senderAddress <- liftedM "Failed to get sender address" getWalletAddress
    utxos <- fromMaybe Map.empty <$> utxosAt senderAddress

    outputWithDatumHash <-
      view _output <$>
        liftContractM "Could not find required unspent output with datum hash"
          (find hasDatumHash $ lookupTxHash txId utxos)

    pure
      { txHash: txId
      , txFinalFee: getTxFinalFee balancedSignedTx
      , outputWithDatumHash
      }
  where
  hasDatumHash :: TransactionUnspentOutput -> Boolean
  hasDatumHash = view _output >>> unwrap >>> _.output >>> unwrap >>> _.datum >>>
    case _ of
      OutputDatumHash _ -> true
      _ -> false

getReceiverAddress :: ContractParams -> Contract () (Maybe Address)
getReceiverAddress (ContractParams { receiverPkh, receiverSkh }) =
  getNetworkId <#> \networkId ->
    case receiverSkh of
      Just skh ->
        payPubKeyHashBaseAddress networkId receiverPkh skh
      Nothing ->
        payPubKeyHashEnterpriseAddress networkId receiverPkh
