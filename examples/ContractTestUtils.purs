module Examples.ContractTestUtils
  ( ContractParams(ContractParams)
  , contract
  ) where

import Contract.Prelude

import Contract.Address
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
import Contract.AuxiliaryData (setTxMetadata)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM, liftContractM)
import Contract.PlutusData (Datum)
import Contract.Scripts (MintingPolicy)
import Contract.ScriptLookups as Lookups
import Contract.Test.Utils
  ( ContractBasicAssertion
  , ContractWrapAssertion
  , label
  )
import Contract.Test.Utils as TestUtils
import Contract.Transaction
  ( TransactionHash
  , TransactionOutput
  , awaitTxConfirmed
  , balanceAndSignTxE
  , getTxFinalFee
  , lookupTxHash
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (lovelaceValueOf, singleton) as Value
import Data.BigInt (BigInt)
import Data.Lens (view)
import Data.Map (empty) as Map
import Examples.Helpers
  ( mustPayToPubKeyStakeAddress
  , mustPayWithDatumToPubKeyStakeAddress
  ) as Helpers
import Metadata.Cip25.V2 (Cip25Metadata)
import Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  , _output
  )

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
  , outputWithDatumHash :: TransactionOutput
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
  pure
    $
      [ TestUtils.assertGainAtAddress' (label receiverAddress "Receiver")
          p.adaToSend

      , TestUtils.assertLossAtAddress (label senderAddress "Sender")
          \{ txFinalFee } -> pure (p.adaToSend + txFinalFee)

      , TestUtils.assertTokenGainAtAddress' (label senderAddress "Sender")
          ( uncurry3 (\cs tn amount -> cs /\ tn /\ (one + amount))
              p.tokensToMint
          )
      ]
    /\
      [ \{ outputWithDatumHash } ->
          TestUtils.assertOutputHasDatum (p.datumToAttach)
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

      , Helpers.mustPayWithDatumToPubKeyStakeAddress ownPkh ownSkh
          p.datumToAttach
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
  hasDatumHash =
    isJust <<< _.dataHash <<< unwrap <<< view _output

getReceiverAddress :: ContractParams -> Contract () (Maybe Address)
getReceiverAddress (ContractParams { receiverPkh, receiverSkh }) =
  getNetworkId <#> \networkId ->
    case receiverSkh of
      Just skh ->
        payPubKeyHashBaseAddress networkId receiverPkh skh
      Nothing ->
        payPubKeyHashEnterpriseAddress networkId receiverPkh
