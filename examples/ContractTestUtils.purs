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

import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(Pay, MintAsset)
  )
import Cardano.Types
  ( BigNum
  , Coin
  , ExUnits(ExUnits)
  , PaymentCredential(PaymentCredential)
  , StakeCredential(StakeCredential)
  , TransactionOutput(TransactionOutput)
  , _body
  , _datum
  , _fee
  , _output
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Credential (Credential(PubKeyHashCredential))
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.Int as Int
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.ScriptRef (ScriptRef(PlutusScriptRef))
import Contract.Address (Address, PaymentPubKeyHash, StakePubKeyHash, mkAddress)
import Contract.Hashing (datumHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum, OutputDatum(OutputDatumHash))
import Contract.Test.Assert
  ( ContractCheck
  , assertOutputHasDatum
  , assertOutputHasRefScript
  , assertionToCheck
  , checkExUnitsNotExceed
  , checkGainAtAddress'
  , checkLossAtAddress
  , checkTokenGainAtAddress'
  , label
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionUnspentOutput
  , awaitTxConfirmed
  , balanceTx
  , buildTx
  , lookupTxHash
  , signTransaction
  , submit
  )
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (lovelaceValueOf, singleton) as Value
import Contract.Wallet
  ( getWalletAddresses
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  )
import Data.Array (head)
import Data.Lens (view)
import Data.Map as Map
import Effect.Exception (throw)

type ContractParams =
  { receiverPkh :: PaymentPubKeyHash
  , receiverSkh :: Maybe StakePubKeyHash
  , adaToSend :: Coin
  , mintingPolicy :: PlutusScript
  , tokensToMint :: Tuple3 CurrencySymbol TokenName BigNum
  , datumToAttach :: Datum
  }

type ContractResult =
  { txHash :: TransactionHash
  , txFinalFee :: Coin
  , txOutputUnderTest :: TransactionOutput
  }

mkChecks
  :: ContractParams
  -> Contract (Array (ContractCheck ContractResult))
mkChecks p = do
  senderAddress <-
    liftedM "Failed to get sender address" $ head <$> getWalletAddresses
  receiverAddress <- getReceiverAddress p
  let dhash = datumHash p.datumToAttach
  pure
    [ checkGainAtAddress' (label receiverAddress "Receiver")
        (BigNum.toBigInt $ unwrap p.adaToSend)

    , checkLossAtAddress (label senderAddress "Sender")
        case _ of
          Just { txFinalFee } -> pure
            ( BigNum.toBigInt (unwrap p.adaToSend) + BigNum.toBigInt
                (unwrap txFinalFee)
            )
          Nothing -> liftEffect $
            throw "Unable to estimate expected loss in wallet"

    , checkTokenGainAtAddress' (label senderAddress "Sender")
        ( uncurry3 (\cs tn amount -> cs /\ tn /\ BigNum.toBigInt amount)
            p.tokensToMint
        )

    , checkExUnitsNotExceed
        (ExUnits { mem: BigNum.fromInt 800, steps: BigNum.fromInt 161100 })

    , assertionToCheck "Sender's output has a datum"
        \{ txOutputUnderTest } ->
          assertOutputHasDatum (Just $ OutputDatumHash dhash)
            (label txOutputUnderTest "Sender's output with datum hash")

    , assertionToCheck "Output has a reference script"
        \{ txOutputUnderTest } ->
          assertOutputHasRefScript
            (PlutusScriptRef p.mintingPolicy)
            (label txOutputUnderTest "Sender's output with reference script")

    ]

mkContract :: ContractParams -> Contract ContractResult
mkContract p = do
  logInfo' "Running Examples.ContractTestUtils"
  ownPkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  ownSkh <- join <<< head <$> ownStakePubKeyHashes
  receiverAddress <- mkAddress
    (PaymentCredential $ PubKeyHashCredential $ unwrap p.receiverPkh)
    (StakeCredential <<< PubKeyHashCredential <<< unwrap <$> p.receiverSkh)
  ownAddress <- mkAddress
    (PaymentCredential $ PubKeyHashCredential $ unwrap ownPkh)
    (StakeCredential <<< PubKeyHashCredential <<< unwrap <$> ownSkh)
  let
    adaValue :: Value
    adaValue = Value.lovelaceValueOf (unwrap p.adaToSend)

    nonAdaValue :: Value
    nonAdaValue = uncurry3 Value.singleton p.tokensToMint

    scriptHash /\ assetName /\ mintAmount /\ _ = p.tokensToMint
    plan =
      [ Pay $ TransactionOutput
          { address: receiverAddress
          , amount: adaValue
          , datum: Nothing
          , scriptRef: Nothing
          }
      , MintAsset scriptHash assetName (Int.newPositive mintAmount)
          $ PlutusScriptCredential (ScriptValue p.mintingPolicy)
              RedeemerDatum.unit
      , Pay $ TransactionOutput
          { address: ownAddress
          , amount: nonAdaValue
          , datum: Just $ OutputDatumHash $ hashPlutusData p.datumToAttach
          , scriptRef: Just $ PlutusScriptRef p.mintingPolicy
          }
      ]

  unbalancedTx <- buildTx plan
  balancedTx <- balanceTx unbalancedTx Map.empty mempty
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
    , txFinalFee: view (_body <<< _fee) balancedSignedTx
    , txOutputUnderTest
    }
  where
  hasDatumHash :: TransactionUnspentOutput -> Boolean
  hasDatumHash = view (_output <<< _datum) >>>
    case _ of
      Just (OutputDatumHash _) -> true
      _ -> false

getReceiverAddress :: ContractParams -> Contract Address
getReceiverAddress { receiverPkh, receiverSkh } =
  mkAddress (wrap $ PubKeyHashCredential $ unwrap receiverPkh)
    (wrap <<< PubKeyHashCredential <<< unwrap <$> receiverSkh)
