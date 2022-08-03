module Contract.Extra
  ( OneShotCurrency
  , mintContract
  , mintedValue
  , oneshotPolicy
  ) where

import Prelude

import Cardano.TextEnvelope (TextEnvelopeType(PlutusScriptV1))
import Contract.Address (ownPaymentPubKeyHash, ownStakePubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractAffM
  , liftContractE
  , liftContractM
  , liftedE
  , liftedM
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, applyArgs)
import Contract.TextEnvelope (textEnvelopeBytes)
import Contract.Transaction (awaitTxConfirmed, balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value as Value
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested (type (/\), (/\))
import Plutus.Types.Address (pubKeyHashAddress)
import ToData (toData)
import Types.Transaction (TransactionInput)
import Types.PubKeyHash (PaymentPubKeyHash, StakePubKeyHash)

import Types.ByteArray (ByteArray, hexToByteArray)
import Types.Cbor (toByteArray)

-- Minting policy parameterised on a txOutRef, requires the spending of said ref to passs. Makes no assertions about the tokens minted.
-- Script size 320
oneshotPolicyStr :: String
oneshotPolicyStr =
  "5901c5010000323232323232323232222332232325333009332300b22533300e00114a02a6646601a0022944c00cc0400044c008c03c0048c8cdd7803000980718048009bac001149854cc03924114446f65736e277420636f6e73756d65207574786f0016300c3007001300b30060013232323200753330083370e90000010991919299980599b87001480004c94ccc030cdc39998069112999808800880109980199b8000248008c0480052000001480084c8c8c8c8c8c9265333013001149854cc0592401186c697374206973206c6f6e676572207468616e207a65726f00163013003375a0026024002602000c6eb8c04000454cc04524011a68617320746f206861766520612073696e676c65206669656c6400163011002153301049012068617320746f207573652074686520666972737420636f6e7374727563746f7200163011001375400260180022a6601a92013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f720016300d002300d0013754006002460106ea80055cd119180111980100100091801119801001000aab9f5744ae855ce2ab9e5573a1"

hexToCbor :: String -> Either String ByteArray
hexToCbor str = note "Failed to decode hex" $ hexToByteArray str

oneshotPolicy :: TransactionInput -> Contract () MintingPolicy
oneshotPolicy ref = do
  pScript <- wrap <<< wrap <$> liftContractE (hexToCbor oneshotPolicyStr)
  liftedE $ applyArgs pScript [ toData ref ]

type OneShotCurrency =
  { oscAmounts :: Map.Map TokenName BigInt.BigInt
  , oscCurrencySymbol :: CurrencySymbol
  , oscTxInput :: TransactionInput
  }

mintedValue :: OneShotCurrency -> Value
mintedValue oneShot = foldMapWithIndex (Value.singleton oneShot.oscCurrencySymbol) $ oneShot.oscAmounts

mustPayToPubKeyStakeAddress
  :: forall (o :: Type) (i :: Type)
   . PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> Value
  -> Constraints.TxConstraints i o
mustPayToPubKeyStakeAddress pkh Nothing = Constraints.mustPayToPubKey pkh
mustPayToPubKeyStakeAddress pkh (Just stk) =
  Constraints.mustPayToPubKeyAddress pkh stk

mintContract
  :: Array (TokenName /\ BigInt.BigInt) -> Contract () OneShotCurrency
mintContract amts = do
  pkh <- liftedM "Failed to get fetch own pubkeyhash" ownPaymentPubKeyHash
  stk <- ownStakePubKeyHash
  utxos <- unwrap <$> liftedM "Failed to fetch UTxOs"
    (utxosAt $ pubKeyHashAddress pkh stk)
  (txIn /\ txOut) <- liftContractM "No UTxOs at own address" $ head $
    Map.toUnfoldable utxos

  mp <- oneshotPolicy txIn

  cs <- liftContractAffM "Cannot get cs" $ Value.scriptCurrencySymbol mp

  let
    toValue :: TokenName /\ BigInt.BigInt -> Value
    toValue (tn /\ amt) = Value.singleton cs tn amt

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValue (foldMap toValue amts)
        <> Constraints.mustSpendPubKeyOutput txIn
        <> mustPayToPubKeyStakeAddress pkh stk (unwrap txOut).amount

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs utxos

  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <-
    liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId

  awaitTxConfirmed txId
  logInfo' $ "Tx submitted successfully!"

  pure
    { oscAmounts: Map.fromFoldable amts
    , oscCurrencySymbol: cs
    , oscTxInput: txIn
    }
