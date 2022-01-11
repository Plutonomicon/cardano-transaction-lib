module BalanceTx where

import Prelude
import Data.Array as Array
import Data.Either (Either(..), hush, note)
import Data.List (List)
import Data.List as List
-- import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Tuple.Nested ((/\), type (/\))
import Undefined (undefined)

import Types.JsonWsp as JsonWsp
import Types.Transaction as Transaction
import Value (isAdaOnly)

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
addTxCollaterals
  :: JsonWsp.UtxoQR
  -> Transaction.TxBody
  -> Either String Transaction.TxBody
addTxCollaterals utxos txBody = do
  let txIns :: List Transaction.TransactionInput
      txIns =
        List.mapMaybe (hush <<< ogTxToTransactionInput) <<<
          Map.toUnfoldable <<< filterAdaOnly <<< unwrap $ utxos
  txIn :: Transaction.TransactionInput <- findPubKeyTxIn txIns
  pure $
    over Transaction.TxBody _{ collateral = Just (Array.singleton txIn) } txBody
  where
    filterAdaOnly :: JsonWsp.UtxoQueryResult -> JsonWsp.UtxoQueryResult
    filterAdaOnly = Map.filter (isAdaOnly <<< _.value)

    -- FIX ME: Plutus has Maybe TxInType e.g. Just ConsumePublicKeyAddress)
    -- for now, we take the head.
    findPubKeyTxIn
      :: List Transaction.TransactionInput
      -> Either String Transaction.TransactionInput
    findPubKeyTxIn =
      note "There are no utxos to be used as collateral" <<< List.head
{- | Pick a collateral from the utxo map and add it to the unbalanced transaction
 (suboptimally we just pick a random utxo from the tx inputs)
-}
-- addTxCollaterals :: Map TxOutRef TxOut -> Tx -> Either Text Tx
-- addTxCollaterals utxos tx = do
--   let txIns = mapMaybe (rightToMaybe . txOutToTxIn) $ Map.toList $ filterAdaOnly utxos
--   txIn <- findPubKeyTxIn txIns
--   pure $ tx {txCollateral = Set.singleton txIn}
--   where
--     findPubKeyTxIn = \case
--       x@(TxIn _ (Just ConsumePublicKeyAddress)) : _ -> Right x
--       x@(TxIn _ Nothing) : _ -> Right x
--       _ : xs -> findPubKeyTxIn xs
--       _ -> Left "There are no utxos to be used as collateral"
--     filterAdaOnly = Map.filter (isAdaOnly . txOutValue)

-- Converting an Ogmios transaction output to a transaction input type
ogTxToTransactionInput
  :: (JsonWsp.TxOutRef /\ JsonWsp.OgmiosTxOut)
  -> Either String Transaction.TransactionInput
ogTxToTransactionInput (txOutRef /\ ogmiosTxOut) =
  case ogTxOutAddressCredentials ogmiosTxOut of
    Transaction.PubKeyCredential _ ->
      Right $ txOutRefToTransactionInput txOutRef
    Transaction.ScriptCredential _ ->
      Left "ogTxToTransactionInput: Cannot convert an Ogmios output to \
        \TransactionInput"
  where
    -- FIX ME:
    txOutRefToTransactionInput
      :: JsonWsp.TxOutRef
      -> Transaction.TransactionInput
    txOutRefToTransactionInput = undefined

-- FIX ME: address of OgmiosTxOut doesn't line up well with Transaction TxOut.
-- So method of extracting credentials is unknown at the momemnt
ogTxOutAddressCredentials :: JsonWsp.OgmiosTxOut -> Transaction.Credential
ogTxOutAddressCredentials ogmiosTxOut =
  Transaction.PubKeyCredential ogmiosTxOut.address

