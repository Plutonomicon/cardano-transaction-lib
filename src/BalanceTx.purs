module BalanceTx where

import Prelude
-- import Data.BigInt as BigInt
import Data.Either (Either(..), hush)
-- import Data.List as List
-- import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (over)
import Data.Tuple.Nested (type (/\), (/\))
import Undefined (undefined)

import Types.JsonWsp as JsonWsp
import Types.Transaction as Transaction
import Value (isAdaOnly)

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
addTxCollaterals
  :: JsonWsp.UtxoQR
  -> Transaction.Transaction
  -> Either String Transaction.Transaction
addTxCollaterals utxos tx = do
  pure tx
  -- let txIns =  List.mapMaybe (hush <<< ) $ Map.toUnfoldable $ filterAdaOnly utxos
  where
    filterAdaOnly :: JsonWsp.UtxoQR -> JsonWsp.UtxoQR
    filterAdaOnly =
      over JsonWsp.UtxoQR $ Map.filter (isAdaOnly <<< _.value)

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

-- | Convert a value to a simple list, keeping only the non-zero amounts.

-- Converting an Ogmios transaction output to a transaction input type
ogTxToTransactionInput
  :: (JsonWsp.TxOutRef /\ JsonWsp.OgmiosTxOut)
  -> Either String Transaction.TransactionInput
ogTxToTransactionInput (txOutRef /\ ogmiosTxOut) =
  case ogTxOutAddressCredentials ogmiosTxOut of
    Transaction.PubKeyCredential _ ->
      Right $ txOutRefToTransactionInput txOutRef
    Transaction.ScriptCredential _ ->
      Left "Cannot convert an Ogmios output to TransactionInput"
  where
    txOutRefToTransactionInput
      :: JsonWsp.TxOutRef
      -> Transaction.TransactionInput
    txOutRefToTransactionInput = undefined

-- FIX ME: address of OgmiosTxOut doesn't line up well with Transaction TxOut.
-- So method of extracting credentials is unknown at the momemnt
ogTxOutAddressCredentials :: JsonWsp.OgmiosTxOut -> Transaction.Credential
ogTxOutAddressCredentials ogmiosTxOut =
  Transaction.PubKeyCredential ogmiosTxOut.address

