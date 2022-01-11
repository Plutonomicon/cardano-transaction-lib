module PreBalanceTx where

import Prelude
import Data.Array as Array
import Data.Either (Either(..), hush, note)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Tuple.Nested ((/\), type (/\))
-- import Undefined (undefined)

import Types.JsonWsp as JsonWsp
import Types.Transaction as Transaction
import Value (isAdaOnly)

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs

-- | Pick a collateral from the utxo map and add it to the unbalanced transaction
-- (suboptimally we just pick a random utxo from the tx inputs)
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
    -- for now, we take the head. The Haskell logic is pasted below:
    -- findPubKeyTxIn = \case
    --   x@(TxIn _ (Just ConsumePublicKeyAddress)) : _ -> Right x
    --   x@(TxIn _ Nothing) : _ -> Right x
    --   _ : xs -> findPubKeyTxIn xs
    --   _ -> Left "There are no utxos to be used as collateral"
    findPubKeyTxIn
      :: List Transaction.TransactionInput
      -> Either String Transaction.TransactionInput
    findPubKeyTxIn =
      note "addTxCollaterals: There are no utxos to be used as collateral"
        <<< List.head

-- Converting an Ogmios transaction output to a transaction input type
-- FIX ME: may need to revisit for credential granularity.
ogTxToTransactionInput
  :: (JsonWsp.TxOutRef /\ JsonWsp.OgmiosTxOut)
  -> Either String Transaction.TransactionInput
ogTxToTransactionInput (txOutRef /\ ogmiosTxOut) =
  case ogTxOutAddressCredentials ogmiosTxOut of
    Transaction.Credential _ ->
      Right $ txOutRefToTransactionInput txOutRef
    _ -> -- Currently unreachable:
      Left "ogTxToTransactionInput: Cannot convert an Ogmios output to \
        \TransactionInput"

-- FIX ME: address of OgmiosTxOut doesn't line up well with Transaction TxOut.
-- So method of extracting credentials is unknown at the moment.
ogTxOutAddressCredentials :: JsonWsp.OgmiosTxOut -> Transaction.Credential
ogTxOutAddressCredentials ogmiosTxOut =
  Transaction.Credential ogmiosTxOut.address

-- FIX ME: This behaves differently to pubKeyTxIn because of TxInType, see
-- https://play.marlowe-finance.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Tx.html#pubKeyTxIn
txOutRefToTransactionInput :: JsonWsp.TxOutRef -> Transaction.TransactionInput
txOutRefToTransactionInput { txId, index } =
  Transaction.TransactionInput { transaction_id: txId, index }

-- -- | We need to balance non ada values, as the cardano-cli is unable to balance them (as of 2021/09/24)
-- balanceNonAdaOuts :: PubKeyHash -> Map TxOutRef TxOut -> Tx -> Either Text Tx
-- balanceNonAdaOuts ownPkh utxos tx =
--   let changeAddr = Ledger.pubKeyHashAddress ownPkh
--       txInRefs = map Tx.txInRef $ Set.toList $ txInputs tx
--       inputValue = mconcat $ map Tx.txOutValue $ mapMaybe (`Map.lookup` utxos) txInRefs
--       outputValue = mconcat $ map Tx.txOutValue $ txOutputs tx
--       nonMintedOutputValue = outputValue `minus` txMint tx
--       nonAdaChange = filterNonAda inputValue `minus` filterNonAda nonMintedOutputValue
--       outputs =
--         case partition ((==) changeAddr . Tx.txOutAddress) $ txOutputs tx of
--           ([], txOuts) ->
--             TxOut
--               { txOutAddress = changeAddr
--               , txOutValue = nonAdaChange
--               , txOutDatumHash = Nothing
--               } :
--             txOuts
--           (txOut@TxOut {txOutValue = v} : txOuts, txOuts') ->
--             txOut {txOutValue = v <> nonAdaChange} : (txOuts <> txOuts')
--    in if isValueNat nonAdaChange
--         then Right $ if Value.isZero nonAdaChange then tx else tx {txOutputs = outputs}
--         else Left "Not enough inputs to balance tokens."