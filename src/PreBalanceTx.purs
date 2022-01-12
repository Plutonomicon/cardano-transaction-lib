module PreBalanceTx where

import Prelude
import Data.Array as Array
import Data.Either (Either(..), hush, note)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (over, unwrap)
import Data.Tuple.Nested ((/\), type (/\))
-- import Undefined (undefined)

import Types.Transaction as Transaction
import Value (emptyValue, filterNonAda, isAdaOnly, minus)

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs

-- | Pick a collateral from the utxo map and add it to the unbalanced transaction
-- | (suboptimally we just pick a random utxo from the tx inputs)
addTxCollaterals
  :: Transaction.Utxo
  -> Transaction.TxBody
  -> Either String Transaction.TxBody
addTxCollaterals utxos txBody = do
  let txIns :: List Transaction.TransactionInput
      txIns =
        List.mapMaybe (hush <<< toEitherTransactionInput)
          <<< Map.toUnfoldable
          <<< filterAdaOnly $ utxos
  txIn :: Transaction.TransactionInput <- findPubKeyTxIn txIns
  pure $
    over Transaction.TxBody _{ collateral = Just (Array.singleton txIn) } txBody
  where
    filterAdaOnly :: Transaction.Utxo -> Transaction.Utxo
    filterAdaOnly = Map.filter (isAdaOnly <<< unwrapAmount)

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
toEitherTransactionInput
  :: (Transaction.TransactionInput /\ Transaction.TransactionOutput)
  -> Either String Transaction.TransactionInput
toEitherTransactionInput (txOutRef /\ txOut) =
  case txOutAddressCredentials txOut of
    Transaction.Credential _ ->
      Right txOutRef
    _ -> -- Currently unreachable:
      Left "toEitherTransactionInput: Cannot convert an output to \
        \TransactionInput"

-- FIX ME: do we need granularity for staking credential?
txOutAddressCredentials
  :: Transaction.TransactionOutput
  -> Transaction.Credential
txOutAddressCredentials = _.payment <<< unwrap
  <<< _."AddrType" <<< unwrap
  <<< _.address  <<< unwrap

-- -- FIX ME: This behaves differently to pubKeyTxIn because of TxInType, see
-- -- https://play.marlowe-finance.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Tx.html#pubKeyTxIn
-- txOutRefToTransactionInput :: JsonWsp.TxOutRef -> Transaction.TransactionInput
-- txOutRefToTransactionInput { txId, index } =
--   Transaction.TransactionInput { transaction_id: txId, index }

-- | We need to balance non ada values, as the cardano-cli is unable to balance
-- | them (as of 2021/09/24)
balanceNonAdaOuts
  :: String -- address for change
  -> Transaction.Utxo
  -> Transaction.TxBody
  -> Either String Transaction.TxBody
balanceNonAdaOuts changeAddr utxos txBody =
  let unwrapTxBody = unwrap txBody
      inputValue =
        Array.foldMap
          unwrapAmount
          (Array.mapMaybe (flip Map.lookup utxos) <<< _.inputs $ unwrapTxBody)
            :: Transaction.Value
      outputValue =
        Array.foldMap unwrapAmount (_.outputs unwrapTxBody)
          :: Transaction.Value
      nonMintedOutputValue =
        outputValue `minus` (fromMaybe emptyValue unwrapTxBody.mint)
          :: Transaction.Value
      nonAdaChange =
        filterNonAda inputValue `minus` filterNonAda nonMintedOutputValue
          :: Transaction.Value
   in pure txBody

unwrapAmount :: Transaction.TransactionOutput -> Transaction.Value
unwrapAmount = _.amount <<< unwrap

-- balanceNonAdaOuts :: PubKeyHash -> Map TxOutRef TxOut -> Tx -> Either Text Tx
-- balanceNonAdaOuts addr utxos tx =
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
