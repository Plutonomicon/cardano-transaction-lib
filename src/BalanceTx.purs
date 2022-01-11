module BalanceTx where

import Prelude
import Data.Either (Either(..))
import Map (Map)
import Map as Map

import Types.JsonWsp (UtxoQueryResult)

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs

addTxCollaterals
  :: UtxoQueryResult
  -> Transaction
  -> Either String Transaction
addTxCollaterals utxos tx = do
  pure tx
  where
    filterAdaOnly :: 

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