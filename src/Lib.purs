module Lib where

import Prelude
import Types.Transaction as Types
import Data.Maybe (Maybe(..))
import Data.BigInt as BigInt

-- Ogmios fetching
-- 1. inputs

-- 
tx
  :: Array Types.TransactionInput
  -> Array Types.TransactionOutput
  -> Array Types.TransactionInput
  -> Types.NetworkId
  -> Types.Transaction
tx i o c id = Types.Transaction
  { body: txBody i o c id
  , witness_set: txWitness
  , is_valid: true -- why is this?
  , auxiliary_data: Nothing
  }

txBody
  :: Array Types.TransactionInput
  -> Array Types.TransactionOutput
  -> Array Types.TransactionInput
  -> Types.NetworkId
  -> Types.TxBody
txBody inputs outputs collateral id = Types.TxBody
  { inputs: inputs
  , outputs: outputs
  , fee: Types.Coin $ BigInt.fromInt 1000000
  , ttl: Just $ Types.Slot $ BigInt.fromInt 10010000 -- validity_end
  , certs: Nothing
  , withdrawals: Nothing
  , update: Nothing
  , auxiliary_data_hash: Nothing
  , validity_start_interval: Just $ Types.Slot $ BigInt.fromInt 10000000
  , mint: Nothing
  , script_data_hash: Nothing
  , collateral: Just $ collateral
  , required_signers: Nothing
  , network_id: Just $ id
  }

txWitness :: Types.TransactionWitnessSet
txWitness = Types.TransactionWitnessSet
  { vkeys: Nothing -- maybe this should be Just []
  , native_scripts: Nothing
  , bootstraps: Nothing
  , plutus_scripts: Nothing
  , plutus_data: Nothing
  , redeemers: Nothing
  }

