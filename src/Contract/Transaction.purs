-- | A module that defines the different transaction data types, balancing
-- | functionality, signing and submission.
module Contract.Transaction
  ( balanceTx
  , balanceTxM
  , signTransaction
  , submitTransaction
  , module BalanceTxError
  , module JsonWsp
  , module Transaction
  , module TxOutput
  , module UnbalancedTx
  ) where

import Prelude
import BalanceTx (balanceTx) as BalanceTx
import BalanceTx (BalanceTxError) as BalanceTxError
import Contract.Monad (Contract)
import Data.Either (Either, hush)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import QueryM (signTransaction, submitTransaction) as QueryM
import Types.JsonWsp (OgmiosTxOut, OgmiosTxOutRef) as JsonWsp --  Perhaps it's best not to expose `JsonWsp`.
import Types.Transaction (Transaction, TransactionHash)
import Types.Transaction -- Most re-exported, don't re-export `Redeemer` and associated lens.
  ( AuxiliaryData(AuxiliaryData)
  , AuxiliaryDataHash(AuxiliaryDataHash)
  , BootstrapWitness
  , Certificate
      ( StakeRegistration
      , StakeDeregistration
      , StakeDelegation
      , PoolRegistration
      , PoolRetirement
      , GenesisKeyDelegation
      , MoveInstantaneousRewardsCert
      )
  , CostModel(CostModel)
  , Costmdls(Costmdls)
  -- , DataHash(DataHash)
  -- , DatumHash
  -- , Ed25519Signature(Ed25519Signature)
  , Epoch(Epoch)
  , ExUnitPrices
  , ExUnits
  , GeneralTransactionMetadata(GeneralTransactionMetadata)
  , GenesisHash(GenesisHash)
  , Language(PlutusV1)
  , Mint(Mint)
  , NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  , Nonce(Nonce)
  , ProposedProtocolParameterUpdates(ProposedProtocolParameterUpdates)
  , ProtocolParamUpdate
  , ProtocolVersion
  -- , PublicKey(PublicKey)
  -- , RequiredSigner(RequiredSigner)
  , ScriptDataHash(ScriptDataHash)
  , SubCoin
  , Transaction(Transaction)
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  , TransactionMetadatum
      ( MetadataMap
      , MetadataList
      , Int
      , Bytes
      , Text
      )
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  , TransactionOutput(TransactionOutput)
  , TransactionWitnessSet(TransactionWitnessSet)
  , TxBody(TxBody)
  , UnitInterval
  , Update
  -- , Utxo
  -- , UtxoM(UtxoM)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  , _auxiliaryData
  , _auxiliaryDataHash
  , _body
  , _bootstraps
  , _certs
  , _collateral
  , _fee
  , _inputs
  , _isValid
  , _mint
  , _nativeScripts
  , _networkId
  , _outputs
  , _plutusData
  , _plutusScripts
  , _requiredSigners
  , _scriptDataHash
  , _ttl
  , _update
  , _validityStartInterval
  , _vkeys
  , _withdrawals
  , _witnessSet
  ) as Transaction
import TxOutput -- Could potentially trim this down:
  ( ogmiosTxOutToScriptOutput
  , ogmiosTxOutToTransactionOutput
  , scriptOutputToOgmiosTxOut
  , scriptOutputToTransactionOutput
  , transactionInputToTxOutRef
  , transactionOutputToOgmiosTxOut
  , transactionOutputToScriptOutput
  , txOutRefToTransactionInput
  ) as TxOutput
import Types.UnbalancedTransaction (UnbalancedTx)
import Types.UnbalancedTransaction
  ( ScriptOutput(ScriptOutput) -- More up-to-date Plutus uses this, wonder if we can just use `TransactionOutput`
  , TxOutRef
  , UnbalancedTx(UnbalancedTx)
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  ) as UnbalancedTx

-- | This module defines transaction-related requests. Currently signing and
-- | submission is done with Nami.

-- | Signs a `Transaction` with potential failure.
signTransaction :: Transaction -> Contract (Maybe Transaction)
signTransaction = wrap <<< QueryM.signTransaction

-- | Submits a `Transaction` with potential failure.
submitTransaction :: Transaction -> Contract (Maybe TransactionHash)
submitTransaction = wrap <<< QueryM.submitTransaction

-- | Attempts to balance an `UnbalancedTx`.
balanceTx
  :: UnbalancedTx -> Contract (Either BalanceTxError.BalanceTxError Transaction)
balanceTx = wrap <<< BalanceTx.balanceTx

-- | Attempts to balance an `UnbalancedTx` hushing the error.
balanceTxM :: UnbalancedTx -> Contract (Maybe Transaction)
balanceTxM = map hush <<< balanceTx