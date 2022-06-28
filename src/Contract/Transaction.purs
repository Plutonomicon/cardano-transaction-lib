-- | A module that defines the different transaction data types, balancing
-- | functionality, transaction fees, signing and submission.
module Contract.Transaction
  ( BalancedSignedTransaction(..)
  , balanceAndSignTx
  , balanceTx
  , balanceTxM
  , calculateMinFee
  , calculateMinFeeM
  , module BalanceTxError
  , module ExportQueryM
  , module PTransaction
  , module ReindexRedeemersExport
  , module ScriptLookups
  , module Transaction
  , module TransactionMetadata
  , module UnbalancedTx
  , reindexSpentScriptRedeemers
  , scriptOutputToTransactionOutput
  , signTransaction
  , submit
  ) where

import Prelude

import BalanceTx (UnattachedTransaction)
import BalanceTx (balanceTx) as BalanceTx
import BalanceTx (BalanceTxError) as BalanceTxError
import Contract.Monad (Contract, liftedE, wrapContract)
import Contract.ProtocolParameters (getProtocolParameters)
import Data.Either (Either, hush)
import Data.Generic.Rep (class Generic)
import Data.Lens.Getter ((^.))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import QueryM
  ( FeeEstimate(FeeEstimate)
  , ClientError(..) -- implicit as this error list will likely increase.
  ) as ExportQueryM
import QueryM
  ( calculateMinFee
  , signTransaction
  , submitTxOgmios
  ) as QueryM
import ReindexRedeemers (reindexSpentScriptRedeemers) as ReindexRedeemers
import ReindexRedeemers
  ( ReindexErrors(CannotGetTxOutRefIndexForRedeemer)
  ) as ReindexRedeemersExport
import Types.ScriptLookups (UnattachedUnbalancedTx(UnattachedUnbalancedTx))
import Types.ScriptLookups
  ( MkUnbalancedTxError(..) -- A lot errors so will refrain from explicit names.
  , mkUnbalancedTx
  ) as ScriptLookups
import Cardano.Types.Transaction (Transaction, _body, _inputs)
import Cardano.Types.Transaction -- Most re-exported, don't re-export `Redeemer` and associated lens.
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
  , Ed25519Signature(Ed25519Signature)
  , Epoch(Epoch)
  , ExUnitPrices
  , ExUnits
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
  , Nonce(IdentityNonce, HashNonce)
  , ProposedProtocolParameterUpdates(ProposedProtocolParameterUpdates)
  , ProtocolParamUpdate
  , ProtocolVersion
  , PublicKey(PublicKey)
  , Redeemer
  , RequiredSigner(RequiredSigner)
  , ScriptDataHash(ScriptDataHash)
  , SubCoin
  , Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , TxBody(TxBody)
  , UnitInterval
  , Update
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  -- Use these lenses with care since some will involved Cardano datatypes, not
  -- Plutus ones. e.g. _fee, _collateral, _outputs. In the unlikely scenario that
  -- you need to tweak Cardano `TransactionOutput`s directly, `fromPlutusType`
  -- then `toPlutusType` should be used.
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
import Plutus.Conversion (toPlutusCoin, toPlutusTxOutput)
import Plutus.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  ) as PTransaction
import Plutus.Types.Value (Coin)
import Serialization (convertTransaction, toBytes) as Serialization
import Serialization.Address (NetworkId)
import Transaction (ReindexedUnattachedTx(ReindexedUnattachedTx), finalizeTransaction)
import TxOutput (scriptOutputToTransactionOutput) as TxOutput
import Types.Transaction (TransactionHash)
import Types.Transaction
  ( DataHash(DataHash)
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  ) as Transaction
import Types.TransactionMetadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  , TransactionMetadatum
      ( MetadataMap
      , MetadataList
      , Int
      , Bytes
      , Text
      )
  ) as TransactionMetadata
import Types.UnbalancedTransaction
  ( ScriptOutput(ScriptOutput) -- More up-to-date Plutus uses this, wonder if we can just use `TransactionOutput`
  , UnbalancedTx(UnbalancedTx)
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  ) as UnbalancedTx
import Untagged.Union (asOneOf)

-- | This module defines transaction-related requests. Currently signing and
-- | submission is done with Nami.

-- | Signs a `Transaction` with potential failure.
signTransaction
  :: forall (r :: Row Type). Transaction -> Contract r (Maybe Transaction)
signTransaction = wrapContract <<< QueryM.signTransaction

-- | Submits a `BalancedSignedTransaction`, which is the output of
-- | `signTransaction` or `balanceAndSignTx`
submit
  :: forall (r :: Row Type)
   . BalancedSignedTransaction
  -> Contract r TransactionHash
submit tx = wrapContract <<< map (wrap <<< unwrap) <<< QueryM.submitTxOgmios =<<
  liftEffect
    ( wrap <<< Serialization.toBytes <<< asOneOf <$>
        Serialization.convertTransaction (unwrap tx)
    )

-- | Query the Haskell server for the minimum transaction fee
calculateMinFee
  :: forall (r :: Row Type)
   . Transaction
  -> Contract r (Either ExportQueryM.ClientError Coin)
calculateMinFee = (map <<< map) toPlutusCoin
  <<< wrapContract
  <<< QueryM.calculateMinFee

-- | Same as `calculateMinFee` hushing the error.
calculateMinFeeM
  :: forall (r :: Row Type). Transaction -> Contract r (Maybe Coin)
calculateMinFeeM = map hush <<< calculateMinFee

-- | Attempts to balance an `UnattachedUnbalancedTx`.
balanceTx
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r (Either BalanceTxError.BalanceTxError UnattachedTransaction)
balanceTx = wrapContract <<< BalanceTx.balanceTx

-- | Attempts to balance an `UnattachedUnbalancedTx` hushing the error.
balanceTxM
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r (Maybe UnattachedTransaction)
balanceTxM = map hush <<< balanceTx

-- | Reindex the `Spend` redeemers. Since we insert to an ordered array, we must
-- | reindex the redeemers with such inputs. This must be crucially called after
-- | balancing when all inputs are in place so they cannot be reordered.
reindexSpentScriptRedeemers
  :: forall (r :: Row Type)
   . Array Transaction.TransactionInput
  -> Array (Transaction.Redeemer /\ Maybe Transaction.TransactionInput)
  -> Contract r
       ( Either
           ReindexRedeemersExport.ReindexErrors
           (Array Transaction.Redeemer)
       )
reindexSpentScriptRedeemers balancedTx =
  wrapContract <<< ReindexRedeemers.reindexSpentScriptRedeemers balancedTx

newtype BalancedSignedTransaction = BalancedSignedTransaction Transaction

derive instance Generic BalancedSignedTransaction _
derive instance Newtype BalancedSignedTransaction _
derive newtype instance Eq BalancedSignedTransaction

instance Show BalancedSignedTransaction where
  show = genericShow

-- | A helper that wraps a few steps into: balancing an unbalanced transaction
-- | (`balanceTx`), reindexing script `Spend` redeemers (not minting redeemers)
-- | (`reindexSpentScriptRedeemers`), adding the final redeemers and
-- | `ScriptDataHash`, and finally signing the tx (`signTransaction`).
-- |
-- | The return type includes the balanced (but unsigned) transaction for
-- | logging and more importantly, the `ByteArray` to be used with `submit` to
-- | submit the transaction.
balanceAndSignTx
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r (Maybe BalancedSignedTransaction)
balanceAndSignTx uaubTx = do
  rbTx <- reindexAndBalanceTx uaubTx
  costModels <- getProtocolParameters <#> unwrap >>> _.costModels
  finalizedTx <- liftedE $ liftEffect $ finalizeTransaction costModels rbTx
  map wrap <$> signTransaction finalizedTx

scriptOutputToTransactionOutput
  :: NetworkId
  -> UnbalancedTx.ScriptOutput
  -> Maybe PTransaction.TransactionOutput
scriptOutputToTransactionOutput networkId =
  toPlutusTxOutput
    <<< TxOutput.scriptOutputToTransactionOutput networkId

reindexAndBalanceTx
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r ReindexedUnattachedTx
reindexAndBalanceTx uaubTx@(UnattachedUnbalancedTx { datums }) = do
  -- Balance unbalanced tx:
  tx /\ redeemersTxIns <- liftedE $ balanceTx uaubTx
  -- Reindex its redeemers
  redeemers <- liftedE $ flip reindexSpentScriptRedeemers redeemersTxIns $
    tx ^. _body <<< _inputs
  pure $ ReindexedUnattachedTx { datums, redeemers, tx }
