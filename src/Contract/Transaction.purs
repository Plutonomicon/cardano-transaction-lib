-- | A module that defines the different transaction data types, balancing
-- | functionality, transaction fees, signing and submission.
module Contract.Transaction
  ( BalancedSignedTransaction(..)
  , balanceAndSignTx
  , balanceTx
  , balanceTxs
  , balanceTxM
  , calculateMinFee
  , calculateMinFeeM
  , finalizeTx
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
  , signTransactionBytes
  , submit
  ) where

import Prelude

import BalanceTx (BalanceTxError) as BalanceTxError
import BalanceTx (UnattachedTransaction)
import BalanceTx (balanceTx) as BalanceTx
import Cardano.Types.Transaction
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
import Cardano.Types.Transaction (Transaction, _body, _inputs)
import Contract.Monad (Contract, liftedE, liftedM, wrapContract)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader (asks, runReaderT)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(Left, Right), hush)
import Data.Generic.Rep (class Generic)
import Data.Lens.Getter ((^.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable
  ( class Foldable
  , class Traversable
  , fold
  , sequence
  , traverse
  )
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\), get1)
import Plutus.ToPlutusType (toPlutusType)
import Plutus.Types.Transaction (TransactionOutput(TransactionOutput)) as PTransaction
import Plutus.Types.Value (Coin)
import QueryM
  ( FinalizedTransaction(FinalizedTransaction)
  , calculateMinFee
  , signTransaction
  , signTransactionBytes
  , finalizeTx
  , submitTxOgmios
  ) as QueryM
import QueryM
  ( QueryConfig
  , FeeEstimate(FeeEstimate)
  , ClientError(..)
  , FinalizedTransaction(FinalizedTransaction)
  ) as ExportQueryM
import ReindexRedeemers (ReindexErrors(CannotGetTxOutRefIndexForRedeemer)) as ReindexRedeemersExport
import ReindexRedeemers (reindexSpentScriptRedeemers) as ReindexRedeemers
import Serialization.Address (NetworkId)
import TxOutput (scriptOutputToTransactionOutput) as TxOutput
import Types.CborBytes (CborBytes)
import Types.Datum (Datum)
import Types.ScriptLookups (MkUnbalancedTxError(..), mkUnbalancedTx) as ScriptLookups
import Types.ScriptLookups (UnattachedUnbalancedTx(UnattachedUnbalancedTx))
import Types.Transaction
  ( DataHash(DataHash)
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  ) as Transaction
import Types.Transaction (TransactionHash)
import Types.TransactionMetadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  , TransactionMetadatum(MetadataMap, MetadataList, Int, Bytes, Text)
  ) as TransactionMetadata
import Types.UnbalancedTransaction
  ( ScriptOutput(ScriptOutput)
  , UnbalancedTx(UnbalancedTx)
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  ) as UnbalancedTx
import Types.UsedTxOuts (lockTransactionInputs)

-- | This module defines transaction-related requests. Currently signing and
-- | submission is done with Nami.

-- | Signs a `Transaction` with potential failure.
signTransaction
  :: forall (r :: Row Type). Transaction -> Contract r (Maybe Transaction)
signTransaction = wrapContract <<< QueryM.signTransaction

-- | Signs a `Transaction` with potential failure
signTransactionBytes
  :: forall (r :: Row Type)
   . CborBytes
  -> Contract r (Maybe CborBytes)
signTransactionBytes = wrapContract <<< QueryM.signTransactionBytes

-- | Submits a Cbor-hex encoded transaction, which is the output of
-- | `signTransactionBytes` or `balanceAndSignTx`
submit :: forall (r :: Row Type). CborBytes -> Contract r TransactionHash
submit = wrapContract <<< map (wrap <<< unwrap) <<< QueryM.submitTxOgmios

-- | Query the Haskell server for the minimum transaction fee
calculateMinFee
  :: forall (r :: Row Type)
   . Transaction
  -> Contract r (Either ExportQueryM.ClientError Coin)
calculateMinFee = (map <<< map) (unwrap <<< toPlutusType)
  <<< wrapContract
  <<< QueryM.calculateMinFee

-- | Same as `calculateMinFee` hushing the error.
calculateMinFeeM
  :: forall (r :: Row Type). Transaction -> Contract r (Maybe Coin)
calculateMinFeeM = map hush <<< calculateMinFee

lockUtx
  :: forall (r :: Row Type) (a :: Type) (e :: Type)
   . (a -> Transaction)
  -> a
  -> Contract r a
lockUtx f tx = do
  usedTxos <- asks (_.usedTxOuts <<< unwrap)
  runReaderT (lockTransactionInputs $ f $ tx) usedTxos
  pure tx

lockMany
  :: forall (r :: Row Type) (t :: Type -> Type) (a :: Type)
   . (Traversable t)
  => (a -> Transaction)
  -> (t a)
  -> Contract r (t a)
lockMany f = traverse (lockUtx f)

balanceMany
  :: forall
       (t :: Type -> Type)
       (r :: Row Type)
   . (Traversable t)
  => t UnattachedUnbalancedTx
  -> Contract r (t (Either BalanceTxError.BalanceTxError UnattachedTransaction))
balanceMany = traverse balanceTx

balanceTxs
  :: forall
       (t :: Type -> Type)
       (r :: Row Type)
   . (Traversable t)
  => t UnattachedUnbalancedTx
  -> Contract r (Either BalanceTxError.BalanceTxError (t UnattachedTransaction))
balanceTxs uts = do
  _ <- lockMany (_.transaction <<< unwrap <<< _.unbalancedTx <<< unwrap) uts
  uts' <- balanceMany uts
  let
    uts'' :: Either BalanceTxError.BalanceTxError (t UnattachedTransaction)
    uts'' = sequence uts'
  case uts'' of
    e@(Left _) -> pure e
    Right uts''' -> do
      _ <- lockMany get1 uts'''
      pure (Right uts''')

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

finalizeTx
  :: forall (r :: Row Type)
   . Transaction.Transaction
  -> Array Datum
  -> Array Transaction.Redeemer
  -> Contract r (Maybe QueryM.FinalizedTransaction)
finalizeTx tx datums redeemers = wrapContract
  $ QueryM.finalizeTx tx datums redeemers

-- We export this because we need the redeemers as Cardano Redeemers to be used
-- in `finalizeTx`
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

newtype BalancedSignedTransaction = BalancedSignedTransaction
  { transaction :: Transaction.Transaction -- the balanced and unsigned transaction to help with logging
  , signedTxCbor :: CborBytes -- the balanced and signed cbor ByteArray representation used in `submit`
  }

derive instance Generic BalancedSignedTransaction _
derive instance Newtype BalancedSignedTransaction _
derive newtype instance Eq BalancedSignedTransaction

instance Show BalancedSignedTransaction where
  show = genericShow

signOne
  :: forall (r :: Row Type)
   . Tuple UnattachedTransaction (Array Datum)
  -> Contract r (Maybe BalancedSignedTransaction)
signOne (Tuple (balancedTx /\ redeemersTxIns) datums) = do
  let inputs = balancedTx ^. _body <<< _inputs
  redeemers <- liftedE $ reindexSpentScriptRedeemers inputs redeemersTxIns
  -- Reattach datums and redeemer:
  QueryM.FinalizedTransaction txCbor <-
    liftedM "balanceAndSignTx: Cannot attach datums and redeemer"
      (finalizeTx balancedTx datums redeemers)
  -- Sign the transaction returned as Cbor-hex encoded:
  signedTxCbor <- liftedM "balanceAndSignTx: Failed to sign transaction" $
    signTransactionBytes (wrap txCbor)
  pure $ pure $ BalancedSignedTransaction
    { transaction: balancedTx, signedTxCbor }

-- | A helper that wraps a few steps into: balance an unbalanced transaction
-- | (`balanceTx`), reindex script spend redeemers (not minting redeemers)
-- | (`reindexSpentScriptRedeemers`), attach datums and redeemers to the
-- | transaction (`finalizeTx`), and finally sign (`signTransactionBytes`).
-- | The return type includes the balanced (but unsigned) transaction for
-- | logging and more importantly, the `ByteArray` to be used with `Submit` to
-- | submit the transaction.
balanceAndSignTxs
  :: forall (r :: Row Type)
   . NonEmptyArray UnattachedUnbalancedTx
  -> Contract r (Maybe (NonEmptyArray BalancedSignedTransaction))
balanceAndSignTxs txs = do

  -- Balance unbalanced txs: 
  balancedTxs <- hush <$> balanceTxs txs
  case balancedTxs of
    Nothing -> pure Nothing
    Just txs' ->
      let
        datumss = map (_.datums <<< unwrap) txs
      in
        do
          results <-
            traverse signOne (NonEmptyArray.zip txs' datumss)
              :: Contract r (NonEmptyArray (Maybe BalancedSignedTransaction))
          pure $ sequence results

balanceAndSignTx
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r (Maybe BalancedSignedTransaction)
balanceAndSignTx tx = do
  tx' <-
    balanceAndSignTxs (NonEmptyArray.singleton tx)
      :: Contract r (Maybe (NonEmptyArray BalancedSignedTransaction))
  pure (map NonEmptyArray.head tx')

scriptOutputToTransactionOutput
  :: NetworkId
  -> UnbalancedTx.ScriptOutput
  -> Maybe PTransaction.TransactionOutput
scriptOutputToTransactionOutput networkId =
  toPlutusType <<< TxOutput.scriptOutputToTransactionOutput networkId

