-- | A module that defines the different transaction data types, balancing
-- | functionality, transaction fees, signing and submission.
module Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , awaitTxConfirmedWithTimeoutSlots
  , balanceAndSignTx
  , balanceAndSignTxs
  , balanceAndSignTxE
  , balanceTx
  , balanceTxs
  , balanceTxsWithConstraints
  , balanceTxsWithConstraints'
  , balanceTxWithConstraints
  , balanceTxM
  , calculateMinFee
  , calculateMinFeeM
  , getTxByHash
  , getTxFinalFee
  , module BalanceTxError
  , module ExportQueryM
  , module NativeScript
  , module OutputDatum
  , module PTransaction
  , module PTransactionUnspentOutput
  , module ReindexRedeemersExport
  , module Scripts
  , module ScriptLookups
  , module ScriptRef
  , module Transaction
  , module TransactionMetadata
  , module UnbalancedTx
  , reindexSpentScriptRedeemers
  , scriptOutputToTransactionOutput
  , signTransaction
  , submit
  , submitE
  , withBalancedTxs
  , withBalancedTx
  , withBalancedAndSignedTxs
  , withBalancedAndSignedTx
  ) where

import Prelude

import Aeson (class EncodeAeson, Aeson)
import Contract.Log (logDebug')
import Contract.Monad
  ( Contract
  , liftedE
  , liftedM
  , runContractInEnv
  , wrapContract
  )
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Reader.Class (ask)
import Ctl.Internal.BalanceTx (BalanceTxError) as BalanceTxError
import Ctl.Internal.BalanceTx (FinalizedTransaction)
import Ctl.Internal.BalanceTx (balanceTx, balanceTxWithConstraints) as BalanceTx
import Ctl.Internal.BalanceTx.Constraints (BalanceTxConstraintsBuilder)
import Ctl.Internal.Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  ) as NativeScript
import Ctl.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  ) as ScriptRef
import Ctl.Internal.Cardano.Types.Transaction
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
  , Mint(Mint)
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
import Ctl.Internal.Cardano.Types.Transaction (Transaction)
import Ctl.Internal.Hashing (transactionHash) as Hashing
import Ctl.Internal.Plutus.Conversion (toPlutusCoin, toPlutusTxOutput)
import Ctl.Internal.Plutus.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  ) as PTransaction
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  , lookupTxHash
  , mkTxUnspentOut
  ) as PTransactionUnspentOutput
import Ctl.Internal.Plutus.Types.Value (Coin)
import Ctl.Internal.QueryM
  ( ClientError
      ( ClientHttpError
      , ClientHttpResponseError
      , ClientDecodeJsonError
      , ClientEncodingError
      , ClientOtherError
      )
  ) as ExportQueryM
import Ctl.Internal.QueryM (submitTxOgmios) as QueryM
import Ctl.Internal.QueryM.AwaitTxConfirmed
  ( awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , awaitTxConfirmedWithTimeoutSlots
  ) as AwaitTx
import Ctl.Internal.QueryM.GetTxByHash (getTxByHash) as QueryM
import Ctl.Internal.QueryM.MinFee (calculateMinFee) as QueryM
import Ctl.Internal.QueryM.Ogmios (SubmitTxR(SubmitTxSuccess, SubmitFail))
import Ctl.Internal.QueryM.Sign (signTransaction) as QueryM
import Ctl.Internal.ReindexRedeemers
  ( ReindexErrors(CannotGetTxOutRefIndexForRedeemer)
  ) as ReindexRedeemersExport
import Ctl.Internal.ReindexRedeemers (reindexSpentScriptRedeemers) as ReindexRedeemers
import Ctl.Internal.Serialization (convertTransaction, toBytes) as Serialization
import Ctl.Internal.Serialization.Address (NetworkId)
import Ctl.Internal.TxOutput (scriptOutputToTransactionOutput) as TxOutput
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  , outputDatumDataHash
  , outputDatumDatum
  ) as OutputDatum
import Ctl.Internal.Types.ScriptLookups
  ( MkUnbalancedTxError
      ( TypeCheckFailed
      , ModifyTx
      , TxOutRefNotFound
      , TxOutRefWrongType
      , DatumNotFound
      , MintingPolicyNotFound
      , MintingPolicyHashNotCurrencySymbol
      , CannotMakeValue
      , ValidatorHashNotFound
      , OwnPubKeyAndStakeKeyMissing
      , TypedValidatorMissing
      , DatumWrongHash
      , CannotQueryDatum
      , CannotHashDatum
      , CannotConvertPOSIXTimeRange
      , CannotGetMintingPolicyScriptIndex
      , CannotGetValidatorHashFromAddress
      , MkTypedTxOutFailed
      , TypedTxOutHasNoDatumHash
      , CannotHashMintingPolicy
      , CannotHashValidator
      , CannotConvertPaymentPubKeyHash
      , CannotSatisfyAny
      )
  , mkUnbalancedTx
  ) as ScriptLookups
import Ctl.Internal.Types.ScriptLookups (UnattachedUnbalancedTx)
import Ctl.Internal.Types.Scripts
  ( Language(PlutusV1, PlutusV2)
  , plutusV1Script
  , plutusV2Script
  ) as Scripts
import Ctl.Internal.Types.Transaction
  ( DataHash(DataHash)
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  ) as Transaction
import Ctl.Internal.Types.Transaction (TransactionHash)
import Ctl.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  , TransactionMetadatum(MetadataMap, MetadataList, Int, Bytes, Text)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  ) as TransactionMetadata
import Ctl.Internal.Types.UnbalancedTransaction
  ( ScriptOutput(ScriptOutput)
  , UnbalancedTx(UnbalancedTx)
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  ) as UnbalancedTx
import Ctl.Internal.Types.UsedTxOuts
  ( UsedTxOuts
  , lockTransactionInputs
  , unlockTransactionInputs
  )
import Data.Array.NonEmpty as NonEmptyArray
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), hush)
import Data.Generic.Rep (class Generic)
import Data.Lens.Getter (view)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds)
import Data.Traversable (class Traversable, for_, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (bracket)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throw)
import Prim.TypeError (class Warn, Text)
import Untagged.Union (asOneOf)

-- | This module defines transaction-related requests. Currently signing and
-- | submission is done with Nami.

-- | Signs a `Transaction` with potential failure.
signTransaction
  :: forall (r :: Row Type). Transaction -> Contract r (Maybe Transaction)
signTransaction = wrapContract <<< QueryM.signTransaction

-- | Signs a `FinalizedTransaction` with potential failure.
signTransaction'
  :: forall (r :: Row Type)
   . FinalizedTransaction
  -> Contract r (Maybe BalancedSignedTransaction)
signTransaction' =
  map (map BalancedSignedTransaction) <<< signTransaction <<< unwrap

-- | Submits a `BalancedSignedTransaction`, which is the output of
-- | `signTransaction` or `balanceAndSignTx`
submit
  :: forall (r :: Row Type)
   . BalancedSignedTransaction
  -> Contract r TransactionHash
submit tx = do
  result <- submitE tx
  case result of
    Right th -> pure th
    Left json -> liftEffect $ throw $
      "`submit` call failed. Error from Ogmios: " <> show json

-- | Like submit except when ogmios sends a SubmitFail
-- | the error is returned as an Array of Aesons
submitE
  :: forall (r :: Row Type)
   . BalancedSignedTransaction
  -> Contract r (Either (Array Aeson) TransactionHash)
submitE tx = do
  cslTx <- liftEffect $ Serialization.convertTransaction (unwrap tx)
  let txHash = Hashing.transactionHash cslTx
  logDebug' $ "Pre-calculated tx hash: " <> show txHash
  let txCborBytes = wrap $ Serialization.toBytes $ asOneOf cslTx
  result <- wrapContract $
    QueryM.submitTxOgmios (unwrap txHash) txCborBytes
  pure $ case result of
    SubmitTxSuccess th -> Right $ wrap th
    SubmitFail json -> Left json

-- | Query the Haskell server for the minimum transaction fee
calculateMinFee
  :: forall (r :: Row Type)
   . Transaction
  -> Contract r (Either ExportQueryM.ClientError Coin)
calculateMinFee = map (pure <<< toPlutusCoin)
  <<< wrapContract
  <<< QueryM.calculateMinFee

-- | Same as `calculateMinFee` hushing the error.
calculateMinFeeM
  :: forall (r :: Row Type). Transaction -> Contract r (Maybe Coin)
calculateMinFeeM = map hush <<< calculateMinFee

-- | Helper to adapt to UsedTxOuts
withUsedTxouts
  :: forall (r :: Row Type) (a :: Type)
   . ReaderT UsedTxOuts (Contract r) a
  -> Contract r a
withUsedTxouts f = asks (_.usedTxOuts <<< _.runtime <<< unwrap) >>= runReaderT f

-- | Attempts to balance an `UnattachedUnbalancedTx` using the default 
-- | constraints.
balanceTx
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r (Either BalanceTxError.BalanceTxError FinalizedTransaction)
balanceTx = wrapContract <<< BalanceTx.balanceTx

-- | Like `balanceTx`, but allows to specify constraints to be considered when
-- | balancing a transaction.
balanceTxWithConstraints
  :: forall (r :: Row Type)
   . BalanceTxConstraintsBuilder
  -> UnattachedUnbalancedTx
  -> Contract r (Either BalanceTxError.BalanceTxError FinalizedTransaction)
balanceTxWithConstraints constraints =
  wrapContract <<< BalanceTx.balanceTxWithConstraints constraints

-- Helper to avoid repetition
withTransactions
  :: forall (a :: Type)
       (t :: Type -> Type)
       (r :: Row Type)
       (tx :: Type)
   . Traversable t
  => (t UnattachedUnbalancedTx -> Contract r (t tx))
  -> (tx -> Transaction)
  -> t UnattachedUnbalancedTx
  -> (t tx -> Contract r a)
  -> Contract r a
withTransactions prepare extract utxs action = do
  env <- ask
  let
    run :: forall (b :: Type). _ b -> _ b
    run = runContractInEnv env
  liftAff $ bracket
    (run (prepare utxs))
    (run <<< cleanup)
    (run <<< action)
  where
  cleanup txs = for_ txs
    (withUsedTxouts <<< unlockTransactionInputs <<< extract)

withSingleTransaction
  :: forall (a :: Type) (tx :: Type) (r :: Row Type)
   . (UnattachedUnbalancedTx -> Contract r tx)
  -> (tx -> Transaction)
  -> UnattachedUnbalancedTx
  -> (tx -> Contract r a)
  -> Contract r a
withSingleTransaction prepare extract utx action =
  withTransactions (traverse prepare) extract (NonEmptyArray.singleton utx)
    (action <<< NonEmptyArray.head)

-- | Execute an action on an array of balanced
-- | transactions (`balanceTxs` will be called). Within
-- | this function, all transaction inputs used by these
-- | transactions will be locked, so that they are not used
-- | in any other context.
-- | After the function completes, the locks will be removed.
-- | Errors will be thrown.
withBalancedTxs
  :: forall (a :: Type) (r :: Row Type)
   . Array UnattachedUnbalancedTx
  -> (Array FinalizedTransaction -> Contract r a)
  -> Contract r a
withBalancedTxs = withTransactions balanceTxs unwrap

-- | Execute an action on a balanced transaction (`balanceTx` will
-- | be called). Within this function, all transaction inputs
-- | used by this transaction will be locked, so that they are not
-- | used in any other context.
-- | After the function completes, the locks will be removed.
-- | Errors will be thrown.
withBalancedTx
  :: forall (a :: Type) (r :: Row Type)
   . UnattachedUnbalancedTx
  -> (FinalizedTransaction -> Contract r a)
  -> Contract r a
withBalancedTx = withSingleTransaction (liftedE <<< balanceTx) unwrap

-- | Execute an action on an array of balanced and signed
-- | transactions (`balanceAndSignTxs` will be called). Within
-- | this function, all transaction inputs used by these
-- | transactions will be locked, so that they are not used
-- | in any other context.
-- | After the function completes, the locks will be removed.
-- | Errors will be thrown.
withBalancedAndSignedTxs
  :: forall (r :: Row Type) (a :: Type)
   . Array UnattachedUnbalancedTx
  -> (Array BalancedSignedTransaction -> Contract r a)
  -> Contract r a
withBalancedAndSignedTxs = withTransactions balanceAndSignTxs unwrap

-- | Execute an action on a balanced and signed transaction.
-- | (`balanceAndSignTx` will be called). Within this function,
-- | all transaction inputs used by this transaction will be
-- | locked, so that they are not used in any other context.
-- | After the function completes, the locks will be removed.
-- | Errors will be thrown.
withBalancedAndSignedTx
  :: forall (a :: Type) (r :: Row Type)
   . UnattachedUnbalancedTx
  -> (BalancedSignedTransaction -> Contract r a)
  -> Contract r a
withBalancedAndSignedTx = withSingleTransaction
  internalBalanceAndSignTx
  unwrap

-- | Like `balanceTxs`, but allows to specify separate sets of balancing 
-- | constraints for each transaction.
balanceTxsWithConstraints'
  :: forall (r :: Row Type) (t :: Type -> Type)
   . Traversable t
  => t (UnattachedUnbalancedTx /\ BalanceTxConstraintsBuilder)
  -> Contract r (t FinalizedTransaction)
balanceTxsWithConstraints' unbalancedTxs =
  unlockAllOnError $ traverse balanceAndLock unbalancedTxs
  where
  unlockAllOnError :: forall (a :: Type). Contract r a -> Contract r a
  unlockAllOnError f = catchError f $ \e -> do
    for_ unbalancedTxs $
      withUsedTxouts <<< unlockTransactionInputs <<< uutxToTx <<< fst
    throwError e

  uutxToTx :: UnattachedUnbalancedTx -> Transaction
  uutxToTx = _.transaction <<< unwrap <<< _.unbalancedTx <<< unwrap

  balanceAndLock
    :: UnattachedUnbalancedTx /\ BalanceTxConstraintsBuilder
    -> Contract r FinalizedTransaction
  balanceAndLock (unbalancedTx /\ constraints) = do
    balancedTx <-
      liftedE $ wrapContract $
        BalanceTx.balanceTxWithConstraints constraints unbalancedTx
    void $ withUsedTxouts $ lockTransactionInputs (unwrap balancedTx)
    pure balancedTx

-- | Like `balanceTxs`, but allows to specify a single set of balancing 
-- | constraints for all transactions.
balanceTxsWithConstraints
  :: forall (r :: Row Type) (t :: Type -> Type)
   . Traversable t
  => BalanceTxConstraintsBuilder
  -> t UnattachedUnbalancedTx
  -> Contract r (t FinalizedTransaction)
balanceTxsWithConstraints constraints =
  balanceTxsWithConstraints' <<< map (_ /\ constraints)

-- | Balances each transaction using the default constraints and locks the used 
-- | inputs so that they cannot be reused by subsequent transactions.
balanceTxs
  :: forall (r :: Row Type) (t :: Type -> Type)
   . Traversable t
  => t UnattachedUnbalancedTx
  -> Contract r (t FinalizedTransaction)
balanceTxs = balanceTxsWithConstraints mempty

-- | Attempts to balance an `UnattachedUnbalancedTx` hushing the error.
balanceTxM
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r (Maybe FinalizedTransaction)
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
derive newtype instance EncodeAeson BalancedSignedTransaction

instance Show BalancedSignedTransaction where
  show = genericShow

-- | Like `balanceAndSignTx`, but for more than one transaction.
-- | This function may throw errors through the contract Monad.
-- | If successful, transaction inputs will be locked afterwards.
-- | If you want to re-use them in the same 'QueryM' context, call
-- | `unlockTransactionInputs`.
balanceAndSignTxs
  :: forall (r :: Row Type)
   . Array UnattachedUnbalancedTx
  -> Contract r (Array BalancedSignedTransaction)
balanceAndSignTxs txs = balanceTxs txs >>= traverse
  (liftedM "error signing a transaction" <<< signTransaction')

-- | Balances an unbalanced transaction and signs it.
-- |
-- | The return type includes the balanced transaction to be used with `submit`
-- | to submit the transaction.
-- | If successful, transaction inputs will be locked afterwards.
-- | If you want to re-use them in the same 'QueryM' context, call
-- | `unlockTransactionInputs`.
balanceAndSignTx
  :: forall (r :: Row Type)
   . Warn
       ( Text
           "`balanceAndSignTx` no longer returns `Nothing` when failing, instead letting errors continue through the `Contract` monad. `Maybe` will be removed in a future release."
       )
  => UnattachedUnbalancedTx
  -> Contract r (Maybe BalancedSignedTransaction)
balanceAndSignTx tx = pure <$> internalBalanceAndSignTx tx

internalBalanceAndSignTx
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r BalancedSignedTransaction
internalBalanceAndSignTx tx = balanceAndSignTxs [ tx ] >>=
  case _ of
    [ x ] -> pure x
    _ -> liftEffect $ throw $
      "Unexpected internal error during transaction signing"

-- TODO Deprecate `balanceAndSignTxE` once `Maybe` is dropped from
-- `balanceAndSignTx`, like in `internalBalanceAndSignTx`.
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/880
-- | Like `balanceAndSignTx`, but does not throw errors, and which are instead
-- | held in `Left`.
-- | If successful, transaction inputs will be locked afterwards.
-- | If you want to re-use them in the same 'QueryM' context, call
-- | `unlockTransactionInputs`.
balanceAndSignTxE
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r (Either Error BalancedSignedTransaction)
balanceAndSignTxE = try <<< internalBalanceAndSignTx

getTxFinalFee :: BalancedSignedTransaction -> BigInt
getTxFinalFee =
  unwrap <<< view (Transaction._body <<< Transaction._fee) <<< unwrap

scriptOutputToTransactionOutput
  :: NetworkId
  -> UnbalancedTx.ScriptOutput
  -> Maybe PTransaction.TransactionOutput
scriptOutputToTransactionOutput networkId =
  toPlutusTxOutput
    <<< TxOutput.scriptOutputToTransactionOutput networkId

-- | Get `Transaction` contents by hash
getTxByHash
  :: forall (r :: Row Type)
   . TransactionHash
  -> Contract r (Maybe Transaction)
getTxByHash = wrapContract <<< QueryM.getTxByHash <<< unwrap

-- | Wait until a transaction with given hash is confirmed.
-- | Use `awaitTxConfirmedWithTimeout` if you want to limit the time of waiting.
awaitTxConfirmed
  :: forall (r :: Row Type)
   . TransactionHash
  -> Contract r Unit
awaitTxConfirmed = wrapContract <<< AwaitTx.awaitTxConfirmed <<< unwrap

-- | Same as `awaitTxConfirmed`, but allows to specify a timeout in seconds for waiting.
-- | Throws an exception on timeout.
awaitTxConfirmedWithTimeout
  :: forall (r :: Row Type)
   . Seconds
  -> TransactionHash
  -> Contract r Unit
awaitTxConfirmedWithTimeout timeout = wrapContract
  <<< AwaitTx.awaitTxConfirmedWithTimeout timeout
  <<< unwrap

-- | Same as `awaitTxConfirmed`, but allows to specify a timeout in slots for waiting.
-- | Throws an exception on timeout.
awaitTxConfirmedWithTimeoutSlots
  :: forall (r :: Row Type)
   . Int
  -> TransactionHash
  -> Contract r Unit
awaitTxConfirmedWithTimeoutSlots timeout = wrapContract
  <<< AwaitTx.awaitTxConfirmedWithTimeoutSlots timeout
  <<< unwrap
