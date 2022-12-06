-- | A module that defines the different transaction data types, balancing
-- | functionality, transaction fees, signing and submission.
module Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , awaitTxConfirmedWithTimeoutSlots
  , balanceTx
  , balanceTxWithConstraints
  , balanceTxs
  , balanceTxsWithConstraints
  , balanceTxM
  , calculateMinFee
  , createAdditionalUtxos
  , getTxByHash
  , getTxFinalFee
  , module BalanceTxError
  , module ExportQueryM
  , module FinalizedTransaction
  , module NativeScript
  , module OutputDatum
  , module PTransaction
  , module PTransactionUnspentOutput
  , module ReindexRedeemersExport
  , module Scripts
  , module ScriptLookups
  , module ScriptRef
  , module Transaction
  , module UnbalancedTx
  , module X
  , reindexSpentScriptRedeemers
  , signTransaction
  , submit
  , submitE
  , withBalancedTx
  , withBalancedTxWithConstraints
  , withBalancedTxs
  , withBalancedTxsWithConstraints
  ) where

import Prelude

import Aeson (class EncodeAeson, Aeson)
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedM
  , runContractInEnv
  )
import Contract.Prelude (undefined)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Reader.Class (ask)
import Ctl.Internal.BalanceTx (BalanceTxError) as BalanceTxError
import Ctl.Internal.BalanceTx (FinalizedTransaction)
import Ctl.Internal.BalanceTx
  ( FinalizedTransaction(FinalizedTransaction)
  ) as FinalizedTransaction
import Ctl.Internal.BalanceTx (balanceTxWithConstraints) as BalanceTx
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
  , scriptRefFromMintingPolicy
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
  , Ed25519Signature
  , Epoch(Epoch)
  , ExUnitPrices
  , ExUnits
  , GenesisHash(GenesisHash)
  , Mint(Mint)
  , Nonce(IdentityNonce, HashNonce)
  , PoolPubKeyHash(PoolPubKeyHash)
  , ProposedProtocolParameterUpdates(ProposedProtocolParameterUpdates)
  , ProtocolParamUpdate
  , ProtocolVersion
  , PublicKey
  , Redeemer
  , RequiredSigner(RequiredSigner)
  , ScriptDataHash(ScriptDataHash)
  , SubCoin
  , Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , TxBody(TxBody)
  , URL(URL)
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
  , _referenceInputs
  , _requiredSigners
  , _scriptDataHash
  , _ttl
  , _update
  , _validityStartInterval
  , _vkeys
  , _withdrawals
  , _witnessSet
  ) as Transaction
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction
  , TransactionOutput
  , _body
  , _outputs
  )
import Ctl.Internal.Contract.AwaitTxConfirmed
  ( awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , awaitTxConfirmedWithTimeoutSlots
  ) as Contract
import Ctl.Internal.Contract.MinFee (calculateMinFee) as Contract
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.Contract.Sign (signTransaction) as Contract
import Ctl.Internal.Hashing (transactionHash) as Hashing
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusUtxoMap
  , toPlutusCoin
  , toPlutusTxOutputWithRefScript
  )
import Ctl.Internal.Plutus.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  ) as PTransaction
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  , _input
  , _output
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
import Ctl.Internal.ReindexRedeemers
  ( ReindexErrors(CannotGetTxOutRefIndexForRedeemer)
  ) as ReindexRedeemersExport
import Ctl.Internal.ReindexRedeemers (reindexSpentScriptRedeemers) as ReindexRedeemers
import Ctl.Internal.Serialization (convertTransaction)
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  , outputDatumDataHash
  , outputDatumDatum
  ) as OutputDatum
import Ctl.Internal.Types.RewardAddress
  ( RewardAddress
  , rewardAddressFromBech32
  , rewardAddressFromBytes
  , rewardAddressToBech32
  , rewardAddressToBytes
  ) as X
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
import Ctl.Internal.Types.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  )
import Ctl.Internal.Types.UnbalancedTransaction
  ( UnbalancedTx(UnbalancedTx)
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  ) as UnbalancedTx
import Ctl.Internal.Types.UsedTxOuts
  ( UsedTxOuts
  , lockTransactionInputs
  , unlockTransactionInputs
  )
import Ctl.Internal.Types.VRFKeyHash
  ( VRFKeyHash
  , vrfKeyHashFromBytes
  , vrfKeyHashToBytes
  ) as X
import Data.Array.NonEmpty as NonEmptyArray
import Data.BigInt (BigInt)
import Data.Either (Either, hush)
import Data.Foldable (foldl, length)
import Data.Generic.Rep (class Generic)
import Data.Lens.Getter (view)
import Data.Map (empty, insert) as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds)
import Data.Traversable (class Traversable, for_, traverse)
import Data.Tuple (Tuple(Tuple), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Effect.Aff (bracket)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)

-- | Signs a transaction with potential failure.
signTransaction
  :: forall (tx :: Type)
   . Newtype tx Transaction
  => tx
  -> Contract BalancedSignedTransaction
signTransaction =
  map BalancedSignedTransaction
    <<< liftedM "Error signing the transaction"
    <<< Contract.signTransaction
    <<< unwrap

-- | Submits a `BalancedSignedTransaction`, which is the output of
-- | `signTransaction`.
submit
  :: BalancedSignedTransaction
  -> Contract TransactionHash
submit tx = do
  queryHandle <- getQueryHandle
  liftedM "Failed to submit tx" $ liftAff $ queryHandle.submitTx $ unwrap tx

-- | Like `submit` except when Ogmios sends a SubmitFail the error is returned
-- | as an Array of Aesons.
submitE
  :: BalancedSignedTransaction
  -> Contract (Either (Array Aeson) TransactionHash)
submitE = -- TODO
  undefined

-- | Calculate the minimum transaction fee.
calculateMinFee
  :: Transaction
  -> UtxoMap
  -> Contract Coin
calculateMinFee tx additionalUtxos = do
  networkId <- asks _.networkId
  let additionalUtxos' = fromPlutusUtxoMap networkId additionalUtxos
  toPlutusCoin <$> Contract.calculateMinFee tx additionalUtxos'

-- | Helper to adapt to UsedTxOuts.
withUsedTxOuts
  :: forall (a :: Type)
   . ReaderT UsedTxOuts Contract a
  -> Contract a
withUsedTxOuts f = asks _.usedTxOuts >>= runReaderT f

-- Helper to avoid repetition.
withTransactions
  :: forall (a :: Type)
       (t :: Type -> Type)
       (ubtx :: Type)
       (tx :: Type)
   . Traversable t
  => (t ubtx -> Contract (t tx))
  -> (tx -> Transaction)
  -> t ubtx
  -> (t tx -> Contract a)
  -> Contract a
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
    (withUsedTxOuts <<< unlockTransactionInputs <<< extract)

withSingleTransaction
  :: forall (a :: Type) (ubtx :: Type) (tx :: Type)
   . (ubtx -> Contract tx)
  -> (tx -> Transaction)
  -> ubtx
  -> (tx -> Contract a)
  -> Contract a
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
withBalancedTxsWithConstraints
  :: forall (a :: Type)
   . Array (UnattachedUnbalancedTx /\ BalanceTxConstraintsBuilder)
  -> (Array FinalizedTransaction -> Contract a)
  -> Contract a
withBalancedTxsWithConstraints =
  withTransactions balanceTxsWithConstraints unwrap

-- | Same as `withBalancedTxsWithConstraints`, but uses the default balancer
-- | constraints.
withBalancedTxs
  :: forall (a :: Type)
   . Array UnattachedUnbalancedTx
  -> (Array FinalizedTransaction -> Contract a)
  -> Contract a
withBalancedTxs = withTransactions balanceTxs unwrap

-- | Execute an action on a balanced transaction (`balanceTx` will
-- | be called). Within this function, all transaction inputs
-- | used by this transaction will be locked, so that they are not
-- | used in any other context.
-- | After the function completes, the locks will be removed.
-- | Errors will be thrown.
withBalancedTxWithConstraints
  :: forall (a :: Type)
   . UnattachedUnbalancedTx
  -> BalanceTxConstraintsBuilder
  -> (FinalizedTransaction -> Contract a)
  -> Contract a
withBalancedTxWithConstraints unbalancedTx =
  withSingleTransaction balanceAndLockWithConstraints unwrap
    <<< Tuple unbalancedTx

-- | Same as `withBalancedTxWithConstraints`, but uses the default balancer
-- | constraints.
withBalancedTx
  :: forall (a :: Type)
   . UnattachedUnbalancedTx
  -> (FinalizedTransaction -> Contract a)
  -> Contract a
withBalancedTx = withSingleTransaction balanceAndLock unwrap

-- | Attempts to balance an `UnattachedUnbalancedTx` using the specified
-- | balancer constraints.
balanceTxWithConstraints
  :: UnattachedUnbalancedTx
  -> BalanceTxConstraintsBuilder
  -> Contract (Either BalanceTxError.BalanceTxError FinalizedTransaction)
balanceTxWithConstraints =
  BalanceTx.balanceTxWithConstraints

-- | Same as `balanceTxWithConstraints`, but uses the default balancer
-- | constraints.
balanceTx
  :: UnattachedUnbalancedTx
  -> Contract (Either BalanceTxError.BalanceTxError FinalizedTransaction)
balanceTx = flip balanceTxWithConstraints mempty

-- | Balances each transaction using specified balancer constraint sets and
-- | locks the used inputs so that they cannot be reused by subsequent
-- | transactions.
balanceTxsWithConstraints
  :: forall (t :: Type -> Type)
   . Traversable t
  => t (UnattachedUnbalancedTx /\ BalanceTxConstraintsBuilder)
  -> Contract (t FinalizedTransaction)
balanceTxsWithConstraints unbalancedTxs =
  unlockAllOnError $ traverse balanceAndLockWithConstraints unbalancedTxs
  where
  unlockAllOnError :: forall (a :: Type). Contract a -> Contract a
  unlockAllOnError f = catchError f $ \e -> do
    for_ unbalancedTxs $
      withUsedTxOuts <<< unlockTransactionInputs <<< uutxToTx <<< fst
    throwError e

  uutxToTx :: UnattachedUnbalancedTx -> Transaction
  uutxToTx = _.transaction <<< unwrap <<< _.unbalancedTx <<< unwrap

-- | Same as `balanceTxsWithConstraints`, but uses the default balancer
-- | constraints.
balanceTxs
  :: forall (t :: Type -> Type)
   . Traversable t
  => t UnattachedUnbalancedTx
  -> Contract (t FinalizedTransaction)
balanceTxs = balanceTxsWithConstraints <<< map (flip Tuple mempty)

-- | Attempts to balance an `UnattachedUnbalancedTx` hushing the error.
balanceTxM
  :: UnattachedUnbalancedTx
  -> Contract (Maybe FinalizedTransaction)
balanceTxM = map hush <<< balanceTx

balanceAndLockWithConstraints
  :: UnattachedUnbalancedTx /\ BalanceTxConstraintsBuilder
  -> Contract FinalizedTransaction
balanceAndLockWithConstraints (unbalancedTx /\ constraints) = do
  balancedTx <-
    liftedE $ balanceTxWithConstraints unbalancedTx constraints
  void $ withUsedTxOuts $
    lockTransactionInputs (unwrap balancedTx)
  pure balancedTx

balanceAndLock
  :: UnattachedUnbalancedTx
  -> Contract FinalizedTransaction
balanceAndLock = balanceAndLockWithConstraints <<< flip Tuple mempty

-- | Reindex the `Spend` redeemers. Since we insert to an ordered array, we must
-- | reindex the redeemers with such inputs. This must be crucially called after
-- | balancing when all inputs are in place so they cannot be reordered.
reindexSpentScriptRedeemers
  :: Array Transaction.TransactionInput
  -> Array (Transaction.Redeemer /\ Maybe Transaction.TransactionInput)
  -> Either
       ReindexRedeemersExport.ReindexErrors
       (Array Transaction.Redeemer)
reindexSpentScriptRedeemers =
  ReindexRedeemers.reindexSpentScriptRedeemers

newtype BalancedSignedTransaction = BalancedSignedTransaction Transaction

derive instance Generic BalancedSignedTransaction _
derive instance Newtype BalancedSignedTransaction _
derive newtype instance Eq BalancedSignedTransaction
derive newtype instance EncodeAeson BalancedSignedTransaction

instance Show BalancedSignedTransaction where
  show = genericShow

getTxFinalFee :: BalancedSignedTransaction -> BigInt
getTxFinalFee =
  unwrap <<< view (Transaction._body <<< Transaction._fee) <<< unwrap

-- | Get `Transaction` contents by hash
getTxByHash
  :: TransactionHash
  -> Contract (Maybe Transaction)
getTxByHash th = do
  queryHandle <- getQueryHandle
  liftAff $ queryHandle.getTxByHash th

-- | Wait until a transaction with given hash is confirmed.
-- | Use `awaitTxConfirmedWithTimeout` if you want to limit the time of waiting.
-- | Will fail to confirm if the transaction includes no outputs
-- | https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
awaitTxConfirmed
  :: TransactionHash
  -> Contract Unit
awaitTxConfirmed = Contract.awaitTxConfirmed <<< unwrap

-- | Same as `awaitTxConfirmed`, but allows to specify a timeout in seconds for waiting.
-- | Throws an exception on timeout.
-- | Will fail to confirm if the transaction includes no outputs
-- | https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
awaitTxConfirmedWithTimeout
  :: Seconds
  -> TransactionHash
  -> Contract Unit
awaitTxConfirmedWithTimeout timeout =
  Contract.awaitTxConfirmedWithTimeout timeout <<< unwrap

-- | Same as `awaitTxConfirmed`, but allows to specify a timeout in slots for waiting.
-- | Throws an exception on timeout.
-- | Will fail to confirm if the transaction includes no outputs
-- | https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
awaitTxConfirmedWithTimeoutSlots
  :: Int
  -> TransactionHash
  -> Contract Unit
awaitTxConfirmedWithTimeoutSlots timeout =
  Contract.awaitTxConfirmedWithTimeoutSlots timeout <<< unwrap

-- | Builds an expected utxo set from transaction outputs. Predicts output
-- | references (`TransactionInput`s) for each output by calculating the
-- | transaction hash and indexing the outputs in the order they appear in the
-- | transaction. This function should be used for transaction chaining
-- | in conjunction with `mustUseAdditionalUtxos` balancer constraint.
-- | Throws an exception if conversion to Plutus outputs fails.
createAdditionalUtxos
  :: forall (tx :: Type)
   . Newtype tx Transaction
  => tx
  -> Contract UtxoMap
createAdditionalUtxos tx = do
  transactionId <-
    liftEffect $ Hashing.transactionHash <$> convertTransaction (unwrap tx)
  let
    txOutputs :: Array TransactionOutput
    txOutputs = view (_body <<< _outputs) (unwrap tx)

    txIn :: UInt -> TransactionInput
    txIn index = TransactionInput { transactionId, index }

  plutusOutputs <-
    liftContractM "createAdditionalUtxos: Failed to convert to Plutus outputs"
      (traverse toPlutusTxOutputWithRefScript txOutputs)

  pure $ plutusOutputs #
    foldl (\utxo txOut -> Map.insert (txIn $ length utxo) txOut utxo) Map.empty
