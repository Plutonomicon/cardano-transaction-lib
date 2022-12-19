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
  , calculateMinFeeM
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
  , submitTxFromConstraints
  , submitTxFromConstraintsReturningFee
  , withBalancedTx
  , withBalancedTxWithConstraints
  , withBalancedTxs
  , withBalancedTxsWithConstraints
  ) where

import Prelude

import Aeson (class EncodeAeson, Aeson)
import Contract.Log (logDebug')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedM
  , runContractInEnv
  , wrapContract
  )
import Contract.PlutusData (class IsData)
import Contract.ScriptLookups (mkUnbalancedTx)
import Contract.Scripts (class ValidatorTypes)
import Contract.TxConstraints (TxConstraints)
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
import Ctl.Internal.Serialization (convertTransaction)
import Ctl.Internal.Serialization (convertTransaction, toBytes) as Serialization
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
  , ScriptLookups
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
import Data.Either (Either(Left, Right), hush)
import Data.Foldable (foldl, length)
import Data.Generic.Rep (class Generic)
import Data.Lens.Getter (view)
import Data.Map (empty, insert) as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds)
import Data.Traversable (class Traversable, for_, traverse)
import Data.Tuple (Tuple(Tuple), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Effect.Aff (bracket)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Untagged.Union (asOneOf)

-- | Signs a transaction with potential failure.
signTransaction
  :: forall (tx :: Type) (r :: Row Type)
   . Newtype tx Transaction
  => tx
  -> Contract r BalancedSignedTransaction
signTransaction =
  map BalancedSignedTransaction
    <<< liftedM "Error signing the transaction"
    <<< wrapContract
    <<< QueryM.signTransaction
    <<< unwrap

-- | Submits a `BalancedSignedTransaction`, which is the output of
-- | `signTransaction`.
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

-- | Like `submit` except when Ogmios sends a SubmitFail the error is returned
-- | as an Array of Aesons.
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

-- | Query for the minimum transaction fee.
calculateMinFee
  :: forall (r :: Row Type)
   . Transaction
  -> UtxoMap
  -> Contract r (Either ExportQueryM.ClientError Coin)
calculateMinFee tx additionalUtxos = do
  networkId <- asks $ unwrap >>> _.config >>> _.networkId
  let additionalUtxos' = fromPlutusUtxoMap networkId additionalUtxos
  map (pure <<< toPlutusCoin)
    (wrapContract $ QueryM.calculateMinFee tx additionalUtxos')

-- | Same as `calculateMinFee` hushing the error.
calculateMinFeeM
  :: forall (r :: Row Type). Transaction -> UtxoMap -> Contract r (Maybe Coin)
calculateMinFeeM tx additionalUtxos =
  map hush $ calculateMinFee tx additionalUtxos

-- | Helper to adapt to UsedTxOuts.
withUsedTxOuts
  :: forall (r :: Row Type) (a :: Type)
   . ReaderT UsedTxOuts (Contract r) a
  -> Contract r a
withUsedTxOuts f = asks (_.usedTxOuts <<< _.runtime <<< unwrap) >>= runReaderT f

-- Helper to avoid repetition.
withTransactions
  :: forall (a :: Type)
       (t :: Type -> Type)
       (r :: Row Type)
       (ubtx :: Type)
       (tx :: Type)
   . Traversable t
  => (t ubtx -> Contract r (t tx))
  -> (tx -> Transaction)
  -> t ubtx
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
    (withUsedTxOuts <<< unlockTransactionInputs <<< extract)

withSingleTransaction
  :: forall (a :: Type) (ubtx :: Type) (tx :: Type) (r :: Row Type)
   . (ubtx -> Contract r tx)
  -> (tx -> Transaction)
  -> ubtx
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
withBalancedTxsWithConstraints
  :: forall (a :: Type) (r :: Row Type)
   . Array (UnattachedUnbalancedTx /\ BalanceTxConstraintsBuilder)
  -> (Array FinalizedTransaction -> Contract r a)
  -> Contract r a
withBalancedTxsWithConstraints =
  withTransactions balanceTxsWithConstraints unwrap

-- | Same as `withBalancedTxsWithConstraints`, but uses the default balancer
-- | constraints.
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
withBalancedTxWithConstraints
  :: forall (a :: Type) (r :: Row Type)
   . UnattachedUnbalancedTx
  -> BalanceTxConstraintsBuilder
  -> (FinalizedTransaction -> Contract r a)
  -> Contract r a
withBalancedTxWithConstraints unbalancedTx =
  withSingleTransaction balanceAndLockWithConstraints unwrap
    <<< Tuple unbalancedTx

-- | Same as `withBalancedTxWithConstraints`, but uses the default balancer
-- | constraints.
withBalancedTx
  :: forall (a :: Type) (r :: Row Type)
   . UnattachedUnbalancedTx
  -> (FinalizedTransaction -> Contract r a)
  -> Contract r a
withBalancedTx = withSingleTransaction balanceAndLock unwrap

-- | Attempts to balance an `UnattachedUnbalancedTx` using the specified
-- | balancer constraints.
balanceTxWithConstraints
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> BalanceTxConstraintsBuilder
  -> Contract r (Either BalanceTxError.BalanceTxError FinalizedTransaction)
balanceTxWithConstraints unbalancedTx =
  wrapContract <<< BalanceTx.balanceTxWithConstraints unbalancedTx

-- | Same as `balanceTxWithConstraints`, but uses the default balancer
-- | constraints.
balanceTx
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r (Either BalanceTxError.BalanceTxError FinalizedTransaction)
balanceTx = flip balanceTxWithConstraints mempty

-- | Balances each transaction using specified balancer constraint sets and
-- | locks the used inputs so that they cannot be reused by subsequent
-- | transactions.
balanceTxsWithConstraints
  :: forall (r :: Row Type) (t :: Type -> Type)
   . Traversable t
  => t (UnattachedUnbalancedTx /\ BalanceTxConstraintsBuilder)
  -> Contract r (t FinalizedTransaction)
balanceTxsWithConstraints unbalancedTxs =
  unlockAllOnError $ traverse balanceAndLockWithConstraints unbalancedTxs
  where
  unlockAllOnError :: forall (a :: Type). Contract r a -> Contract r a
  unlockAllOnError f = catchError f $ \e -> do
    for_ unbalancedTxs $
      withUsedTxOuts <<< unlockTransactionInputs <<< uutxToTx <<< fst
    throwError e

  uutxToTx :: UnattachedUnbalancedTx -> Transaction
  uutxToTx = _.transaction <<< unwrap <<< _.unbalancedTx <<< unwrap

-- | Same as `balanceTxsWithConstraints`, but uses the default balancer
-- | constraints.
balanceTxs
  :: forall (r :: Row Type) (t :: Type -> Type)
   . Traversable t
  => t UnattachedUnbalancedTx
  -> Contract r (t FinalizedTransaction)
balanceTxs = balanceTxsWithConstraints <<< map (flip Tuple mempty)

-- | Attempts to balance an `UnattachedUnbalancedTx` hushing the error.
balanceTxM
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r (Maybe FinalizedTransaction)
balanceTxM = map hush <<< balanceTx

balanceAndLockWithConstraints
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx /\ BalanceTxConstraintsBuilder
  -> Contract r FinalizedTransaction
balanceAndLockWithConstraints (unbalancedTx /\ constraints) = do
  balancedTx <-
    liftedE $ balanceTxWithConstraints unbalancedTx constraints
  void $ withUsedTxOuts $
    lockTransactionInputs (unwrap balancedTx)
  pure balancedTx

balanceAndLock
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r FinalizedTransaction
balanceAndLock = balanceAndLockWithConstraints <<< flip Tuple mempty

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

getTxFinalFee :: BalancedSignedTransaction -> BigInt
getTxFinalFee =
  unwrap <<< view (Transaction._body <<< Transaction._fee) <<< unwrap

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

-- | Builds an expected utxo set from transaction outputs. Predicts output
-- | references (`TransactionInput`s) for each output by calculating the
-- | transaction hash and indexing the outputs in the order they appear in the
-- | transaction. This function should be used for transaction chaining
-- | in conjunction with `mustUseAdditionalUtxos` balancer constraint.
-- | Throws an exception if conversion to Plutus outputs fails.
createAdditionalUtxos
  :: forall (tx :: Type) (r :: Row Type)
   . Newtype tx Transaction
  => tx
  -> Contract r UtxoMap
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

submitTxFromConstraintsReturningFee
  :: forall (r :: Row Type) (validator :: Type) (datum :: Type)
       (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups.ScriptLookups validator
  -> TxConstraints redeemer datum
  -> Contract r { txHash :: TransactionHash, txFinalFee :: BigInt }
submitTxFromConstraintsReturningFee lookups constraints = do
  unbalancedTx <- liftedE $ mkUnbalancedTx lookups constraints
  balancedTx <- liftedE $ balanceTx unbalancedTx
  balancedSignedTx <- signTransaction balancedTx
  txHash <- submit balancedSignedTx
  pure { txHash, txFinalFee: getTxFinalFee balancedSignedTx }

submitTxFromConstraints
  :: forall (r :: Row Type) (validator :: Type) (datum :: Type)
       (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups.ScriptLookups validator
  -> TxConstraints redeemer datum
  -> Contract r TransactionHash
submitTxFromConstraints lookups constraints =
  _.txHash <$> submitTxFromConstraintsReturningFee lookups constraints
