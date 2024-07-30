-- | A module that defines the different transaction data types, balancing
-- | functionality, transaction fees, signing and submission.
module Contract.Transaction
  ( balanceTx
  , balanceTxE
  , balanceTxs
  , createAdditionalUtxos
  , getTxAuxiliaryData
  , module BalanceTxError
  , module X
  , submit
  , submitE
  , submitTxFromConstraints
  , withBalancedTx
  , withBalancedTxs
  , lookupTxHash
  , mkPoolPubKeyHash
  , hashTransaction
  , buildTx
  , submitTxFromBuildPlan
  ) where

import Prelude

import Cardano.Transaction.Builder
  ( TransactionBuilderStep
  , buildTransaction
  , explainTxBuildError
  )
import Cardano.Types
  ( Bech32String
  , PoolPubKeyHash(PoolPubKeyHash)
  , Transaction(Transaction)
  , TransactionHash
  , TransactionInput(TransactionInput)
  , TransactionOutput
  , TransactionUnspentOutput(TransactionUnspentOutput)
  , UtxoMap
  , _body
  , _outputs
  )
import Cardano.Types
  ( DataHash(DataHash)
  , Epoch(Epoch)
  , NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TransactionUnspentOutput(TransactionUnspentOutput)
  ) as X
import Cardano.Types.AuxiliaryData (AuxiliaryData)
import Cardano.Types.Ed25519KeyHash as Ed25519KeyHash
import Cardano.Types.OutputDatum (OutputDatum(OutputDatum, OutputDatumHash)) as X
import Cardano.Types.PoolPubKeyHash (PoolPubKeyHash(PoolPubKeyHash)) as X
import Cardano.Types.ScriptRef (ScriptRef(NativeScriptRef, PlutusScriptRef)) as X
import Cardano.Types.Transaction (Transaction(Transaction), empty) as X
import Cardano.Types.Transaction as Transaction
import Contract.Log (logTrace')
import Contract.Monad (Contract, runContractInEnv)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Control.Monad.Error.Class (catchError, liftEither, throwError)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Reader.Class (ask)
import Ctl.Internal.BalanceTx as B
import Ctl.Internal.BalanceTx.Constraints (BalancerConstraints)
import Ctl.Internal.BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError
      ( BalanceInsufficientError
      , CouldNotConvertScriptOutputToTxInput
      , CouldNotGetCollateral
      , InsufficientCollateralUtxos
      , CouldNotGetUtxos
      , CollateralReturnError
      , CollateralReturnMinAdaValueCalcError
      , ExUnitsEvaluationFailed
      , InsufficientUtxoBalanceToCoverAsset
      , ReindexRedeemersError
      , UtxoLookupFailedFor
      , UtxoMinAdaValueCalculationFailed
      )
  , Expected(Expected)
  , explainBalanceTxError
  ) as BalanceTxError
import Ctl.Internal.Contract.AwaitTxConfirmed
  ( awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , awaitTxConfirmedWithTimeoutSlots
  , isTxConfirmed
  ) as X
import Ctl.Internal.Contract.MinFee (calculateMinFee) as X
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Contract.QueryHandle.Error (GetTxMetadataError)
import Ctl.Internal.Contract.QueryHandle.Error
  ( GetTxMetadataError
      ( GetTxMetadataTxNotFoundError
      , GetTxMetadataMetadataEmptyOrMissingError
      , GetTxMetadataClientError
      )
  ) as X
import Ctl.Internal.Contract.Sign (signTransaction)
import Ctl.Internal.Contract.Sign (signTransaction) as X
import Ctl.Internal.Service.Error (ClientError)
import Ctl.Internal.Types.ScriptLookups (ScriptLookups)
import Ctl.Internal.Types.TxConstraints (TxConstraints)
import Ctl.Internal.Types.UsedTxOuts
  ( UsedTxOuts
  , lockTransactionInputs
  , unlockTransactionInputs
  )
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.Foldable (foldl, length)
import Data.Lens.Getter (view)
import Data.Map (empty, insert, toUnfoldable) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap)
import Data.String.Utils (startsWith)
import Data.Traversable (class Traversable, for_, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.UInt (UInt)
import Effect.Aff (bracket, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (try)
import Prim.Coerce (class Coercible)
import Prim.TypeError (class Warn, Text)
import Safe.Coerce (coerce)

buildTx
  :: Array TransactionBuilderStep
  -> Contract Transaction
buildTx steps = do
  case buildTransaction steps of
    Left err -> do
      throwError (error $ explainTxBuildError err)
    Right res -> pure res

hashTransaction
  :: Warn
       (Text "Deprecated: hashTransaction. Use Cardano.Types.Transaction.hash")
  => Transaction
  -> TransactionHash
hashTransaction = Transaction.hash

-- | Submits a `Transaction`, which is the output of
-- | `signTransaction`.
submit
  :: Transaction
  -> Contract TransactionHash
submit tx = do
  logTrace' $ "Submitting transaction: " <> show tx
  eiTxHash <- submitE tx
  liftEither $ flip lmap eiTxHash \err -> error $
    "Failed to submit tx:\n" <> show err

-- | Submits a `Transaction` that normally should be retreived from
-- | `signTransaction`. Preserves the errors returned by the backend in
-- | the case they need to be inspected.
submitE
  :: Transaction
  -> Contract (Either ClientError TransactionHash)
submitE tx = do
  queryHandle <- getQueryHandle
  eiTxHash <- liftAff $ queryHandle.submitTx tx
  void $ asks (_.hooks >>> _.onSubmit) >>=
    traverse \hook -> liftEffect $ void $ try $ hook tx
  pure eiTxHash

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
withBalancedTxs
  :: forall (a :: Type)
   . Array
       { transaction :: Transaction
       , usedUtxos :: UtxoMap
       , balancerConstraints :: BalancerConstraints
       }
  -> (Array Transaction -> Contract a)
  -> Contract a
withBalancedTxs = withTransactions balanceTxs identity

-- | Execute an action on a balanced transaction (`balanceTx` will
-- | be called). Within this function, all transaction inputs
-- | used by this transaction will be locked, so that they are not
-- | used in any other context.
-- | After the function completes, the locks will be removed.
-- | Errors will be thrown.
withBalancedTx
  :: forall (a :: Type)
   . Transaction
  -> UtxoMap
  -> BalancerConstraints
  -> (Transaction -> Contract a)
  -> Contract a
withBalancedTx tx usedUtxos balancerConstraints =
  withSingleTransaction
    ( \transaction -> balanceAndLock
        { transaction, usedUtxos, balancerConstraints }
    )
    identity
    tx

-- | A variant of `balanceTx` that returns a balancer error value.
balanceTxE
  :: Transaction
  -> UtxoMap
  -> BalancerConstraints
  -> Contract (Either BalanceTxError.BalanceTxError Transaction)
balanceTxE tx utxos = B.balanceTxWithConstraints tx utxos

-- | Balance a single transaction.
-- |
-- | `UtxoMap` is a collection of UTxOs used as inputs that are coming from outside of the user wallet.
-- |
-- | `balanceTxE` is a non-throwing version of this function.
-- |
-- | Use `balanceTxs` to balance multiple transactions and prevent them from
-- | using the same input UTxOs.
balanceTx
  :: Transaction
  -> UtxoMap
  -> BalancerConstraints
  -> Contract Transaction
balanceTx utx utxos constraints = do
  result <- balanceTxE utx utxos constraints
  case result of
    Left err -> throwError $ error $ BalanceTxError.explainBalanceTxError err
    Right ftx -> pure ftx

-- | Balances each transaction using specified balancer constraint sets and
-- | locks the used inputs so that they cannot be reused by subsequent
-- | transactions.
balanceTxs
  :: Array
       { transaction :: Transaction
       , usedUtxos :: UtxoMap
       , balancerConstraints :: BalancerConstraints
       }
  -> Contract (Array Transaction)
balanceTxs unbalancedTxs =
  unlockAllOnError $ traverse balanceAndLock unbalancedTxs
  where
  unlockAllOnError :: forall (a :: Type). Contract a -> Contract a
  unlockAllOnError f = catchError f $ \e -> do
    for_ unbalancedTxs $
      withUsedTxOuts <<< unlockTransactionInputs <<< _.transaction
    throwError e

balanceAndLock
  :: { transaction :: Transaction
     , usedUtxos :: UtxoMap
     , balancerConstraints :: BalancerConstraints
     }
  -> Contract Transaction
balanceAndLock { transaction, usedUtxos, balancerConstraints } = do
  balancedTx <- balanceTx transaction usedUtxos balancerConstraints
  void $ withUsedTxOuts $ lockTransactionInputs balancedTx
  pure balancedTx

-- | Fetch transaction auxiliary data.
-- | Returns `Right` when the transaction exists and auxiliary data is not empty
getTxAuxiliaryData
  :: TransactionHash
  -> Contract (Either GetTxMetadataError AuxiliaryData)
getTxAuxiliaryData txHash = do
  queryHandle <- getQueryHandle
  liftAff $ queryHandle.getTxAuxiliaryData txHash

-- | Builds an expected utxo set from transaction outputs. Predicts output
-- | references (`TransactionInput`s) for each output by calculating the
-- | transaction hash and indexing the outputs in the order they appear in the
-- | transaction. This function should be used for transaction chaining
-- | in conjunction with `mustUseAdditionalUtxos` balancer constraint.
createAdditionalUtxos
  :: forall (tx :: Type)
   . Coercible tx Transaction
  => tx
  -> Contract UtxoMap
createAdditionalUtxos tx = do
  let transactionId = Transaction.hash $ coerce tx
  let
    txOutputs :: Array TransactionOutput
    txOutputs = view (_body <<< _outputs) $ coerce tx

    txIn :: UInt -> TransactionInput
    txIn index = TransactionInput { transactionId, index }

  pure $ txOutputs #
    foldl (\utxo txOut -> Map.insert (txIn $ length utxo) txOut utxo) Map.empty

submitTxFromConstraints
  :: ScriptLookups
  -> TxConstraints
  -> Contract TransactionHash
submitTxFromConstraints lookups constraints = do
  unbalancedTx /\ usedUtxos <- mkUnbalancedTx lookups constraints
  balancedTx <- balanceTx unbalancedTx usedUtxos mempty
  balancedSignedTx <- signTransaction balancedTx
  submit balancedSignedTx

submitTxFromBuildPlan
  :: UtxoMap
  -> BalancerConstraints
  -> Array TransactionBuilderStep
  -> Contract Transaction
submitTxFromBuildPlan usedUtxos balancerConstraints plan = do
  unbalancedTx <- buildTx plan
  balancedTx <- balanceTx unbalancedTx usedUtxos balancerConstraints
  balancedSignedTx <- signTransaction balancedTx
  void $ submit balancedSignedTx
  pure balancedSignedTx

lookupTxHash
  :: TransactionHash -> UtxoMap -> Array TransactionUnspentOutput
lookupTxHash txHash utxos =
  map (\(input /\ output) -> TransactionUnspentOutput { input, output })
    $ Array.filter (fst >>> unwrap >>> _.transactionId >>> eq txHash)
    $ Map.toUnfoldable utxos

mkPoolPubKeyHash :: Bech32String -> Maybe PoolPubKeyHash
mkPoolPubKeyHash str
  | startsWith "pool" str = PoolPubKeyHash <$>
      Ed25519KeyHash.fromBech32 str
  | otherwise = Nothing
