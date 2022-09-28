-- | A module that defines the different transaction data types, balancing
-- | functionality, transaction fees, signing and submission.
module Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , awaitTxConfirmed
  , awaitTxConfirmedWithTimeout
  , awaitTxConfirmedWithTimeoutSlots
  , balanceAndSignTx
  , balanceAndSignTxE
  , balanceAndSignTxs
  , balanceTx
  , balanceTxM
  , balanceTxWithAdditionalUtxos
  , balanceTxWithAddress
  , balanceTxWithAddressesAndAdditionalUtxos
  , balanceTxsWithAddress
  , balanceTxsWithAddressesAndAdditionalUtxos
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
import Ctl.Internal.BalanceTx (balanceTx, balanceTxWithAddress) as BalanceTx
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
import Ctl.Internal.Cardano.Types.Transaction (Transaction, UtxoMap)
import Ctl.Internal.Hashing (transactionHash) as Hashing
import Ctl.Internal.Plutus.Conversion
  ( toPlutusAddress
  , toPlutusCoin
  , toPlutusTxOutput
  )
import Ctl.Internal.Plutus.Conversion.Address (fromPlutusAddress)
import Ctl.Internal.Plutus.Types.Address (Address)
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
import Ctl.Internal.QueryM (getWalletAddresses, submitTxOgmios) as QueryM
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
import Data.Map (empty) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds)
import Data.Traversable (class Traversable, for, for_, traverse)
import Data.Tuple (fst, uncurry)
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

-- | Like `balanceTx`, but uses an additional `UtxoMap`.
balanceTxWithAdditionalUtxos
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> UtxoMap
  -> Contract r (Either BalanceTxError.BalanceTxError FinalizedTransaction)
balanceTxWithAdditionalUtxos tx utxos = do
  wrapContract $ BalanceTx.balanceTx tx utxos

-- | Attempts to balance an `UnattachedUnbalancedTx`.
balanceTx
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> Contract r (Either BalanceTxError.BalanceTxError FinalizedTransaction)
balanceTx tx = balanceTxWithAdditionalUtxos tx Map.empty

-- | Like `balanceTxWithAddress`, but uses an additional `UtxoMap`.
balanceTxWithAddressesAndAdditionalUtxos
  :: forall (r :: Row Type)
   . Array Address
  -> UnattachedUnbalancedTx
  -> UtxoMap
  -> Contract r (Either BalanceTxError.BalanceTxError FinalizedTransaction)
balanceTxWithAddressesAndAdditionalUtxos ownAddresses tx utxos = do
  networkId <- asks $ unwrap >>> _.config >>> _.networkId
  wrapContract $ BalanceTx.balanceTxWithAddress
    (fromPlutusAddress networkId <$> ownAddresses)
    tx
    utxos

-- | Attempts to balance an `UnattachedUnbalancedTx`.
balanceTxWithAddress
  :: forall (r :: Row Type)
   . Array Address
  -> UnattachedUnbalancedTx
  -> Contract r (Either BalanceTxError.BalanceTxError FinalizedTransaction)
balanceTxWithAddress ownAddresses tx =
  balanceTxWithAddressesAndAdditionalUtxos ownAddresses tx Map.empty

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

-- | Like `balanceTxsWithAddress`, but uses an additional `UtxoMap`.
balanceTxsWithAddressesAndAdditionalUtxos
  :: forall
       (t :: Type -> Type)
       (r :: Row Type)
   . Traversable t
  => Array Address
  -> t (UnattachedUnbalancedTx /\ UtxoMap)
  -> Contract r (t FinalizedTransaction)
balanceTxsWithAddressesAndAdditionalUtxos ownAddrs unbalancedTxs =
  unlockAllOnError $ traverse (uncurry balanceAndLock) unbalancedTxs
  where
  unlockAllOnError :: forall (a :: Type). Contract r a -> Contract r a
  unlockAllOnError f = catchError f $ \e -> do
    for_ (fst <$> unbalancedTxs) $
      withUsedTxouts <<< unlockTransactionInputs <<< uutxToTx
    throwError e

  uutxToTx :: UnattachedUnbalancedTx -> Transaction
  uutxToTx = _.transaction <<< unwrap <<< _.unbalancedTx <<< unwrap

  balanceAndLock
    :: UnattachedUnbalancedTx -> UtxoMap -> Contract r FinalizedTransaction
  balanceAndLock unbalancedTx utxos = do
    networkId <- asks $ unwrap >>> _.config >>> _.networkId
    balancedTx <- liftedE $ wrapContract $ BalanceTx.balanceTxWithAddress
      (fromPlutusAddress networkId <$> ownAddrs)
      unbalancedTx
      utxos
    void $ withUsedTxouts $ lockTransactionInputs (unwrap balancedTx)
    pure balancedTx

-- | Like `balanceTxs`, but uses `balanceTxWithAddress` instead of `balanceTx`
-- | internally.
balanceTxsWithAddresses
  :: forall
       (t :: Type -> Type)
       (r :: Row Type)
   . Traversable t
  => Array Address
  -> t UnattachedUnbalancedTx
  -> Contract r (t FinalizedTransaction)
balanceTxsWithAddresses ownAddrs unbalancedTxs =
  balanceTxsWithAddressesAndAdditionalUtxos ownAddrs $
    (flip (/\) $ Map.empty) <$> unbalancedTxs

-- | Like `balanceTxs`, but uses an additional `UtxoMap`.
balanceTxsWithAdditionalUtxos
  :: forall
       (t :: Type -> Type)
       (r :: Row Type)
   . Traversable t
  => t (UnattachedUnbalancedTx /\ UtxoMap)
  -> Contract r (t FinalizedTransaction)
balanceTxsWithAdditionalUtxos unbalancedTxs = do
  mbOwnAddress <- walletAddresses
  case mbOwnAddress of
    Nothing -> liftEffect $ throw $
      "Failed to get own Address"
    Just ownAddress ->
      balanceTxsWithAddressesAndAdditionalUtxos ownAddress unbalancedTxs
  where
  -- TODO: this is a helper function to get array of wallet ownAddresses
  -- should be removed when Contract's api changes to multi-address
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1045
  walletAddresses = do
    mbAddrs <- wrapContract QueryM.getWalletAddresses
    for mbAddrs \addrs ->
      for addrs
        ( liftedM "getWalletAddress: failed to deserialize address"
            <<< wrapContract
            <<< pure
            <<< toPlutusAddress
        )

balanceTxsWithAddress
  :: forall
       (t :: Type -> Type)
       (r :: Row Type)
   . Traversable t
  => Address
  -> t UnattachedUnbalancedTx
  -> Contract r (t FinalizedTransaction)
balanceTxsWithAddress ownAddr = balanceTxsWithAddresses [ ownAddr ]

-- | Balances each transaction and locks the used inputs
-- | so that they cannot be reused by subsequent transactions.
balanceTxs
  :: forall
       (t :: Type -> Type)
       (r :: Row Type)
   . Traversable t
  => t UnattachedUnbalancedTx
  -> Contract r (t FinalizedTransaction)
balanceTxs unbalancedTxs =
  balanceTxsWithAdditionalUtxos $
    (flip (/\) $ Map.empty) <$> unbalancedTxs

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

-- | Like `balanceAndSignTxs`, but uses an additional `UtxoMap`.
balanceAndSignTxsWithAdditionalUtxos
  :: forall (r :: Row Type)
   . Array (UnattachedUnbalancedTx /\ UtxoMap)
  -> Contract r (Array BalancedSignedTransaction)
balanceAndSignTxsWithAdditionalUtxos txs =
  balanceTxsWithAdditionalUtxos txs >>= traverse
    (liftedM "error signing a transaction" <<< signTransaction')

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
internalBalanceAndSignTx tx =
  internalBalanceAndSignTxWithAdditionalUtxos tx Map.empty

internalBalanceAndSignTxWithAdditionalUtxos
  :: forall (r :: Row Type)
   . UnattachedUnbalancedTx
  -> UtxoMap
  -> Contract r BalancedSignedTransaction
internalBalanceAndSignTxWithAdditionalUtxos tx utxos =
  balanceAndSignTxsWithAdditionalUtxos [ tx /\ utxos ] >>=
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
