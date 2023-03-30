-- | A module that defines the different transaction data types, balancing
-- | functionality, transaction fees, signing and submission.
module Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceTx
  , balanceTxE
  , balanceTxs
  , balanceTxsE
  , calculateMinFee
  , createAdditionalUtxos
  , getTxFinalFee
  , getTxMetadata
  , module BalanceTxError
  , module NativeScript
  , module OutputDatum
  , module PTransaction
  , module PTransactionUnspentOutput
  , module ScriptRef
  , module Scripts
  , module Transaction
  , module X
  , signTransaction
  , submit
  , submitE
  , submitTxFromConstraints
  , submitTxFromConstraintsReturningFee
  ) where

import Prelude

import Aeson (class EncodeAeson)
import Contract.ClientError (ClientError)
import Contract.Metadata (GeneralTransactionMetadata)
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (class IsData)
import Contract.ScriptLookups (mkUnbalancedTx)
import Contract.Scripts (class ValidatorTypes)
import Contract.TxConstraints (TxConstraints)
import Control.Monad.Error.Class (catchError, liftEither, throwError, try)
import Control.Monad.Reader (asks)
import Ctl.Internal.BalanceTx (FinalizedTransaction, balanceTxWithConstraints)
import Ctl.Internal.BalanceTx (FinalizedTransaction(FinalizedTransaction)) as X
import Ctl.Internal.BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError
      ( BalanceInsufficientError
      , CouldNotConvertScriptOutputToTxInput
      , CouldNotGetChangeAddress
      , CouldNotGetCollateral
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
  , ImpossibleError(Impossible)
  , InvalidInContext(InvalidInContext)
  , printTxEvaluationFailure
  ) as BalanceTxError
import Ctl.Internal.BalanceTx.Error (BalanceTxError)
import Ctl.Internal.BalanceTx.UnattachedTx (UnindexedTx)
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
  , mkPoolPubKeyHash
  , poolPubKeyHashToBech32
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
  , isTxConfirmed
  ) as X
import Ctl.Internal.Contract.MinFee (calculateMinFee) as Contract
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Contract.QueryHandle.Error (GetTxMetadataError)
import Ctl.Internal.Contract.QueryHandle.Error
  ( GetTxMetadataError
      ( GetTxMetadataTxNotFoundError
      , GetTxMetadataMetadataEmptyOrMissingError
      , GetTxMetadataClientError
      )
  ) as X
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
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx(UnbalancedTx))
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
import Ctl.Internal.Types.ScriptLookups (ScriptLookups)
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
import Ctl.Internal.Types.VRFKeyHash
  ( VRFKeyHash
  , vrfKeyHashFromBytes
  , vrfKeyHashToBytes
  ) as X
import Ctl.Internal.UsedTxOuts (lockTransactionInputs, unlockTransactionInputs)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (foldl, length)
import Data.Generic.Rep (class Generic)
import Data.Lens.Getter (view)
import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, for_, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Effect.Aff (error)
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
  eiTxHash <- submitE tx
  liftEither $ flip lmap eiTxHash \err -> error $
    "Failed to submit tx:\n" <> show err

-- | Submits a `BalancedSignedTransaction`, which is the output of
-- | `signTransaction`. Preserves the errors returned by the backend in
-- | the case they need to be inspected.
submitE
  :: BalancedSignedTransaction
  -> Contract (Either ClientError TransactionHash)
submitE tx = do
  queryHandle <- getQueryHandle
  eiTxHash <- liftAff $ queryHandle.submitTx $ unwrap tx
  void $ asks (_.hooks >>> _.onSubmit) >>=
    traverse \hook -> liftEffect $ void $ try $ hook $ unwrap tx
  pure eiTxHash

-- | Calculate the minimum transaction fee.
calculateMinFee
  :: Transaction
  -> UtxoMap
  -> Contract Coin
calculateMinFee tx additionalUtxos = do
  networkId <- asks _.networkId
  let additionalUtxos' = fromPlutusUtxoMap networkId additionalUtxos
  toPlutusCoin <$> Contract.calculateMinFee tx additionalUtxos'

unUnbalancedTx
  :: UnbalancedTx -> UnindexedTx /\ Map TransactionInput TransactionOutput
unUnbalancedTx
  ( UnbalancedTx
      { transaction
      , datums
      , redeemers
      , usedUtxos
      }
  ) =
  { transaction, datums, redeemers } /\ usedUtxos

balanceTx
  :: UnbalancedTx
  -> Contract FinalizedTransaction
balanceTx tx = balanceTxs (NonEmptyArray.singleton tx) <#>
  NonEmptyArray.head

-- | Attempts to balance an `UnbalancedTx` using the specified
-- | balancer constraints.
balanceTxE
  :: UnbalancedTx
  -> Contract (Either BalanceTxError FinalizedTransaction)
balanceTxE tx = balanceTxsE (NonEmptyArray.singleton tx) <#>
  NonEmptyArray.head

-- | Balances each transaction using specified balancer constraint sets and
-- | locks the used inputs so that they cannot be reused by subsequent
-- | transactions.
balanceTxs
  :: forall (t :: Type -> Type)
   . Traversable t
  => t UnbalancedTx
  -> Contract (t FinalizedTransaction)
balanceTxs txs = balanceTxsE txs >>= traverse (liftedE <<< pure)

-- | Balances each transaction using specified balancer constraint sets and
-- | locks the used inputs so that they cannot be reused by subsequent
-- | transactions.
balanceTxsE
  :: forall (t :: Type -> Type)
   . Traversable t
  => t UnbalancedTx
  -> Contract (t (Either BalanceTxError FinalizedTransaction))
balanceTxsE unbalancedTxs = do
  storage <- asks _.storage
  let
    unlockAllOnError :: forall (a :: Type). Contract a -> Contract a
    unlockAllOnError f = catchError f $ \e -> do
      liftEffect $ for_ unbalancedTxs $
        unlockTransactionInputs storage <<< _.transaction <<< unwrap
      throwError e
  unlockAllOnError $ traverse balanceAndLockWithConstraintsE unbalancedTxs

balanceAndLockWithConstraintsE
  :: UnbalancedTx
  -> Contract (Either BalanceTxError FinalizedTransaction)
balanceAndLockWithConstraintsE unbalancedTx = do
  let
    tx' /\ ix = unUnbalancedTx unbalancedTx
  id <- asks _.appInstanceId
  balanceTxWithConstraints tx' ix (unwrap unbalancedTx).balancerConstraints >>=
    either (pure <<< Left)
      \finalizedTx -> do
        storage <- asks _.storage
        successfullyLocked <- liftEffect $
          lockTransactionInputs id storage (unwrap finalizedTx)
        if successfullyLocked then pure $ Right finalizedTx
        else balanceAndLockWithConstraintsE unbalancedTx

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

-- | Fetch transaction metadata.
-- | Returns `Right` when the transaction exists and metadata was non-empty
getTxMetadata
  :: TransactionHash
  -> Contract (Either GetTxMetadataError GeneralTransactionMetadata)
getTxMetadata th = do
  queryHandle <- getQueryHandle
  liftAff $ queryHandle.getTxMetadata th

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

submitTxFromConstraintsReturningFee
  :: forall (validator :: Type) (datum :: Type)
       (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups validator
  -> TxConstraints redeemer datum
  -> Contract { txHash :: TransactionHash, txFinalFee :: BigInt }
submitTxFromConstraintsReturningFee lookups constraints = do
  unbalancedTx <- liftedE $ mkUnbalancedTx lookups constraints
  balancedTx <- balanceTx unbalancedTx
  balancedSignedTx <- signTransaction balancedTx
  txHash <- submit balancedSignedTx
  pure { txHash, txFinalFee: getTxFinalFee balancedSignedTx }

submitTxFromConstraints
  :: forall (validator :: Type) (datum :: Type)
       (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups validator
  -> TxConstraints redeemer datum
  -> Contract TransactionHash
submitTxFromConstraints lookups constraints =
  _.txHash <$> submitTxFromConstraintsReturningFee lookups constraints
