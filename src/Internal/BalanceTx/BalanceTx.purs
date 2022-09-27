module Ctl.Internal.BalanceTx
  ( module BalanceTxErrorExport
  , module FinalizedTransaction
  , balanceTx
  , balanceTxWithConstraints
  ) where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Logger.Class as Logger
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.BalanceTx.Collateral
  ( addTxCollateral
  , addTxCollateralReturn
  )
import Ctl.Internal.BalanceTx.Constraints (BalanceTxConstraintsBuilder)
import Ctl.Internal.BalanceTx.Constraints
  ( _maxChangeOutputTokenQuantity
  , _nonSpendableInputs
  , _ownAddresses
  ) as Constraints
import Ctl.Internal.BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError
      ( CouldNotConvertScriptOutputToTxInput
      , CouldNotGetCollateral
      , CouldNotGetUtxos
      , CouldNotGetWalletAddresses
      , ExUnitsEvaluationFailed
      , InsufficientTxInputs
      , ReindexRedeemersError
      , UtxoLookupFailedFor
      , UtxoMinAdaValueCalculationFailed
      )
  , Expected(Expected)
  , printTxEvaluationFailure
  ) as BalanceTxErrorExport
import Ctl.Internal.BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError
      ( CouldNotConvertScriptOutputToTxInput
      , CouldNotGetCollateral
      , CouldNotGetUtxos
      , CouldNotGetWalletAddresses
      , InsufficientTxInputs
      , UtxoLookupFailedFor
      , UtxoMinAdaValueCalculationFailed
      )
  , Expected(Expected)
  )
import Ctl.Internal.BalanceTx.ExUnitsAndMinFee
  ( evalExUnitsAndMinFee
  , finalizeTransaction
  )
import Ctl.Internal.BalanceTx.Helpers
  ( _body'
  , _redeemersTxIns
  , _transaction'
  , _unbalancedTx
  )
import Ctl.Internal.BalanceTx.Types
  ( BalanceTxM
  , FinalizedTransaction
  , PrebalancedTransaction(PrebalancedTransaction)
  , askCoinsPerUtxoUnit
  , askConstraints
  , liftEitherQueryM
  , liftQueryM
  , withBalanceTxConstraints
  )
import Ctl.Internal.BalanceTx.Types (FinalizedTransaction(FinalizedTransaction)) as FinalizedTransaction
import Ctl.Internal.BalanceTx.UtxoMinAda (utxoMinAdaValue)
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction(Transaction)
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  , UtxoMap
  , _body
  , _fee
  , _inputs
  , _mint
  , _networkId
  , _outputs
  )
import Ctl.Internal.Cardano.Types.Value
  ( Coin(Coin)
  , Value
  , equipartitionValueWithTokenQuantityUpperBound
  , geq
  , getNonAdaAsset
  , minus
  , mkValue
  , posNonAdaAsset
  , valueToCoin'
  )
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM (getWalletAddresses) as QueryM
import Ctl.Internal.QueryM.Utxos
  ( filterLockedUtxos
  , getWalletCollateral
  , utxosAt
  )
import Ctl.Internal.Serialization.Address
  ( Address
  , addressPaymentCred
  , withStakeCredential
  )
import Ctl.Internal.Types.OutputDatum (OutputDatum(NoOutputDatum))
import Ctl.Internal.Types.ScriptLookups (UnattachedUnbalancedTx)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Types.UnbalancedTransaction (_utxoIndex)
import Ctl.Internal.Wallet (cip30Wallet)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray) as NEArray
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), hush, note)
import Data.Foldable (foldMap, foldl, foldr)
import Data.Lens.Getter (view, (^.))
import Data.Lens.Setter ((%~), (.~), (?~))
import Data.Log.Tag (tag)
import Data.Map (empty, filterKeys, lookup, toUnfoldable, union) as Map
import Data.Maybe (Maybe(Nothing, Just), isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)

-- | Balances an unbalanced transaction using the default constraints.
balanceTx
  :: UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError FinalizedTransaction)
balanceTx = balanceTxWithConstraints mempty

-- | Like `balanceTx`, but allows to specify constraints to be considered when
-- | balancing a transaction.
balanceTxWithConstraints
  :: BalanceTxConstraintsBuilder
  -> UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError FinalizedTransaction)
balanceTxWithConstraints constraintsBuilder unbalancedTx =
  withBalanceTxConstraints constraintsBuilder $ runExceptT do
    let
      getWalletAddresses :: BalanceTxM (Array Address)
      getWalletAddresses =
        liftEitherQueryM $
          QueryM.getWalletAddresses <#> note CouldNotGetWalletAddresses

    constraints <- askConstraints

    ownAddrs <-
      maybe getWalletAddresses pure (constraints ^. Constraints._ownAddresses)

    changeAddr <- liftMaybe CouldNotGetWalletAddresses $ Array.head ownAddrs

    utxos <- liftEitherQueryM $ traverse utxosAt ownAddrs <#>
      traverse (note CouldNotGetUtxos) -- Maybe -> Either and unwrap UtxoM

        >>> map (foldr Map.union Map.empty) -- merge all utxos into one map

    unbalancedCollTx <-
      case Array.null (unbalancedTx ^. _redeemersTxIns) of
        true ->
          -- Don't set collateral if tx doesn't contain phase-2 scripts:
          liftQueryM unbalancedTxWithNetworkId
        false ->
          setTransactionCollateral changeAddr
            =<< liftQueryM unbalancedTxWithNetworkId
    let
      allUtxos :: UtxoMap
      allUtxos =
        -- Combine utxos at the user address and those from any scripts
        -- involved with the contract in the unbalanced transaction:
        utxos `Map.union` (unbalancedTx ^. _unbalancedTx <<< _utxoIndex)

    availableUtxos <- liftQueryM $ filterLockedUtxos allUtxos

    logTx "unbalancedCollTx" availableUtxos unbalancedCollTx

    -- Balance and finalize the transaction:
    runBalancer availableUtxos changeAddr
      (unbalancedTx # _transaction' .~ unbalancedCollTx)
  where
  unbalancedTxWithNetworkId :: QueryM Transaction
  unbalancedTxWithNetworkId = do
    let transaction = unbalancedTx ^. _transaction'
    networkId <-
      transaction ^. _body <<< _networkId #
        maybe (asks $ _.networkId <<< _.config) pure
    pure (transaction # _body <<< _networkId ?~ networkId)

  setTransactionCollateral :: Address -> Transaction -> BalanceTxM Transaction
  setTransactionCollateral changeAddr transaction = do
    collateral <-
      liftEitherQueryM $ note CouldNotGetCollateral <$> getWalletCollateral
    let collaterisedTx = addTxCollateral collateral transaction
    -- Don't mess with Cip30 collateral
    isCip30 <- asks $ (_.runtime >>> _.wallet >=> cip30Wallet) >>> isJust
    if isCip30 then pure collaterisedTx
    else addTxCollateralReturn
      collateral
      collaterisedTx
      changeAddr

--------------------------------------------------------------------------------
-- Balancing Algorithm
--------------------------------------------------------------------------------

type ChangeAddress = Address
type TransactionChangeOutput = TransactionOutput
type MinFee = BigInt
type Iteration = Int

runBalancer
  :: UtxoMap
  -> ChangeAddress
  -> UnattachedUnbalancedTx
  -> BalanceTxM FinalizedTransaction
runBalancer utxos changeAddress =
  mainLoop one zero <=< addLovelacesToTransactionOutputs
  where
  mainLoop
    :: Iteration
    -> MinFee
    -> UnattachedUnbalancedTx
    -> BalanceTxM FinalizedTransaction
  mainLoop iteration minFee unbalancedTx = do
    let
      unbalancedTxWithMinFee =
        setTransactionMinFee minFee unbalancedTx

    unbalancedTxWithInputs <-
      addTransactionInputs changeAddress utxos unbalancedTxWithMinFee

    traceMainLoop "added transaction inputs" "unbalancedTxWithInputs"
      unbalancedTxWithInputs

    prebalancedTx <-
      addTransactionChangeOutputs changeAddress utxos unbalancedTxWithInputs

    traceMainLoop "added transaction change output" "prebalancedTx"
      prebalancedTx

    balancedTx /\ newMinFee <- evalExUnitsAndMinFee prebalancedTx utxos

    traceMainLoop "calculated ex units and min fee" "balancedTx" balancedTx

    case newMinFee == minFee of
      true -> do
        finalizedTransaction <-
          liftQueryM $ finalizeTransaction balancedTx utxos -- TODO: all available?

        traceMainLoop "finalized transaction" "finalizedTransaction"
          finalizedTransaction

        pure finalizedTransaction
      false ->
        mainLoop (iteration + one) newMinFee unbalancedTxWithInputs
    where
    traceMainLoop
      :: forall (a :: Type). Show a => String -> String -> a -> BalanceTxM Unit
    traceMainLoop meta message object =
      let
        tagMessage :: String
        tagMessage =
          "mainLoop (iteration " <> show iteration <> "): " <> meta
      in
        Logger.trace (tag tagMessage "^") $ message <> ": " <> show object

setTransactionMinFee
  :: MinFee -> UnattachedUnbalancedTx -> UnattachedUnbalancedTx
setTransactionMinFee minFee = _body' <<< _fee .~ Coin minFee

-- | For each transaction output, if necessary, adds some number of lovelaces
-- | to cover the utxo min-ada-value requirement.
addLovelacesToTransactionOutputs
  :: UnattachedUnbalancedTx -> BalanceTxM UnattachedUnbalancedTx
addLovelacesToTransactionOutputs unbalancedTx =
  map (\txOutputs -> unbalancedTx # _body' <<< _outputs .~ txOutputs) $
    traverse addLovelacesToTransactionOutput
      (unbalancedTx ^. _body' <<< _outputs)

addLovelacesToTransactionOutput
  :: TransactionOutput -> BalanceTxM TransactionOutput
addLovelacesToTransactionOutput txOutput = do
  coinsPerUtxoUnit <- askCoinsPerUtxoUnit
  txOutputMinAda <- ExceptT $ liftEffect $
    utxoMinAdaValue coinsPerUtxoUnit txOutput
      <#> note UtxoMinAdaValueCalculationFailed
  let
    txOutputRec = unwrap txOutput

    txOutputValue :: Value
    txOutputValue = txOutputRec.amount

    newCoin :: Coin
    newCoin = Coin $ max (valueToCoin' txOutputValue) txOutputMinAda

  pure $ wrap txOutputRec
    { amount = mkValue newCoin (getNonAdaAsset txOutputValue) }

-- | Generates change outputs to return all excess `Value` back to the owner's 
-- | address. If necessary, adds lovelaces to the generated change outputs to 
-- | cover the utxo min-ada-value requirement.
-- | 
-- | If the `maxChangeOutputTokenQuantity` constraint is set, partitions the 
-- | change `Value` into smaller `Value`s (where the Ada amount and the quantity 
-- | of each token is equipartitioned across the resultant `Value`s; and no 
-- | token quantity in any of the resultant `Value`s exceeds the given upper 
-- | bound). Then builds `TransactionChangeOutput`s using those `Value`s. 
genTransactionChangeOutputs
  :: ChangeAddress
  -> UtxoMap
  -> UnattachedUnbalancedTx
  -> BalanceTxM (Array TransactionChangeOutput)
genTransactionChangeOutputs changeAddress utxos tx = do
  let
    txBody :: TxBody
    txBody = tx ^. _body'

    totalInputValue :: Value
    totalInputValue = getInputValue utxos txBody

    changeValue :: Value
    changeValue = posValue $
      (totalInputValue <> mintValue txBody)
        `minus` (totalOutputValue txBody <> minFeeValue txBody)

    mkChangeOutput :: Value -> TransactionChangeOutput
    mkChangeOutput amount =
      TransactionOutput
        { address: changeAddress
        , amount
        , datum: NoOutputDatum
        , scriptRef: Nothing
        }

  constraints <- askConstraints
  case (constraints ^. Constraints._maxChangeOutputTokenQuantity) of
    Nothing ->
      Array.singleton <$>
        addLovelacesToTransactionOutput (mkChangeOutput changeValue)

    Just maxChangeOutputTokenQuantity ->
      let
        values :: NonEmptyArray Value
        values =
          equipartitionValueWithTokenQuantityUpperBound changeValue
            maxChangeOutputTokenQuantity
      in
        traverse (addLovelacesToTransactionOutput <<< mkChangeOutput)
          (NEArray.toArray values)

addTransactionChangeOutputs
  :: ChangeAddress
  -> UtxoMap
  -> UnattachedUnbalancedTx
  -> BalanceTxM PrebalancedTransaction
addTransactionChangeOutputs changeAddress utxos unbalancedTx = do
  changeOutputs <- genTransactionChangeOutputs changeAddress utxos unbalancedTx
  pure $ PrebalancedTransaction $
    unbalancedTx # _body' <<< _outputs %~ flip append changeOutputs

-- | Selects a combination of unspent transaction outputs from the wallet's
-- | utxo set so that the total input value is sufficient to cover all
-- | transaction outputs, including the change that will be generated
-- | when using that particular combination of inputs.
-- |
-- | Prerequisites:
-- |   1. Must be called with a transaction with no change output.
-- |   2. The `fee` field of a transaction body must be set.
addTransactionInputs
  :: ChangeAddress
  -> UtxoMap
  -> UnattachedUnbalancedTx
  -> BalanceTxM UnattachedUnbalancedTx
addTransactionInputs changeAddress utxos unbalancedTx = do
  let
    txBody :: TxBody
    txBody = unbalancedTx ^. _body'

    txInputs :: Set TransactionInput
    txInputs = txBody ^. _inputs

    nonMintedValue :: Value
    nonMintedValue = totalOutputValue txBody `minus` mintValue txBody

  txChangeOutputs <-
    genTransactionChangeOutputs changeAddress utxos unbalancedTx

  nonSpendableInputs <-
    askConstraints <#> view Constraints._nonSpendableInputs

  let
    changeValue :: Value
    changeValue = foldMap getAmount txChangeOutputs

    requiredInputValue :: Value
    requiredInputValue = nonMintedValue <> minFeeValue txBody <> changeValue

    spendableUtxos :: UtxoMap
    spendableUtxos =
      Map.filterKeys (not <<< flip Set.member nonSpendableInputs) utxos

  newTxInputs <-
    except $ collectTransactionInputs txInputs spendableUtxos requiredInputValue

  case newTxInputs == txInputs of
    true ->
      pure unbalancedTx
    false ->
      addTransactionInputs changeAddress utxos
        (unbalancedTx # _body' <<< _inputs %~ Set.union newTxInputs)

collectTransactionInputs
  :: Set TransactionInput
  -> UtxoMap
  -> Value
  -> Either BalanceTxError (Set TransactionInput)
collectTransactionInputs originalTxIns utxos value = do
  txInsValue <- updatedInputs >>= getTxInsValue utxos
  updatedInputs' <- updatedInputs
  case isSufficient updatedInputs' txInsValue of
    true ->
      pure $ Set.fromFoldable updatedInputs'
    false ->
      Left $ InsufficientTxInputs (Expected value) (Actual txInsValue)
  where
  updatedInputs :: Either BalanceTxError (Array TransactionInput)
  updatedInputs =
    foldl
      ( \newTxIns txIn -> do
          txIns <- newTxIns
          txInsValue <- getTxInsValue utxos txIns
          case Array.elem txIn txIns || isSufficient txIns txInsValue of
            true -> newTxIns
            false ->
              Right $ Array.insert txIn txIns -- treat as a set.
      )
      (Right $ Array.fromFoldable originalTxIns)
      $ utxosToTransactionInput utxos

  isSufficient :: Array TransactionInput -> Value -> Boolean
  isSufficient txIns' txInsValue =
    not (Array.null txIns') && txInsValue `geq` value

  getTxInsValue
    :: UtxoMap -> Array TransactionInput -> Either BalanceTxError Value
  getTxInsValue utxos' =
    map (Array.foldMap getAmount) <<<
      traverse (\x -> note (UtxoLookupFailedFor x) $ Map.lookup x utxos')

  utxosToTransactionInput :: UtxoMap -> Array TransactionInput
  utxosToTransactionInput =
    Array.mapMaybe (hush <<< getPublicKeyTransactionInput) <<< Map.toUnfoldable

getAmount :: TransactionOutput -> Value
getAmount = _.amount <<< unwrap

totalOutputValue :: TxBody -> Value
totalOutputValue txBody = foldMap getAmount (txBody ^. _outputs)

mintValue :: TxBody -> Value
mintValue txBody = maybe mempty (mkValue mempty <<< unwrap) (txBody ^. _mint)

minFeeValue :: TxBody -> Value
minFeeValue txBody = mkValue (txBody ^. _fee) mempty

posValue :: Value -> Value
posValue value = mkValue
  (Coin $ max (valueToCoin' value) zero)
  (posNonAdaAsset $ getNonAdaAsset value)

-- | Get `TransactionInput` such that it is associated to `PaymentCredentialKey`
-- | and not `PaymentCredentialScript`, i.e. we want wallets only
getPublicKeyTransactionInput
  :: TransactionInput /\ TransactionOutput
  -> Either BalanceTxError TransactionInput
getPublicKeyTransactionInput (txOutRef /\ txOut) =
  note CouldNotConvertScriptOutputToTxInput $ do
    paymentCred <- unwrap txOut # (_.address >>> addressPaymentCred)
    -- TEST ME: using StakeCredential to determine whether wallet or script
    paymentCred # withStakeCredential
      { onKeyHash: const $ pure txOutRef
      , onScriptHash: const Nothing
      }

getInputValue :: UtxoMap -> TxBody -> Value
getInputValue utxos (TxBody txBody) =
  Array.foldMap
    getAmount
    ( Array.mapMaybe (flip Map.lookup utxos)
        <<< Array.fromFoldable
        <<< _.inputs $ txBody
    )

--------------------------------------------------------------------------------
-- Logging Helpers
--------------------------------------------------------------------------------

-- Logging for Transaction type without returning Transaction
logTx
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => MonadLogger m
  => String
  -> UtxoMap
  -> Transaction
  -> m Unit
logTx msg utxos (Transaction { body: body'@(TxBody body) }) =
  traverse_ (Logger.trace (tag msg mempty))
    [ "Input Value: " <> show (getInputValue utxos body')
    , "Output Value: " <> show (Array.foldMap getAmount body.outputs)
    , "Fees: " <> show body.fee
    ]
