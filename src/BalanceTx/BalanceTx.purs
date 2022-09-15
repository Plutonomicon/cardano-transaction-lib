module BalanceTx
  ( module BalanceTxErrorExport
  , module FinalizedTransaction
  , balanceTx
  , balanceTxWithAddress
  ) where

import Prelude

import BalanceTx.Collateral (addTxCollateral, addTxCollateralReturn)
import BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError
      ( CouldNotConvertScriptOutputToTxInput
      , CouldNotGetCollateral
      , CouldNotGetUtxos
      , CouldNotGetWalletAddress
      , ExUnitsEvaluationFailed
      , InsufficientTxInputs
      , ReindexRedeemersError
      , UtxoLookupFailedFor
      , UtxoMinAdaValueCalculationFailed
      )
  , Expected(Expected)
  , printTxEvaluationFailure
  ) as BalanceTxErrorExport
import BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError
      ( CouldNotConvertScriptOutputToTxInput
      , CouldNotGetCollateral
      , CouldNotGetUtxos
      , CouldNotGetWalletAddress
      , InsufficientTxInputs
      , UtxoLookupFailedFor
      , UtxoMinAdaValueCalculationFailed
      )
  , Expected(Expected)
  )
import BalanceTx.ExUnitsAndMinFee (evalExUnitsAndMinFee, finalizeTransaction)
import BalanceTx.Helpers (_body', _redeemersTxIns, _transaction', _unbalancedTx)
import BalanceTx.Types
  ( BalanceTxM
  , FinalizedTransaction
  , PrebalancedTransaction(PrebalancedTransaction)
  )
import BalanceTx.Types (FinalizedTransaction(FinalizedTransaction)) as FinalizedTransaction
import BalanceTx.UtxoMinAda (utxoMinAdaValue)
import Cardano.Types.Transaction
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
import Cardano.Types.Value
  ( Coin(Coin)
  , Value
  , geq
  , getNonAdaAsset
  , minus
  , mkValue
  , posNonAdaAsset
  , valueToCoin'
  )
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Logger.Class as Logger
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), hush, note)
import Data.Foldable (foldl, foldMap)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((.~), (?~), (%~))
import Data.Log.Tag (tag)
import Data.Map (lookup, toUnfoldable, union) as Map
import Data.Maybe (Maybe(Nothing, Just), maybe, isJust)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect, liftEffect)
import QueryM (QueryM)
import QueryM (getWalletAddress) as QueryM
import QueryM.Utxos (utxosAt, filterLockedUtxos, getWalletCollateral)
import QueryM.Ogmios (CoinsPerUtxoUnit)
import Serialization.Address (Address, addressPaymentCred, withStakeCredential)
import Types.OutputDatum (OutputDatum(NoOutputDatum))
import Types.ScriptLookups (UnattachedUnbalancedTx)
import Types.Transaction (TransactionInput)
import Types.UnbalancedTransaction (_utxoIndex)
import Wallet (cip30Wallet)

-- | Balances an unbalanced transaction using utxos from the current wallet's
-- | address.
balanceTx
  :: UnattachedUnbalancedTx
  -> UtxoMap
  -> QueryM (Either BalanceTxError FinalizedTransaction)
balanceTx unbalancedTx utxos = do
  QueryM.getWalletAddress >>= case _ of
    Nothing ->
      pure $ Left CouldNotGetWalletAddress
    Just address ->
      balanceTxWithAddress address unbalancedTx utxos

-- | Like `balanceTx`, but allows to provide an address that is treated like
-- | user's own (while `balanceTx` gets it from the attached wallet).
balanceTxWithAddress
  :: Address
  -> UnattachedUnbalancedTx
  -> UtxoMap
  -> QueryM (Either BalanceTxError FinalizedTransaction)
balanceTxWithAddress ownAddr unbalancedTx utxos' = runExceptT do
  utxos <- ExceptT $ utxosAt ownAddr
    <#> note CouldNotGetUtxos

  unbalancedCollTx <-
    case Array.null (unbalancedTx ^. _redeemersTxIns) of
      true ->
        -- Don't set collateral if tx doesn't contain phase-2 scripts:
        lift unbalancedTxWithNetworkId
      false ->
        setTransactionCollateral =<< lift unbalancedTxWithNetworkId

  let
    allUtxos :: UtxoMap
    allUtxos =
      -- Combine utxos at the user address and those from any scripts
      -- involved with the contract in the unbalanced transaction:
      utxos `Map.union` (unbalancedTx ^. _unbalancedTx <<< _utxoIndex)

  availableUtxos <- lift $ filterLockedUtxos allUtxos

  logTx "unbalancedCollTx" availableUtxos unbalancedCollTx

  -- Balance and finalize the transaction:
  ExceptT $ runBalancer availableUtxos utxos' ownAddr
    (unbalancedTx # _transaction' .~ unbalancedCollTx)
  where
  unbalancedTxWithNetworkId :: QueryM Transaction
  unbalancedTxWithNetworkId = do
    let transaction = unbalancedTx ^. _transaction'
    networkId <-
      transaction ^. _body <<< _networkId #
        maybe (asks $ _.networkId <<< _.config) pure
    pure (transaction # _body <<< _networkId ?~ networkId)

  setTransactionCollateral :: Transaction -> BalanceTxM Transaction
  setTransactionCollateral transaction = do
    collateral <-
      ExceptT $ note CouldNotGetCollateral <$> getWalletCollateral
    let collaterisedTx = addTxCollateral collateral transaction
    -- Don't mess with Cip30 collateral
    isCip30 <- asks $ (_.runtime >>> _.wallet >=> cip30Wallet) >>> isJust
    if isCip30 then pure collaterisedTx
    else addTxCollateralReturn
      collateral
      collaterisedTx
      ownAddr

--------------------------------------------------------------------------------
-- Balancing Algorithm
--------------------------------------------------------------------------------

type ChangeAddress = Address
type TransactionChangeOutput = TransactionOutput
type MinFee = BigInt
type Iteration = Int

runBalancer
  :: UtxoMap
  -> UtxoMap
  -> ChangeAddress
  -> UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError FinalizedTransaction)
runBalancer utxos utxos' changeAddress =
  runExceptT <<<
    (mainLoop one zero <=< addLovelacesToTransactionOutputs)
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

    let
      prebalancedTx =
        addTransactionChangeOutput changeAddress utxos unbalancedTxWithInputs

    traceMainLoop "added transaction change output" "prebalancedTx"
      prebalancedTx

    balancedTx /\ newMinFee <- evalExUnitsAndMinFee prebalancedTx utxos utxos'

    traceMainLoop "calculated ex units and min fee" "balancedTx" balancedTx

    case newMinFee == minFee of
      true -> do
        finalizedTransaction <- lift $ finalizeTransaction balancedTx utxos -- TODO: all available?

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
addLovelacesToTransactionOutputs unbalancedTx = do
  coinsPerUtxoUnit <- lift $ asks
    (_.runtime >>> _.pparams >>> unwrap >>> _.coinsPerUtxoUnit)
  map (\txOutputs -> unbalancedTx # _body' <<< _outputs .~ txOutputs) $
    traverse (addLovelacesToTransactionOutput coinsPerUtxoUnit)
      (unbalancedTx ^. _body' <<< _outputs)

addLovelacesToTransactionOutput
  :: CoinsPerUtxoUnit -> TransactionOutput -> BalanceTxM TransactionOutput
addLovelacesToTransactionOutput coinsPerUtxoUnit txOutput = do
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

-- | Generates a change output to return all excess `Value` back to the owner's
-- | address. Does NOT check if the generated output fulfills the utxo
-- | min-ada-value requirement (see Prerequisites).
-- |
-- | Prerequisites:
-- |   1. Must be called after `addTransactionInputs`, which guarantees that
-- |   the change output will cover the utxo min-ada-value requirement.
-- |
-- | TODO: Modify the logic to handle "The Problem of Concurrency"
-- | https://github.com/Plutonomicon/cardano-transaction-lib/issues/924
buildTransactionChangeOutput
  :: ChangeAddress
  -> UtxoMap
  -> UnattachedUnbalancedTx
  -> TransactionChangeOutput
buildTransactionChangeOutput changeAddress utxos tx =
  let
    txBody :: TxBody
    txBody = tx ^. _body'

    totalInputValue :: Value
    totalInputValue = getInputValue utxos txBody

    changeValue :: Value
    changeValue = posValue $
      (totalInputValue <> mintValue txBody)
        `minus` (totalOutputValue txBody <> minFeeValue txBody)
  in
    TransactionOutput
      { address: changeAddress
      , amount: changeValue
      , datum: NoOutputDatum
      , scriptRef: Nothing
      }

addTransactionChangeOutput
  :: ChangeAddress
  -> UtxoMap
  -> UnattachedUnbalancedTx
  -> PrebalancedTransaction
addTransactionChangeOutput changeAddress utxos unbalancedTx =
  PrebalancedTransaction $ unbalancedTx # _body' <<< _outputs %~
    Array.cons (buildTransactionChangeOutput changeAddress utxos unbalancedTx)

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

  coinsPerUtxoUnit <- lift $ asks
    (_.runtime >>> _.pparams >>> unwrap >>> _.coinsPerUtxoUnit)
  txChangeOutput <-
    addLovelacesToTransactionOutput coinsPerUtxoUnit
      (buildTransactionChangeOutput changeAddress utxos unbalancedTx)

  let
    changeValue :: Value
    changeValue = (unwrap txChangeOutput).amount

    requiredInputValue :: Value
    requiredInputValue = nonMintedValue <> minFeeValue txBody <> changeValue

  newTxInputs <-
    except $ collectTransactionInputs txInputs utxos requiredInputValue

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
