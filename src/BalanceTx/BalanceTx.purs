module BalanceTx
  ( module BalanceTxErrorExport
  , module FinalizedTransaction
  , balanceTx
  , balanceTxWithAddress
  ) where

import Prelude

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
  ( RequiredSigner(RequiredSigner)
  , Transaction(Transaction)
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  , Utxos
  , _body
  , _collateral
  , _fee
  , _inputs
  , _mint
  , _networkId
  , _outputs
  , _requiredSigners
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
import Contract.Prelude (foldr, for, fromMaybe)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Logger.Class as Logger
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (fromFoldable, head)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), hush, note)
import Data.Foldable (foldl, foldMap)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((.~), (?~), (%~))
import Data.Log.Tag (tag)
import Data.Map (empty, lookup, toUnfoldable, union, fromFoldable) as Map
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set, union)
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect)
import QueryM (QueryM)
import QueryM (getWalletAddresses) as QueryM
import QueryM.Utxos (filterLockedUtxos, getWalletCollateral, utxosAt)
import Serialization.Address
  ( Address
  , addressPaymentCred
  , stakeCredentialToKeyHash
  , withStakeCredential
  )
import Types.ScriptLookups (UnattachedUnbalancedTx)
import Types.Transaction (TransactionInput)
import Types.UnbalancedTransaction (_utxoIndex)

-- | Balances an unbalanced transaction using utxos from the current wallet's
-- | address.
balanceTx
  :: UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError FinalizedTransaction)
balanceTx unbalancedTx = do
  QueryM.getWalletAddresses >>= case _ of
    Nothing ->
      pure $ Left CouldNotGetWalletAddress
    Just address ->
      balanceTxWithAddress address unbalancedTx

-- | Like `balanceTx`, but allows to provide an address that is treated like
-- | user's own (while `balanceTx` gets it from the attached wallet).
balanceTxWithAddress
  :: Array Address
  -> UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError FinalizedTransaction)
balanceTxWithAddress ownAddrs unbalancedTx = runExceptT do
  utxos <- ExceptT $ traverse utxosAt ownAddrs <#>
    ( traverse (note CouldNotGetUtxos >>> map unwrap) --Maybe -> Either and unwrap UtxoM

        >>> map (foldr Map.union Map.empty) -- merge all utxos into one map
    )

  unbalancedCollTx <-
    case Array.null (unbalancedTx ^. _redeemersTxIns) of
      true ->
        -- Don't set collateral if tx doesn't contain phase-2 scripts:
        lift unbalancedTxWithNetworkId
      false ->
        setTransactionCollateral =<< lift unbalancedTxWithNetworkId

  let
    allUtxos :: Utxos
    allUtxos =
      -- Combine utxos at the user address and those from any scripts
      -- involved with the contract in the unbalanced transaction:
      utxos `Map.union` (unbalancedTx ^. _unbalancedTx <<< _utxoIndex)

  availableUtxos <- lift $ filterLockedUtxos allUtxos

  logTx "unbalancedCollTx" availableUtxos unbalancedCollTx

  changeAddr <- ExceptT $ pure $ note CouldNotGetWalletAddress $ head ownAddrs

  -- Balance and finalize the transaction: 
  ExceptT $ runBalancer availableUtxos changeAddr
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
      ExceptT $ note CouldNotGetCollateral <<< map (map (_.input <<< unwrap))
        <$> getWalletCollateral
    pure $ transaction # _body <<< _collateral ?~ collateral

--------------------------------------------------------------------------------
-- Balancing Algorithm
--------------------------------------------------------------------------------

type ChangeAddress = Address
type TransactionChangeOutput = TransactionOutput
type MinFee = BigInt
type Iteration = Int

runBalancer
  :: Utxos
  -> ChangeAddress
  -> UnattachedUnbalancedTx
  -> QueryM (Either BalanceTxError FinalizedTransaction)
runBalancer utxos changeAddress =
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

    walletCollats <- lift
      ( Map.fromFoldable
          <<< map
            ( ( case _ of
                  { input, output } -> input /\ output
              ) <<< unwrap
            )
          <<< fromMaybe [] <$> getWalletCollateral
      )

    let
      utxosWithCollats = utxos `Map.union` walletCollats
      collats =
        (unwrap prebalancedTx ^. _transaction' <<< _body <<< _collateral)
          # fromMaybe []
          # Set.fromFoldable
      txInputs = unwrap prebalancedTx ^. _transaction' <<< _body <<< _inputs
      signers = for (fromFoldable $ txInputs `union` collats) \ti -> do
        TransactionOutput { address } <- Map.lookup ti utxosWithCollats
        kh <- (addressPaymentCred >=> stakeCredentialToKeyHash) address
        pure $ RequiredSigner kh
      prebalancedWithSigners = PrebalancedTransaction
        ( unwrap prebalancedTx # _transaction' <<< _body <<< _requiredSigners .~
            signers
        )

    traceMainLoop "Required signers" "signers" signers

    balancedTx /\ newMinFee <- evalExUnitsAndMinFee prebalancedWithSigners

    traceMainLoop "calculated ex units and min fee" "balancedTx" balancedTx

    case newMinFee == minFee of
      true -> do
        finalizedTransaction <- lift $ finalizeTransaction balancedTx

        -- someMinFee <- lift $ calculateMinFee (unwrap finalizedTransaction)

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
  txOutputMinAda <- ExceptT $ utxoMinAdaValue txOutput
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
  :: ChangeAddress -> Utxos -> UnattachedUnbalancedTx -> TransactionChangeOutput
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
      { address: changeAddress, amount: changeValue, dataHash: Nothing }

addTransactionChangeOutput
  :: ChangeAddress -> Utxos -> UnattachedUnbalancedTx -> PrebalancedTransaction
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
  -> Utxos
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

  txChangeOutput <-
    addLovelacesToTransactionOutput
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
  -> Utxos
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
    :: Utxos -> Array TransactionInput -> Either BalanceTxError Value
  getTxInsValue utxos' =
    map (Array.foldMap getAmount) <<<
      traverse (\x -> note (UtxoLookupFailedFor x) $ Map.lookup x utxos')

  utxosToTransactionInput :: Utxos -> Array TransactionInput
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

getInputValue :: Utxos -> TxBody -> Value
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
  -> Utxos
  -> Transaction
  -> m Unit
logTx msg utxos (Transaction { body: body'@(TxBody body) }) =
  traverse_ (Logger.trace (tag msg mempty))
    [ "Input Value: " <> show (getInputValue utxos body')
    , "Output Value: " <> show (Array.foldMap getAmount body.outputs)
    , "Fees: " <> show body.fee
    ]

