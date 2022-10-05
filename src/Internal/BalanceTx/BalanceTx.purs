module Ctl.Internal.BalanceTx
  ( module BalanceTxErrorExport
  , module FinalizedTransaction
  , balanceTx
  , balanceTxWithConstraints
  ) where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Logger.Class (trace) as Logger
import Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionState
  , SelectionStrategy(SelectionStrategyOptimal)
  , _leftoverUtxos
  , performMultiAssetSelection
  , selectedInputs
  )
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
  ( BalanceTxError
      ( CouldNotGetCollateral
      , CouldNotGetUtxos
      , CouldNotGetWalletAddresses
      , UtxoLookupFailedFor
      , UtxoMinAdaValueCalculationFailed
      )
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
  , PrebalancedTransaction
  , askCip30Wallet
  , askCoinsPerUtxoUnit
  , askNetworkId
  , asksConstraints
  , liftEitherQueryM
  , liftQueryM
  , withBalanceTxConstraints
  )
import Ctl.Internal.BalanceTx.Types (FinalizedTransaction(FinalizedTransaction)) as FinalizedTransaction
import Ctl.Internal.BalanceTx.UtxoMinAda (utxoMinAdaValue)
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction
  , TransactionOutput(TransactionOutput)
  , TxBody
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
  , Value(Value)
  , equipartitionValueWithTokenQuantityUpperBound
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
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Types.OutputDatum (OutputDatum(NoOutputDatum))
import Ctl.Internal.Types.ScriptLookups (UnattachedUnbalancedTx)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Types.UnbalancedTransaction (_utxoIndex)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray) as NEArray
import Data.BigInt (BigInt)
import Data.Either (Either, note)
import Data.Foldable (foldMap, foldr)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~), (?~))
import Data.Log.Tag (TagSet)
import Data.Log.Tag (fromArray, tag) as TagSet
import Data.Map (empty, filterKeys, lookup, union) as Map
import Data.Maybe (Maybe(Nothing, Just), isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)

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

    ownAddrs <-
      maybe getWalletAddresses pure
        =<< asksConstraints Constraints._ownAddresses

    changeAddr <- liftMaybe CouldNotGetWalletAddresses $ Array.head ownAddrs

    utxos <- liftEitherQueryM $ traverse utxosAt ownAddrs <#>
      traverse (note CouldNotGetUtxos) -- Maybe -> Either and unwrap UtxoM

        >>> map (foldr Map.union Map.empty) -- merge all utxos into one map

    unbalancedCollTx <-
      case Array.null (unbalancedTx ^. _redeemersTxIns) of
        true ->
          -- Don't set collateral if tx doesn't contain phase-2 scripts:
          unbalancedTxWithNetworkId
        false ->
          setTransactionCollateral changeAddr =<< unbalancedTxWithNetworkId
    let
      allUtxos :: UtxoMap
      allUtxos =
        -- Combine utxos at the user address and those from any scripts
        -- involved with the contract in the unbalanced transaction:
        utxos `Map.union` (unbalancedTx ^. _unbalancedTx <<< _utxoIndex)

    availableUtxos <- liftQueryM $ filterLockedUtxos allUtxos

    -- Balance and finalize the transaction:
    runBalancer availableUtxos changeAddr
      (unbalancedTx # _transaction' .~ unbalancedCollTx)
  where
  unbalancedTxWithNetworkId :: BalanceTxM Transaction
  unbalancedTxWithNetworkId = do
    let transaction = unbalancedTx ^. _transaction'
    networkId <- maybe askNetworkId pure (transaction ^. _body <<< _networkId)
    pure (transaction # _body <<< _networkId ?~ networkId)

  setTransactionCollateral :: Address -> Transaction -> BalanceTxM Transaction
  setTransactionCollateral changeAddr transaction = do
    collateral <-
      liftEitherQueryM $ note CouldNotGetCollateral <$> getWalletCollateral
    let collaterisedTx = addTxCollateral collateral transaction
    -- Don't mess with Cip30 collateral
    isCip30 <- isJust <$> askCip30Wallet
    if isCip30 then pure collaterisedTx
    else addTxCollateralReturn collateral collaterisedTx changeAddr

--------------------------------------------------------------------------------
-- Balancing Algorithm
--------------------------------------------------------------------------------

type BalancerState =
  { unbalancedTx :: UnattachedUnbalancedTx
  , changeOutputs :: Array TransactionOutput
  , leftoverUtxos :: UtxoMap
  }

runBalancer
  :: UtxoMap
  -> Address
  -> UnattachedUnbalancedTx
  -> BalanceTxM FinalizedTransaction
runBalancer utxos changeAddress unbalancedTx' = do
  spendableUtxos <- getSpendableUtxos
  addLovelacesToTransactionOutputs unbalancedTx'
    >>= ((\tx -> mkBalancerState tx mempty spendableUtxos) >>> prebalanceTx)
  where
  getSpendableUtxos :: BalanceTxM UtxoMap
  getSpendableUtxos =
    asksConstraints Constraints._nonSpendableInputs <#>
      \nonSpendableInputs ->
        Map.filterKeys (not <<< flip Set.member nonSpendableInputs) utxos

  runNextBalancingStep
    :: UnattachedUnbalancedTx -> UtxoMap -> BalanceTxM FinalizedTransaction
  runNextBalancingStep unbalancedTx leftoverUtxos = do
    changeOutputs <-
      genTransactionChangeOutputs (unbalancedTx ^. _body')

    requiredValue <-
      except $ getRequiredValue utxos
        (setTxChangeOutputs changeOutputs unbalancedTx ^. _body')

    { unbalancedTx, changeOutputs, leftoverUtxos } #
      if requiredValue == mempty then balanceChangeAndMinFee else prebalanceTx

  prebalanceTx :: BalancerState -> BalanceTxM FinalizedTransaction
  prebalanceTx state@{ unbalancedTx, changeOutputs, leftoverUtxos } = do
    logBalancerState "Pre-balancing (Stage 1)" utxos state

    selectionState <-
      performCoinSelection
        (setTxChangeOutputs changeOutputs unbalancedTx ^. _body')
    let
      leftoverUtxos' :: UtxoMap
      leftoverUtxos' = selectionState ^. _leftoverUtxos

      selectedInputs' :: Set TransactionInput
      selectedInputs' = selectedInputs selectionState

      unbalancedTxWithInputs :: UnattachedUnbalancedTx
      unbalancedTxWithInputs =
        unbalancedTx # _body' <<< _inputs %~ Set.union selectedInputs'

    runNextBalancingStep unbalancedTxWithInputs leftoverUtxos'
    where
    performCoinSelection :: TxBody -> BalanceTxM SelectionState
    performCoinSelection txBody =
      except (getRequiredValue utxos txBody)
        >>= performMultiAssetSelection SelectionStrategyOptimal leftoverUtxos

  balanceChangeAndMinFee :: BalancerState -> BalanceTxM FinalizedTransaction
  balanceChangeAndMinFee
    state@{ unbalancedTx, changeOutputs, leftoverUtxos } = do
    logBalancerState "Balancing change and fees (Stage 2)" utxos state
    let
      prebalancedTx :: PrebalancedTransaction
      prebalancedTx = wrap $ setTxChangeOutputs changeOutputs unbalancedTx

      minFee :: BigInt
      minFee = unwrap $ unbalancedTx ^. _body' <<< _fee

    balancedTx /\ newMinFee <- evalExUnitsAndMinFee prebalancedTx utxos

    case newMinFee <= minFee of
      true ->
        liftQueryM (finalizeTransaction balancedTx utxos)
          <* logTransaction "Balanced transaction (Done)" utxos balancedTx
      false ->
        let
          unbalancedTxWithMinFee :: UnattachedUnbalancedTx
          unbalancedTxWithMinFee =
            unbalancedTx # _body' <<< _fee .~ Coin newMinFee
        in
          runNextBalancingStep unbalancedTxWithMinFee leftoverUtxos

  -- | Generates change outputs to return all excess `Value` back to the owner's 
  -- | address. If necessary, adds lovelaces to the generated change outputs to 
  -- | cover the utxo min-ada-value requirement.
  -- | 
  -- | If the `maxChangeOutputTokenQuantity` constraint is set, partitions the 
  -- | change `Value` into smaller `Value`s (where the Ada amount and the quantity 
  -- | of each token is equipartitioned across the resultant `Value`s; and no 
  -- | token quantity in any of the resultant `Value`s exceeds the given upper 
  -- | bound). Then builds `TransactionChangeOutput`s using those `Value`s. 
  genTransactionChangeOutputs :: TxBody -> BalanceTxM (Array TransactionOutput)
  genTransactionChangeOutputs txBody = do
    inputValue <- except $ getInputValue utxos txBody
    let
      posValue :: Value -> Value
      posValue (Value (Coin coin) nonAdaAsset) =
        mkValue (Coin $ max coin zero) (posNonAdaAsset nonAdaAsset)

      changeValue :: Value
      changeValue = posValue $
        (inputValue <> mintValue txBody)
          `minus` (outputValue txBody <> minFeeValue txBody)

      mkChangeOutput :: Value -> TransactionOutput
      mkChangeOutput amount =
        TransactionOutput
          { address: changeAddress
          , amount
          , datum: NoOutputDatum
          , scriptRef: Nothing
          }

    if changeValue == mempty then pure mempty
    else
      asksConstraints Constraints._maxChangeOutputTokenQuantity >>= case _ of
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
  txOutputMinAda <-
    ExceptT $ liftEffect $ utxoMinAdaValue coinsPerUtxoUnit txOutput
      <#> note UtxoMinAdaValueCalculationFailed
  let
    txOutputRec = unwrap txOutput

    txOutputValue :: Value
    txOutputValue = txOutputRec.amount

    newCoin :: Coin
    newCoin = Coin $ max (valueToCoin' txOutputValue) txOutputMinAda

  pure $ wrap txOutputRec
    { amount = mkValue newCoin (getNonAdaAsset txOutputValue) }

setTxChangeOutputs
  :: Array TransactionOutput -> UnattachedUnbalancedTx -> UnattachedUnbalancedTx
setTxChangeOutputs outputs = _body' <<< _outputs %~ flip append outputs

--------------------------------------------------------------------------------
-- Getters for various `Value`s
--------------------------------------------------------------------------------

getRequiredValue :: UtxoMap -> TxBody -> Either BalanceTxError Value
getRequiredValue utxos txBody =
  getInputValue utxos txBody <#> \inputValue ->
    (outputValue txBody <> minFeeValue txBody)
      `minus` (inputValue <> mintValue txBody)

getAmount :: TransactionOutput -> Value
getAmount = _.amount <<< unwrap

getInputValue :: UtxoMap -> TxBody -> Either BalanceTxError Value
getInputValue utxos txBody =
  foldMap getAmount <$>
    for (Array.fromFoldable $ txBody ^. _inputs) \oref ->
      note (UtxoLookupFailedFor oref) (Map.lookup oref utxos)

outputValue :: TxBody -> Value
outputValue txBody = foldMap getAmount (txBody ^. _outputs)

minFeeValue :: TxBody -> Value
minFeeValue txBody = mkValue (txBody ^. _fee) mempty

mintValue :: TxBody -> Value
mintValue txBody = maybe mempty (mkValue mempty <<< unwrap) (txBody ^. _mint)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

mkBalancerState
  :: UnattachedUnbalancedTx
  -> Array TransactionOutput
  -> UtxoMap
  -> BalancerState
mkBalancerState unbalancedTx changeOutputs leftoverUtxos =
  { unbalancedTx, changeOutputs, leftoverUtxos }

logBalancerState :: String -> UtxoMap -> BalancerState -> BalanceTxM Unit
logBalancerState message utxos { unbalancedTx, changeOutputs } =
  logTransactionWithChange message utxos (Just changeOutputs) unbalancedTx

logTransaction
  :: String -> UtxoMap -> UnattachedUnbalancedTx -> BalanceTxM Unit
logTransaction message utxos =
  logTransactionWithChange message utxos Nothing

logTransactionWithChange
  :: String
  -> UtxoMap
  -> Maybe (Array TransactionOutput)
  -> UnattachedUnbalancedTx
  -> BalanceTxM Unit
logTransactionWithChange message utxos mChangeOutputs unbalancedTx =
  let
    txBody :: TxBody
    txBody = unbalancedTx ^. _body'

    tag :: forall (a :: Type). Show a => String -> a -> TagSet
    tag title = TagSet.tag title <<< show

    outputValuesTagSet :: Maybe (Array TransactionOutput) -> Array TagSet
    outputValuesTagSet Nothing =
      [ "Output Value" `tag` outputValue txBody ]
    outputValuesTagSet (Just changeOutputs) =
      [ "Output Value without change" `tag` outputValue txBody
      , "Change Value" `tag` foldMap getAmount changeOutputs
      ]

    transactionInfo :: Value -> TagSet
    transactionInfo inputValue =
      TagSet.fromArray $
        [ "Input Value" `tag` inputValue
        , "Mint Value" `tag` mintValue txBody
        , "Fees" `tag` (txBody ^. _fee)
        ] <> outputValuesTagSet mChangeOutputs
  in
    except (getInputValue utxos txBody)
      >>= (flip Logger.trace (message <> ":") <<< transactionInfo)

