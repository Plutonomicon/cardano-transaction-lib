module Ctl.Internal.BalanceTx
  ( module BalanceTxErrorExport
  , module FinalizedTransaction
  , balanceTxWithConstraints
  ) where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
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
  ( _changeAddress
  , _maxChangeOutputTokenQuantity
  , _nonSpendableInputs
  , _srcAddresses
  ) as Constraints
import Ctl.Internal.BalanceTx.Error
  ( Actual(Actual)
  , BalanceTxError
      ( CouldNotGetChangeAddress
      , CouldNotGetCollateral
      , CouldNotGetUtxos
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
      ( CouldNotGetChangeAddress
      , CouldNotGetCollateral
      , CouldNotGetUtxos
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
  ( Certificate(StakeRegistration, StakeDeregistration)
  , Transaction(Transaction)
  , TransactionOutput(TransactionOutput)
  , TxBody
  , UtxoMap
  , _body
  , _certs
  , _fee
  , _inputs
  , _mint
  , _networkId
  , _outputs
  , _referenceInputs
  , _withdrawals
  )
import Ctl.Internal.Cardano.Types.Value
  ( Coin(Coin)
  , Value
  , coinToValue
  , equipartitionValueWithTokenQuantityUpperBound
  , getNonAdaAsset
  , minus
  , mkValue
  , valueToCoin'
  )
import Ctl.Internal.QueryM (QueryM, getProtocolParameters)
import Ctl.Internal.QueryM (getChangeAddress, getWalletAddresses) as QueryM
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
import Data.Foldable (fold, foldMap, foldr, sum)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~), (?~))
import Data.Map (empty, lookup, union) as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)

-- | Balances an unbalanced transaction using the specified balancer
-- | constraints.
balanceTxWithConstraints
  :: UnattachedUnbalancedTx
  -> BalanceTxConstraintsBuilder
  -> QueryM (Either BalanceTxError FinalizedTransaction)
balanceTxWithConstraints unbalancedTx constraintsBuilder = do
  pparams <- getProtocolParameters

  withBalanceTxConstraints constraintsBuilder $ runExceptT do
    let
      depositValuePerCert = (unwrap pparams).stakeAddressDeposit
      certsFee = getStakingBalance (unbalancedTx ^. _transaction')
        depositValuePerCert

    srcAddrs <-
      asksConstraints Constraints._srcAddresses
        >>= maybe (liftQueryM QueryM.getWalletAddresses) pure

    changeAddr <- getChangeAddress

    utxos <- liftEitherQueryM $ traverse utxosAt srcAddrs <#>
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
    runBalancer allUtxos availableUtxos changeAddr certsFee
      (unbalancedTx # _transaction' .~ unbalancedCollTx)
  where
  getChangeAddress :: BalanceTxM Address
  getChangeAddress =
    liftMaybe CouldNotGetChangeAddress
      =<< maybe (liftQueryM QueryM.getChangeAddress) (pure <<< Just)
      =<< asksConstraints Constraints._changeAddress

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

type BalanceTxState =
  { unbalancedTx :: UnattachedUnbalancedTx
  , changeOutputs :: Array TransactionOutput
  , leftoverUtxos :: UtxoMap
  }

runBalancer
  :: UtxoMap
  -> UtxoMap
  -> Address
  -> Coin
  -> UnattachedUnbalancedTx
  -> BalanceTxM FinalizedTransaction
runBalancer allUtxos utxos changeAddress certsFee =
  ( prebalanceTx
      <<< { unbalancedTx: _, changeOutputs: mempty, leftoverUtxos: utxos }
  )
    <=< addLovelacesToTransactionOutputs
  where
  prebalanceTx :: BalanceTxState -> BalanceTxM FinalizedTransaction
  prebalanceTx { unbalancedTx, changeOutputs, leftoverUtxos } = do
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

    changeOutputs' <-
      genTransactionChangeOutputs certsFee (unbalancedTxWithInputs ^. _body')

    requiredValue <-
      except $ getRequiredValue certsFee utxos
        (setTxChangeOutputs changeOutputs' unbalancedTxWithInputs ^. _body')

    let
      updatedState :: BalanceTxState
      updatedState =
        { unbalancedTx: unbalancedTxWithInputs
        , changeOutputs: changeOutputs'
        , leftoverUtxos: leftoverUtxos'
        }

    case requiredValue == mempty of
      true ->
        balanceChangeAndMinFee updatedState
      false ->
        prebalanceTx updatedState
    where
    performCoinSelection :: TxBody -> BalanceTxM SelectionState
    performCoinSelection txBody =
      except (getRequiredValue certsFee utxos txBody)
        >>= performMultiAssetSelection SelectionStrategyOptimal leftoverUtxos

  balanceChangeAndMinFee :: BalanceTxState -> BalanceTxM FinalizedTransaction
  balanceChangeAndMinFee state@{ unbalancedTx, changeOutputs } = do
    let
      prebalancedTx :: PrebalancedTransaction
      prebalancedTx =
        wrap (setTxChangeOutputs changeOutputs unbalancedTx)

    balancedTx /\ minFee <- evalExUnitsAndMinFee prebalancedTx allUtxos

    case Coin minFee <= unbalancedTx ^. _body' <<< _fee of
      true ->
        finalizeTransaction balancedTx allUtxos
      false -> do
        let
          unbalancedTxWithMinFee :: UnattachedUnbalancedTx
          unbalancedTxWithMinFee =
            unbalancedTx # _body' <<< _fee .~ Coin minFee

        changeOutputs' <- genTransactionChangeOutputs certsFee
          (unbalancedTxWithMinFee ^. _body')

        requiredValue <-
          except $ getRequiredValue certsFee utxos
            (setTxChangeOutputs changeOutputs' unbalancedTxWithMinFee ^. _body')

        let
          updatedState :: BalanceTxState
          updatedState = state
            { unbalancedTx = unbalancedTxWithMinFee
            , changeOutputs = changeOutputs'
            }

        case requiredValue == mempty of
          true ->
            balanceChangeAndMinFee updatedState
          false ->
            prebalanceTx updatedState

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
    :: Coin -> TxBody -> BalanceTxM (Array TransactionOutput)
  genTransactionChangeOutputs certsFee txBody = do
    inputValue <- except $ getInputValue utxos txBody
    let
      changeValue :: Value
      changeValue =
        (inputValue <> mintValue txBody) `minus`
          (outputValue txBody <> minFeeValue txBody <> coinToValue certsFee)

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

getRequiredValue :: Coin -> UtxoMap -> TxBody -> Either BalanceTxError Value
getRequiredValue certsFee utxos txBody =
  getInputValue utxos txBody <#> \inputValue ->
    (outputValue txBody <> minFeeValue txBody <> coinToValue certsFee)
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

-- | Accounts for:
-- |
-- | - stake registration deposit
-- | - stake deregistration deposit returns
-- | - stake withdrawals fees
getStakingBalance :: Transaction -> Coin -> Coin
getStakingBalance tx depositLovelacesPerCert =
  let
    stakeDeposits :: BigInt
    stakeDeposits =
      (tx ^. _body <<< _certs) # fold
        >>> map
          case _ of
            StakeRegistration _ -> unwrap depositLovelacesPerCert
            StakeDeregistration _ -> negate $ unwrap depositLovelacesPerCert
            _ -> zero
        >>> sum
    stakeWithdrawals =
      unwrap $ fold $ fromMaybe Map.empty $ tx ^. _body <<<
        _withdrawals
    fee = stakeDeposits - stakeWithdrawals
  in
    Coin fee

