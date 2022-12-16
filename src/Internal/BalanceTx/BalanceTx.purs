module Ctl.Internal.BalanceTx
  ( module BalanceTxErrorExport
  , module FinalizedTransaction
  , balanceTxWithConstraints
  ) where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Logger.Class (trace) as Logger
import Control.Parallel (parTraverse)
import Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionState
  , SelectionStrategy
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
  , _selectionStrategy
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
  , Transaction
  , TransactionOutput
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
  ( AssetClass
  , Coin(Coin)
  , Value(Value)
  , coinToValue
  , equipartitionValueWithTokenQuantityUpperBound
  , getNonAdaAsset
  , lovelaceValueOf
  , minus
  , mkValue
  , posNonAdaAsset
  , valueToCoin'
  )
import Ctl.Internal.Cardano.Types.Value
  ( assetToValue
  , getAssetQuantity
  , valueAssets
  ) as Value
import Ctl.Internal.CoinSelection.UtxoIndex (UtxoIndex, buildUtxoIndex)
import Ctl.Internal.Helpers ((??))
import Ctl.Internal.Partition (equipartition, partition)
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
import Ctl.Internal.Types.UnbalancedTransaction (_utxoIndex)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty
  ( cons'
  , fromArray
  , replicate
  , singleton
  , sortWith
  , toArray
  , uncons
  , zip
  , zipWith
  ) as NEArray
import Data.BigInt (BigInt)
import Data.Either (Either, note)
import Data.Foldable (fold, foldMap, foldr, length, null, sum)
import Data.Function (on)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~), (?~))
import Data.Log.Tag (TagSet)
import Data.Log.Tag (fromArray, tag) as TagSet
import Data.Map (empty, filterKeys, lookup, union) as Map
import Data.Maybe (Maybe(Nothing, Just), fromJust, fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as Set
import Data.Traversable (for, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)

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

    utxos <- liftEitherQueryM $ parTraverse utxosAt srcAddrs <#>
      traverse (note CouldNotGetUtxos)
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

    selectionStrategy <- asksConstraints Constraints._selectionStrategy

    -- Balance and finalize the transaction:
    runBalancer
      { strategy: selectionStrategy
      , unbalancedTx: unbalancedTx # _transaction' .~ unbalancedCollTx
      , changeAddress: changeAddr
      , allUtxos
      , utxos: availableUtxos
      , certsFee
      }
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

type BalancerParams =
  { strategy :: SelectionStrategy
  , unbalancedTx :: UnattachedUnbalancedTx
  , changeAddress :: Address
  , allUtxos :: UtxoMap
  , utxos :: UtxoMap
  , certsFee :: Coin
  }

type BalancerState =
  { transaction :: UnattachedUnbalancedTx
  , leftoverUtxos :: UtxoIndex
  , changeOutputs :: Array TransactionOutput
  , minFee :: BigInt
  }

initBalancerState
  :: UnattachedUnbalancedTx
  -> UtxoMap
  -> BalancerState
initBalancerState transaction =
  buildUtxoIndex >>>
    { transaction, leftoverUtxos: _, changeOutputs: mempty, minFee: zero }

data BalancerStep
  = PrebalanceTx BalancerState
  | BalanceChangeAndMinFee BalancerState

runBalancer :: BalancerParams -> BalanceTxM FinalizedTransaction
runBalancer p = do
  spendableUtxos <- getSpendableUtxos
  unbalancedTx <- addLovelacesToTransactionOutputs p.unbalancedTx
  mainLoop (initBalancerState unbalancedTx spendableUtxos)
  where
  getSpendableUtxos :: BalanceTxM UtxoMap
  getSpendableUtxos =
    asksConstraints Constraints._nonSpendableInputs <#>
      \nonSpendableInputs ->
        flip Map.filterKeys p.utxos \oref -> not $
          Set.member oref nonSpendableInputs
            || Set.member oref (p.unbalancedTx ^. _body' <<< _referenceInputs)

  mainLoop :: BalancerState -> BalanceTxM FinalizedTransaction
  mainLoop = worker <<< PrebalanceTx
    where
    worker :: BalancerStep -> BalanceTxM FinalizedTransaction
    worker (PrebalanceTx state) = do
      logBalancerState "Pre-balancing (Stage 1)" p.allUtxos state
      prebalanceTx state >>= runNextBalancerStep
    worker (BalanceChangeAndMinFee s@{ transaction, minFee, leftoverUtxos }) =
      do
        logBalancerState "Balancing change and fees (Stage 2)" p.allUtxos s
        { transaction: balancedTx, minFee: newMinFee } <- evaluateTx s
        case newMinFee <= minFee of
          true ->
            if (Set.isEmpty $ balancedTx ^. _body' <<< _inputs) then do
              selectionState <-
                performMultiAssetSelection p.strategy leftoverUtxos
                  (lovelaceValueOf one)
              runNextBalancerStep $ s
                { transaction =
                    balancedTx # _body' <<< _inputs %~
                      Set.union (selectedInputs selectionState)
                , leftoverUtxos =
                    selectionState ^. _leftoverUtxos
                }
            else
              logTransaction "Balanced transaction (Done)" p.allUtxos balancedTx
                *> finalizeTransaction balancedTx p.allUtxos
          false ->
            runNextBalancerStep $ s
              { transaction = transaction # _body' <<< _fee .~ Coin newMinFee
              , minFee = newMinFee
              }

    -- | Determines which balancing step will be performed next.
    -- |
    -- | If the transaction remains unbalanced (i.e. `requiredValue != mempty`)
    -- | after generation of change, the first balancing step `PrebalanceTx`
    -- | is performed, otherwise we proceed to `BalanceChangeAndMinFee`.
    runNextBalancerStep :: BalancerState -> BalanceTxM FinalizedTransaction
    runNextBalancerStep state@{ transaction } = do
      let txBody = transaction ^. _body'
      inputValue <- except $ getInputValue p.allUtxos txBody
      changeOutputs <- makeChange p.changeAddress inputValue p.certsFee txBody

      requiredValue <-
        except $ getRequiredValue p.certsFee p.allUtxos
          (setTxChangeOutputs changeOutputs transaction ^. _body')

      worker $ state { changeOutputs = changeOutputs } #
        if requiredValue == mempty then BalanceChangeAndMinFee else PrebalanceTx

    -- | Selects a combination of unspent transaction outputs from the wallet's
    -- | utxo set so that the total input value is sufficient to cover all
    -- | transaction outputs, including generated change and min fee.
    prebalanceTx :: BalancerState -> BalanceTxM BalancerState
    prebalanceTx state@{ transaction, changeOutputs, leftoverUtxos } =
      performCoinSelection <#> \selectionState -> state
        { transaction =
            transaction # _body' <<< _inputs %~
              Set.union (selectedInputs selectionState)
        , leftoverUtxos =
            selectionState ^. _leftoverUtxos
        }
      where
      performCoinSelection :: BalanceTxM SelectionState
      performCoinSelection =
        let
          txBody :: TxBody
          txBody = setTxChangeOutputs changeOutputs transaction ^. _body'
        in
          except (getRequiredValue p.certsFee p.allUtxos txBody)
            >>= performMultiAssetSelection p.strategy leftoverUtxos

    -- | Calculates execution units for each script in the transaction and sets
    -- | min fee.
    -- |
    -- | The transaction must be pre-balanced before evaluating execution units,
    -- | since this pre-condition is sometimes required for successfull script
    -- | execution during transaction evaluation.
    evaluateTx :: BalancerState -> BalanceTxM BalancerState
    evaluateTx state@{ transaction, changeOutputs } = do
      let
        prebalancedTx :: PrebalancedTransaction
        prebalancedTx = wrap $ setTxChangeOutputs changeOutputs transaction

      transaction' /\ minFee <- evalExUnitsAndMinFee prebalancedTx p.allUtxos
      pure $ state { transaction = transaction', minFee = minFee }

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
-- Making change
--------------------------------------------------------------------------------

-- | Constructs change outputs to return all excess `Value` back to the owner's
-- | address.
-- |
-- | Returns an array of change outputs even if the transaction becomes
-- | unbalanced after attaching them (which can be the case if the specified
-- | inputs do not provide enough ada to satisfy minimum ada quantites of the
-- | change outputs generated).
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/4c2eb651d79212157a749d8e69a48fff30862e93/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1396
makeChange
  :: Address -> Value -> Coin -> TxBody -> BalanceTxM (Array TransactionOutput)
makeChange changeAddress inputValue certsFee txBody =
  -- Always generate change when a transaction has no outputs to avoid issues
  -- with transaction confirmation:
  -- FIXME: https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
  if excessValue == mempty && (txBody ^. _outputs) /= mempty then pure mempty
  else
    map (mkChangeOutput changeAddress) <$>
      ( assignCoinsToChangeValues changeAddress excessCoin
          =<< splitOversizedValues changeValueOutputCoinPairs
      )
  where
  -- | Change `Value`s for all assets, where each change map is paired with a
  -- | corresponding coin from the original outputs.
  -- |
  -- | This array is sorted into ascending order of asset count, where empty
  -- | change `Value`s are all located at the start of the list.
  -- |
  -- | Taken from cardano-wallet:
  -- | https://github.com/input-output-hk/cardano-wallet/blob/4c2eb651d79212157a749d8e69a48fff30862e93/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1447
  changeValueOutputCoinPairs :: NonEmptyArray (Value /\ BigInt)
  changeValueOutputCoinPairs = outputCoins
    # NEArray.zip changeForAssets
    # NEArray.sortWith (AssetCount <<< fst)

  splitOversizedValues
    :: NonEmptyArray (Value /\ BigInt)
    -> BalanceTxM (NonEmptyArray (Value /\ BigInt))
  splitOversizedValues pairs =
    asksConstraints Constraints._maxChangeOutputTokenQuantity <#> case _ of
      Nothing -> pairs
      Just maxTokenQuantity ->
        unbundle <$>
          ( equipartitionValueWithTokenQuantityUpperBound maxTokenQuantity
              =<< map bundle pairs
          )
    where
    bundle :: Value /\ BigInt -> Value
    bundle (Value _ assets /\ coin) = mkValue (wrap coin) assets

    unbundle :: Value -> Value /\ BigInt
    unbundle (Value coin assets) = mkValue mempty assets /\ unwrap coin

  changeForAssets :: NonEmptyArray Value
  changeForAssets = foldr
    (NEArray.zipWith (<>) <<< makeChangeForAsset txOutputs)
    (NEArray.replicate (length txOutputs) mempty)
    excessAssets

  outputCoins :: NonEmptyArray BigInt
  outputCoins =
    NEArray.fromArray (valueToCoin' <<< _.amount <<< unwrap <$> txOutputs)
      ?? NEArray.singleton zero

  txOutputs :: Array TransactionOutput
  txOutputs = txBody ^. _outputs

  excessAssets :: Array (AssetClass /\ BigInt)
  excessAssets = Value.valueAssets excessValue

  excessCoin :: BigInt
  excessCoin = valueToCoin' excessValue

  excessValue :: Value
  excessValue = posValue $
    (inputValue <> mintValue txBody) `minus`
      (outputValue txBody <> minFeeValue txBody <> coinToValue certsFee)

  posValue :: Value -> Value
  posValue (Value (Coin coin) nonAdaAsset) =
    mkValue (Coin $ max coin zero) (posNonAdaAsset nonAdaAsset)

-- | Constructs change outputs for an asset.
-- |
-- | The given asset quantity is partitioned into a list of `Value`s that are
-- | proportional to the weights withing the given distribution. If the given
-- | asset quantity does not appear in the distribution, then it is equally
-- | partitioned into a list of the same length.
-- |
-- | The length of the output list is always the same as the length of the input
-- | list, and the sum of quantities is exactly equal to the asset quantity in
-- | the second argument.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/4c2eb651d79212157a749d8e69a48fff30862e93/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1729
makeChangeForAsset
  :: Array TransactionOutput -> (AssetClass /\ BigInt) -> NonEmptyArray Value
makeChangeForAsset txOutputs (assetClass /\ excess) =
  Value.assetToValue assetClass <$>
    partition excess weights ?? equipartition excess (length weights)
  where
  weights :: NonEmptyArray BigInt
  weights = NEArray.fromArray assetQuantities ?? NEArray.singleton one

  assetQuantities :: Array BigInt
  assetQuantities =
    txOutputs <#> Value.getAssetQuantity assetClass <<< _.amount <<< unwrap

-- | Constructs an array of ada change outputs based on the given distribution.
-- |
-- | The given ada amount is partitioned into a list of `Value`s that are
-- | proportional to the weights withing the given distribution. If the sum of
-- | weights in the given distribution is equal to zero, then the given excess
-- | coin is equally partitioned into a list of the same length.
-- |
-- | The length of the output list is always the same as the length of the input
-- | list, and the sum of its quantities is always exactly equal to the excess
-- | ada amount given as the second argument.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/4c2eb651d79212157a749d8e69a48fff30862e93/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1799
makeChangeForCoin :: NonEmptyArray BigInt -> BigInt -> NonEmptyArray Value
makeChangeForCoin weights excess =
  lovelaceValueOf <$>
    partition excess weights ?? equipartition excess (length weights)

-- | Assigns coin quantities to a list of pre-computed change `Value`s.
-- |
-- | Each pre-computed change `Value` must be paired with the original coin
-- | value of its corresponding output.
-- |
-- | This function:
-- |   - expects the list of pre-computed change `Value`s to be sorted in an
-- |     order that ensures all empty `Value`s are at the start of the list.
-- |
-- |   - attempts to assign a minimum ada quantity to every change `Value`, but
-- |     iteratively drops empty change `Value`s from the start of the list if
-- |     the amount of ada is insufficient to cover them all.
-- |
-- |   - continues dropping empty change maps from the start of the list until
-- |     it is possible to assign a minimum ada value to all remaining entries,
-- |     or until only one entry remains (in which case it assigns a minimum
-- |     ada value, even if the amount of ada is insufficient to cover it).
-- |
-- |   - assigns the minimum ada quantity to all non-empty change `Value`s, even
-- |     if `adaAvailable` is insufficient, does not fail.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/4c2eb651d79212157a749d8e69a48fff30862e93/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1631
assignCoinsToChangeValues
  :: Address
  -> BigInt
  -> NonEmptyArray (Value /\ BigInt)
  -> BalanceTxM (Array Value)
assignCoinsToChangeValues changeAddress adaAvailable pairsAtStart =
  changeValuesAtStart <#> \changeValues ->
    worker (adaRequiredAtStart changeValues) changeValues
  where
  worker :: BigInt -> NonEmptyArray ChangeValue -> Array Value
  worker adaRequired = NEArray.uncons >>> case _ of
    { head: x, tail: xs }
      | not (null xs) && adaAvailable < adaRequired && noTokens x ->
          worker (adaRequired - x.minCoin) (fromArrayUnsafe xs)

    { head: x, tail: xs } ->
      let
        changeValues :: NonEmptyArray ChangeValue
        changeValues = NEArray.cons' x xs

        adaRemaining :: BigInt
        adaRemaining = max zero (adaAvailable - adaRequired)

        changeValuesForOutputCoins :: NonEmptyArray Value
        changeValuesForOutputCoins =
          makeChangeForCoin (_.outputAda <$> changeValues) adaRemaining

        changeValuesWithMinCoins :: NonEmptyArray Value
        changeValuesWithMinCoins = assignMinCoin <$> changeValues
      in
        NEArray.toArray $
          NEArray.zipWith append changeValuesWithMinCoins
            changeValuesForOutputCoins
    where
    noTokens :: ChangeValue -> Boolean
    noTokens = null <<< Value.valueAssets <<< _.value

    assignMinCoin :: ChangeValue -> Value
    assignMinCoin { value: (Value _ assets), minCoin } =
      mkValue (wrap minCoin) assets

    fromArrayUnsafe :: forall (a :: Type). Array a -> NonEmptyArray a
    fromArrayUnsafe = unsafePartial fromJust <<< NEArray.fromArray

  adaRequiredAtStart :: NonEmptyArray ChangeValue -> BigInt
  adaRequiredAtStart = sum <<< map _.minCoin

  changeValuesAtStart :: BalanceTxM (NonEmptyArray ChangeValue)
  changeValuesAtStart =
    for pairsAtStart \(value /\ outputAda) ->
      { value, outputAda, minCoin: _ } <$> minCoinFor value

  minCoinFor :: Value -> BalanceTxM BigInt
  minCoinFor value = do
    let txOutput = mkChangeOutput changeAddress value
    coinsPerUtxoUnit <- askCoinsPerUtxoUnit
    ExceptT $ liftEffect $ utxoMinAdaValue coinsPerUtxoUnit txOutput
      <#> note UtxoMinAdaValueCalculationFailed

type ChangeValue = { value :: Value, outputAda :: BigInt, minCoin :: BigInt }

newtype AssetCount = AssetCount Value

derive instance Newtype AssetCount _
derive newtype instance Eq AssetCount

instance Ord AssetCount where
  compare = compare `on` (Array.length <<< Value.valueAssets <<< unwrap)

mkChangeOutput :: Address -> Value -> TransactionOutput
mkChangeOutput changeAddress amount = wrap
  { address: changeAddress, amount, datum: NoOutputDatum, scriptRef: Nothing }

--------------------------------------------------------------------------------
-- Getters for various `Value`s
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

logBalancerState :: String -> UtxoMap -> BalancerState -> BalanceTxM Unit
logBalancerState message utxos { transaction, changeOutputs } =
  logTransactionWithChange message utxos (Just changeOutputs) transaction

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

