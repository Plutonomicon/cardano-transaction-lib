module Ctl.Internal.BalanceTx
  ( balanceTxWithConstraints
  ) where

import Prelude

import Contract.Log (logWarn')
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Logger.Class (info) as Logger
import Control.Monad.Reader (asks)
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
import Ctl.Internal.BalanceTx.Collateral.Select (selectCollateral)
import Ctl.Internal.BalanceTx.Constraints
  ( BalanceTxConstraintsBuilder
  , _collateralUtxos
  , _nonSpendableInputs
  )
import Ctl.Internal.BalanceTx.Constraints
  ( _changeAddress
  , _changeDatum
  , _maxChangeOutputTokenQuantity
  , _nonSpendableInputs
  , _selectionStrategy
  , _srcAddresses
  ) as Constraints
import Ctl.Internal.BalanceTx.Error
  ( BalanceTxError
      ( InsufficientCollateralUtxos
      , UtxoLookupFailedFor
      , UtxoMinAdaValueCalculationFailed
      , ReindexRedeemersError
      , CouldNotGetUtxos
      , CouldNotGetCollateral
      , CouldNotGetChangeAddress
      )
  )
import Ctl.Internal.BalanceTx.ExUnitsAndMinFee
  ( evalExUnitsAndMinFee
  , finalizeTransaction
  )
import Ctl.Internal.BalanceTx.RedeemerIndex
  ( attachIndexedRedeemers
  , indexRedeemers
  , mkRedeemersContext
  )
import Ctl.Internal.BalanceTx.Sync (syncBackendWithWallet)
import Ctl.Internal.BalanceTx.Types
  ( BalanceTxM
  , FinalizedTransaction
  , askCip30Wallet
  , askCoinsPerUtxoUnit
  , askNetworkId
  , asksConstraints
  , liftContract
  , liftEitherContract
  , withBalanceTxConstraints
  )
import Ctl.Internal.BalanceTx.UnattachedTx
  ( EvaluatedTx
  , UnindexedTx
  , _transaction
  , indexTx
  )
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
  , _plutusScripts
  , _referenceInputs
  , _withdrawals
  , _witnessSet
  , pprintUtxoMap
  )
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( transactionUnspentOutputsToUtxoMap
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
  , pprintValue
  , valueToCoin'
  )
import Ctl.Internal.Cardano.Types.Value as Value
import Ctl.Internal.CoinSelection.UtxoIndex (UtxoIndex, buildUtxoIndex)
import Ctl.Internal.Contract (getProtocolParameters)
import Ctl.Internal.Contract.Monad (Contract, filterLockedUtxos, getQueryHandle)
import Ctl.Internal.Contract.Wallet
  ( getChangeAddress
  , getWalletCollateral
  , getWalletUtxos
  ) as Wallet
import Ctl.Internal.Helpers (liftEither, pprintTagSet, (??))
import Ctl.Internal.Partition (equipartition, partition)
import Ctl.Internal.Plutus.Conversion (fromPlutusUtxoMap)
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Types.OutputDatum (OutputDatum(NoOutputDatum, OutputDatum))
import Ctl.Internal.Types.ProtocolParameters
  ( ProtocolParameters(ProtocolParameters)
  )
import Ctl.Internal.Types.Scripts
  ( Language(PlutusV1)
  , PlutusScript(PlutusScript)
  )
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty
  ( fromArray
  , replicate
  , singleton
  , sortWith
  , toArray
  , uncons
  , zip
  , zipWith
  ) as NEArray
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Either (Either, hush, note)
import Data.Foldable (fold, foldMap, foldr, length, null, sum)
import Data.Function (on)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~), (?~))
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag (fromArray) as TagSet
import Data.Map (Map)
import Data.Map (empty, filterKeys, insert, lookup, toUnfoldable, union) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toInt) as UInt
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import JS.BigInt (BigInt)
import JS.BigInt (toString) as BigInt

-- | Balances an unbalanced transaction using the specified balancer
-- | constraints.
balanceTxWithConstraints
  :: UnindexedTx
  -> Map TransactionInput TransactionOutput
  -> BalanceTxConstraintsBuilder
  -> Contract (Either BalanceTxError FinalizedTransaction)
balanceTxWithConstraints transaction extraUtxos constraintsBuilder = do
  pparams <- getProtocolParameters

  withBalanceTxConstraints constraintsBuilder $ runExceptT do
    let
      depositValuePerCert = (unwrap pparams).stakeAddressDeposit
      certsFee = getStakingBalance (transaction.transaction)
        depositValuePerCert

    changeAddress <- getChangeAddress

    mbSrcAddrs <- asksConstraints Constraints._srcAddresses

    changeDatum' <- asksConstraints Constraints._changeDatum

    utxos <- liftEitherContract do
      case mbSrcAddrs of
        -- Use wallet UTxOs.
        Nothing -> do
          whenM
            ( asks $ _.synchronizationParams
                >>> _.syncBackendWithWallet
                >>> _.beforeBalancing
            )
            syncBackendWithWallet
          note CouldNotGetUtxos <$> do
            Wallet.getWalletUtxos
        -- Use UTxOs from source addresses
        Just srcAddrs -> do
          queryHandle <- getQueryHandle
          -- Even though some of the addresses may be controlled by the wallet,
          -- we can't query the wallet for available UTxOs, because there's no
          -- way to tell it to return UTxOs only from specific subset of the
          -- addresses controlled by a CIP-30 wallet.
          -- `utxosAt` calls are expensive when there are a lot of addresses to
          -- check.
          parTraverse (queryHandle.utxosAt >>> liftAff >>> map hush) srcAddrs
            <#> traverse (note CouldNotGetUtxos)
              >>> map (foldr Map.union Map.empty) -- merge all utxos into one map

    unbalancedCollTx <- transactionWithNetworkId >>=
      if Array.null (transaction # _.redeemers)
      -- Don't set collateral if tx doesn't contain phase-2 scripts:
      then pure
      else setTransactionCollateral changeAddress
    let
      allUtxos :: UtxoMap
      allUtxos =
        -- Combine utxos at the user address and those from any scripts
        -- involved with the contract in the unbalanced transaction:
        utxos `Map.union` extraUtxos

    availableUtxos <- liftContract $ filterLockedUtxos allUtxos

    Logger.info (pprintUtxoMap allUtxos) "balanceTxWithConstraints: all UTxOs"
    Logger.info (pprintUtxoMap availableUtxos)
      "balanceTxWithConstraints: available UTxOs"

    selectionStrategy <- asksConstraints Constraints._selectionStrategy

    -- Reindex redeemers and update transaction
    reindexedRedeemers <- liftEither $ lmap ReindexRedeemersError $
      indexRedeemers (mkRedeemersContext unbalancedCollTx) transaction.redeemers
    let
      reindexedTransaction = transaction
        { transaction = attachIndexedRedeemers reindexedRedeemers
            unbalancedCollTx
        }

    -- Balance and finalize the transaction:
    runBalancer
      { strategy: selectionStrategy
      , transaction: reindexedTransaction
      , changeAddress
      , changeDatum: fromMaybe NoOutputDatum changeDatum'
      , allUtxos
      , utxos: availableUtxos
      , certsFee
      }
  where
  getChangeAddress :: BalanceTxM Address
  getChangeAddress =
    liftMaybe CouldNotGetChangeAddress
      =<< maybe (liftContract Wallet.getChangeAddress) (pure <<< Just)
      =<< asksConstraints Constraints._changeAddress

  transactionWithNetworkId :: BalanceTxM Transaction
  transactionWithNetworkId = do
    networkId <- maybe askNetworkId pure
      (transaction ^. _transaction <<< _body <<< _networkId)
    pure (transaction.transaction # _body <<< _networkId ?~ networkId)

setTransactionCollateral :: Address -> Transaction -> BalanceTxM Transaction
setTransactionCollateral changeAddr transaction = do
  nonSpendableSet <- asksConstraints _nonSpendableInputs
  mbCollateralUtxos <- asksConstraints _collateralUtxos
  -- We must filter out UTxOs that are set as non-spendable in the balancer
  -- constraints
  let isSpendable = not <<< flip Set.member nonSpendableSet
  collateral <- case mbCollateralUtxos of
    -- if no collateral utxos are specified, use the wallet, but filter
    -- the unspendable ones
    Nothing -> do
      let isSpendableUtxo = isSpendable <<< _.input <<< unwrap
      { yes: spendableUtxos, no: filteredUtxos } <-
        Array.partition isSpendableUtxo <$> do
          liftEitherContract $ note CouldNotGetCollateral <$>
            Wallet.getWalletCollateral
      when (not $ Array.null filteredUtxos) do
        logWarn' $ pprintTagSet
          "Some of the collateral UTxOs returned by the wallet were marked as non-spendable and ignored"
          (pprintUtxoMap (transactionUnspentOutputsToUtxoMap filteredUtxos))
      pure spendableUtxos
    -- otherwise, get all the utxos, filter out unspendable, and select
    -- collateral using internal algo, that is also used in KeyWallet
    Just utxoMap -> do
      ProtocolParameters params <- liftContract getProtocolParameters
      networkId <- askNetworkId
      let
        coinsPerUtxoUnit = params.coinsPerUtxoUnit
        maxCollateralInputs = UInt.toInt $ params.maxCollateralInputs
        utxoMap' = fromPlutusUtxoMap networkId $ Map.filterKeys isSpendable
          utxoMap
      mbCollateral <- liftEffect $ map Array.fromFoldable <$>
        selectCollateral coinsPerUtxoUnit maxCollateralInputs utxoMap'
      liftEither $ note (InsufficientCollateralUtxos utxoMap') mbCollateral
  addTxCollateralReturn collateral (addTxCollateral collateral transaction)
    changeAddr

--------------------------------------------------------------------------------
-- Balancing Algorithm
--------------------------------------------------------------------------------

type BalancerParams =
  { strategy :: SelectionStrategy
  , transaction :: UnindexedTx
  , changeAddress :: Address
  , changeDatum :: OutputDatum
  , allUtxos :: UtxoMap
  , utxos :: UtxoMap
  , certsFee :: Coin
  }

type BalancerState tx =
  { transaction :: tx
  , leftoverUtxos :: UtxoIndex
  , changeOutputs :: Array TransactionOutput
  , minFee :: BigInt
  }

initBalancerState
  :: UnindexedTx
  -> UtxoMap
  -> BalancerState UnindexedTx
initBalancerState transaction =
  buildUtxoIndex >>>
    { transaction, leftoverUtxos: _, changeOutputs: mempty, minFee: zero }

data BalancerStep
  = PrebalanceTx (BalancerState UnindexedTx)
  | BalanceChangeAndMinFee (BalancerState UnindexedTx)

runBalancer :: BalancerParams -> BalanceTxM FinalizedTransaction
runBalancer p = do
  utxos <- partitionAndFilterUtxos
  transaction <- addLovelacesToTransactionOutputs p.transaction
  mainLoop (initBalancerState transaction utxos.spendable)
  where
  -- We check if the transaction uses a plutusv1 script, so that we can filter
  -- out utxos which use plutusv2 features if so.
  txHasPlutusV1 :: Boolean
  txHasPlutusV1 =
    case p.transaction ^. _transaction <<< _witnessSet <<< _plutusScripts of
      Just scripts -> flip Array.any scripts case _ of
        PlutusScript (_ /\ PlutusV1) -> true
        _ -> false
      Nothing -> false

  partitionAndFilterUtxos
    :: BalanceTxM { spendable :: UtxoMap, invalidInContext :: UtxoMap }
  partitionAndFilterUtxos = do
    isCip30 <- isJust <$> askCip30Wallet
    -- Get collateral inputs to mark them as unspendable.
    -- Some CIP-30 wallets don't allow to sign Txs that spend it.
    nonSpendableCollateralInputs <-
      if isCip30 then
        liftContract $ Wallet.getWalletCollateral <#>
          fold >>> map (unwrap >>> _.input) >>> Set.fromFoldable
      else mempty
    asksConstraints Constraints._nonSpendableInputs <#>
      append nonSpendableCollateralInputs >>>
        \nonSpendableInputs ->
          foldr
            ( \(oref /\ output) acc ->
                let
                  hasInlineDatum :: Boolean
                  hasInlineDatum = case (unwrap output).datum of
                    OutputDatum _ -> true
                    _ -> false

                  hasScriptRef :: Boolean
                  hasScriptRef = isJust (unwrap output).scriptRef

                  spendable :: Boolean
                  spendable = not $ Set.member oref nonSpendableInputs ||
                    Set.member oref
                      ( p.transaction ^. _transaction <<< _body <<<
                          _referenceInputs
                      )

                  validInContext :: Boolean
                  validInContext = not $ txHasPlutusV1 &&
                    (hasInlineDatum || hasScriptRef)
                in
                  case spendable, validInContext of
                    true, true -> acc
                      { spendable = Map.insert oref output acc.spendable }
                    true, false -> acc
                      { invalidInContext = Map.insert oref output
                          acc.invalidInContext
                      }
                    _, _ -> acc
            )
            { spendable: Map.empty
            , invalidInContext: Map.empty
            }
            (Map.toUnfoldable p.utxos :: Array _)

  mainLoop :: BalancerState UnindexedTx -> BalanceTxM FinalizedTransaction
  mainLoop = worker <<< PrebalanceTx
    where
    worker :: BalancerStep -> BalanceTxM FinalizedTransaction
    worker (PrebalanceTx state) = do
      logBalancerState "Pre-balancing (Stage 1)" p.allUtxos state
      prebalanceTx state >>= runNextBalancerStep
    worker (BalanceChangeAndMinFee state@{ transaction, minFee, leftoverUtxos }) =
      do
        logBalancerState "Balancing change and fees (Stage 2)" p.allUtxos state
        { transaction: evaluatedTx, minFee: newMinFee } <- evaluateTx state
        case newMinFee <= minFee of
          true -> do
            logTransaction "Balanced transaction (Done)" p.allUtxos
              evaluatedTx.transaction
            if Set.isEmpty $ evaluatedTx.transaction ^. _body <<< _inputs then
              do
                selectionState <-
                  performMultiAssetSelection p.strategy leftoverUtxos
                    (lovelaceValueOf one)
                runNextBalancerStep $ state
                  { transaction = transaction #
                      _transaction <<< _body <<< _inputs %~ Set.union
                        (selectedInputs selectionState)
                  , leftoverUtxos =
                      selectionState ^. _leftoverUtxos
                  }
            else do
              logTransaction "Balanced transaction (Done)" p.allUtxos
                transaction.transaction
              finalizeTransaction evaluatedTx p.allUtxos
          false ->
            runNextBalancerStep $ state
              { transaction = transaction
                  # _transaction <<< _body <<< _fee .~ Coin newMinFee
              , minFee = newMinFee
              }

    -- | Determines which balancing step will be performed next.
    -- |
    -- | If the transaction remains unbalanced (i.e. `requiredValue != mempty`)
    -- | after generation of change, the first balancing step `PrebalanceTx`
    -- | is performed, otherwise we proceed to `BalanceChangeAndMinFee`.
    runNextBalancerStep
      :: BalancerState UnindexedTx -> BalanceTxM FinalizedTransaction
    runNextBalancerStep state@{ transaction } = do
      let txBody = transaction ^. _transaction <<< _body
      inputValue <- except $ getInputValue p.allUtxos txBody
      ownWalletAddresses <- asks _.ownAddresses
      changeOutputs <- makeChange ownWalletAddresses p.changeAddress
        p.changeDatum
        inputValue
        p.certsFee
        txBody

      requiredValue <-
        except $ getRequiredValue p.certsFee p.allUtxos
          $ setTxChangeOutputs changeOutputs transaction ^. _transaction <<<
              _body

      worker $
        if requiredValue == mempty then BalanceChangeAndMinFee $ state
          { changeOutputs = changeOutputs, transaction = transaction }
        else PrebalanceTx $ state { changeOutputs = changeOutputs }

    -- | Selects a combination of unspent transaction outputs from the wallet's
    -- | utxo set so that the total input value is sufficient to cover all
    -- | transaction outputs, including generated change and min fee.
    prebalanceTx
      :: BalancerState UnindexedTx -> BalanceTxM (BalancerState UnindexedTx)
    prebalanceTx state@{ transaction, changeOutputs, leftoverUtxos } =
      performCoinSelection <#> \selectionState -> state
        { transaction =
            ( transaction #
                _transaction <<< _body <<< _inputs %~
                  Set.union (selectedInputs selectionState)
            )
        , leftoverUtxos =
            selectionState ^. _leftoverUtxos
        }
      where
      performCoinSelection :: BalanceTxM SelectionState
      performCoinSelection =
        let
          txBody :: TxBody
          txBody = setTxChangeOutputs changeOutputs transaction ^. _transaction
            <<< _body
        in
          except (getRequiredValue p.certsFee p.allUtxos txBody)
            >>= performMultiAssetSelection p.strategy leftoverUtxos

    -- | Calculates execution units for each script in the transaction and sets
    -- | min fee.
    -- |
    -- | The transaction must be pre-balanced before evaluating execution units,
    -- | since this pre-condition is sometimes required for successfull script
    -- | execution during transaction evaluation.
    evaluateTx
      :: BalancerState UnindexedTx -> BalanceTxM (BalancerState EvaluatedTx)
    evaluateTx state@{ transaction, changeOutputs } = do
      let
        prebalancedTx :: UnindexedTx
        prebalancedTx = setTxChangeOutputs changeOutputs transaction
      indexedTx <- liftEither $ lmap ReindexRedeemersError $ indexTx
        prebalancedTx
      evaluatedTx /\ minFee <- evalExUnitsAndMinFee indexedTx p.allUtxos
      pure $ state { transaction = evaluatedTx, minFee = minFee }

-- | For each transaction output, if necessary, adds some number of lovelaces
-- | to cover the utxo min-ada-value requirement.
addLovelacesToTransactionOutputs
  :: UnindexedTx -> BalanceTxM UnindexedTx
addLovelacesToTransactionOutputs transaction =
  map
    ( \txOutputs -> transaction #
        _transaction <<< _body <<< _outputs .~ txOutputs
    ) $
    traverse addLovelacesToTransactionOutput
      (transaction ^. _transaction <<< _body <<< _outputs)

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
  :: Array TransactionOutput -> UnindexedTx -> UnindexedTx
setTxChangeOutputs outputs tx =
  tx # _transaction <<< _body <<< _outputs %~ flip append outputs

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
-- |
-- | Differences from cardano-wallet:
-- |
-- | - We only consider outputs that go back to our wallet when deciding on
-- | the number of desired outputs for change generation. See
-- | https://github.com/Plutonomicon/cardano-transaction-lib/issues/1530
makeChange
  :: Set Address
  -> Address
  -> OutputDatum
  -> Value
  -> Coin
  -> TxBody
  -> BalanceTxM (Array TransactionOutput)
makeChange
  ownWalletAddresses
  changeAddress
  changeDatum
  inputValue
  certsFee
  txBody =
  -- Always generate change when a transaction has no outputs to avoid issues
  -- with transaction confirmation:
  -- FIXME: https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
  if excessValue == mempty && (txBody ^. _outputs) /= mempty then pure mempty
  else
    map (mkChangeOutput changeAddress changeDatum) <$>
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
    where
    outputCoins :: NonEmptyArray BigInt
    outputCoins =
      NEArray.fromArray
        (valueToCoin' <<< _.amount <<< unwrap <$> ownAddressOutputs)
        ?? NEArray.singleton zero

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

  -- outputs belonging to one of the wallet's addresses.
  ownAddressOutputs :: Array TransactionOutput
  ownAddressOutputs = Array.filter isOwnWalletAddress $ txBody ^. _outputs
    where
    isOwnWalletAddress = unwrap >>> _.address >>> flip Set.member
      ownWalletAddresses

  changeForAssets :: NonEmptyArray Value
  changeForAssets = foldr
    (NEArray.zipWith (<>) <<< makeChangeForAsset ownAddressOutputs)
    (NEArray.replicate (length ownAddressOutputs) mempty)
    excessAssets

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
  :: Array TransactionOutput
  -> (AssetClass /\ BigInt)
  -> NonEmptyArray Value
makeChangeForAsset ownAddressOutputs (assetClass /\ excess) =
  Value.assetToValue assetClass <$>
    partition excess weights ?? equipartition excess (length weights)
  where
  weights :: NonEmptyArray BigInt
  weights = NEArray.fromArray assetQuantities ?? NEArray.singleton one

  assetQuantities :: Array BigInt
  assetQuantities =
    ownAddressOutputs <#> Value.getAssetQuantity assetClass <<< _.amount <<<
      unwrap

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
  worker adaRequired changeValues = changeValues # NEArray.uncons >>> case _ of
    { head: x, tail }
      | Just xs <- NEA.fromArray tail
      , adaAvailable < adaRequired && noTokens x ->
          worker (adaRequired - x.minCoin) xs
    _ ->
      let
        adaRemaining :: BigInt
        adaRemaining = max zero (adaAvailable - adaRequired)

        changeValuesForOutputCoins :: NonEmptyArray Value
        changeValuesForOutputCoins =
          let
            weights = _.outputAda <$> changeValues
          in
            makeChangeForCoin weights adaRemaining

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

  adaRequiredAtStart :: NonEmptyArray ChangeValue -> BigInt
  adaRequiredAtStart = sum <<< map _.minCoin

  changeValuesAtStart :: BalanceTxM (NonEmptyArray ChangeValue)
  changeValuesAtStart =
    for pairsAtStart \(value /\ outputAda) ->
      { value, outputAda, minCoin: _ } <$> minCoinFor value

  minCoinFor :: Value -> BalanceTxM BigInt
  minCoinFor value = do
    let
      -- NOTE: Datum here doesn't matter, we deconstruct UTxO immediately anyway
      txOutput = mkChangeOutput changeAddress NoOutputDatum value
    coinsPerUtxoUnit <- askCoinsPerUtxoUnit
    ExceptT $ liftEffect $ utxoMinAdaValue coinsPerUtxoUnit txOutput
      <#> note UtxoMinAdaValueCalculationFailed

type ChangeValue = { value :: Value, outputAda :: BigInt, minCoin :: BigInt }

newtype AssetCount = AssetCount Value

derive instance Newtype AssetCount _
derive newtype instance Eq AssetCount

instance Ord AssetCount where
  compare = compare `on` (Array.length <<< Value.valueAssets <<< unwrap)

mkChangeOutput :: Address -> OutputDatum -> Value -> TransactionOutput
mkChangeOutput changeAddress datum amount = wrap
  { address: changeAddress, amount, datum, scriptRef: Nothing }

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

logBalancerState
  :: forall rest
   . String
  -> UtxoMap
  -> BalancerState { transaction :: Transaction | rest }
  -> BalanceTxM Unit
logBalancerState message utxos { transaction: { transaction }, changeOutputs } =
  logTransactionWithChange message utxos (Just changeOutputs) transaction

logTransaction
  :: String -> UtxoMap -> Transaction -> BalanceTxM Unit
logTransaction message utxos =
  logTransactionWithChange message utxos Nothing

logTransactionWithChange
  :: String
  -> UtxoMap
  -> Maybe (Array TransactionOutput)
  -> Transaction
  -> BalanceTxM Unit
logTransactionWithChange message utxos mChangeOutputs tx =
  let
    txBody :: TxBody
    txBody = tx ^. _body

    outputValuesTagSet :: Maybe (Array TransactionOutput) -> Array TagSet
    outputValuesTagSet Nothing =
      [ "Output Value" `tagSetTag` pprintValue (outputValue txBody) ]
    outputValuesTagSet (Just changeOutputs) =
      [ "Output Value without change" `tagSetTag` pprintValue
          (outputValue txBody)
      , "Change Value" `tagSetTag` pprintValue (foldMap getAmount changeOutputs)
      ]

    transactionInfo :: Value -> TagSet
    transactionInfo inputValue =
      TagSet.fromArray $
        [ "Input Value" `tagSetTag` pprintValue inputValue
        , "Mint Value" `tagSetTag` pprintValue (mintValue txBody)
        , "Fees" `tag` BigInt.toString (unwrap (txBody ^. _fee))
        ] <> outputValuesTagSet mChangeOutputs
  in
    except (getInputValue utxos txBody)
      >>= (flip Logger.info (message <> ":") <<< transactionInfo)
