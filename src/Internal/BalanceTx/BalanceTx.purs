module Ctl.Internal.BalanceTx
  ( balanceTxWithConstraints
  ) where

import Prelude

import Cardano.Transaction.Edit (editTransaction)
import Cardano.Types
  ( AssetClass(AssetClass)
  , Certificate
      ( StakeDeregistration
      , StakeRegistration
      , StakeRegDelegCert
      , VoteRegDelegCert
      , StakeVoteRegDelegCert
      , RegDrepCert
      , UnregDrepCert
      )
  , Coin(Coin)
  , Language(PlutusV1)
  , PlutusScript(PlutusScript)
  , Transaction
  , TransactionBody
  , TransactionOutput
  , UtxoMap
  , Value(Value)
  , _amount
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
  )
import Cardano.Types.Address (Address)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Coin as Coin
import Cardano.Types.OutputDatum (OutputDatum(OutputDatum))
import Cardano.Types.TransactionBody (_votingProposals)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionUnspentOutput as TransactionUnspentOutputs
import Cardano.Types.TransactionWitnessSet (_redeemers)
import Cardano.Types.UtxoMap (pprintUtxoMap)
import Cardano.Types.Value (getMultiAsset, mkValue, pprintValue)
import Cardano.Types.Value as Value
import Contract.Log (logInfo', logWarn')
import Control.Monad.Except (class MonadError)
import Control.Monad.Except.Trans (except, runExceptT)
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
      ( CouldNotGetUtxos
      , CouldNotGetCollateral
      , InsufficientCollateralUtxos
      , NumericOverflowError
      , UtxoLookupFailedFor
      )
  )
import Ctl.Internal.BalanceTx.ExUnitsAndMinFee
  ( evalExUnitsAndMinFee
  , finalizeTransaction
  )
import Ctl.Internal.BalanceTx.Sync (isCip30Wallet, syncBackendWithWallet)
import Ctl.Internal.BalanceTx.Types
  ( BalanceTxM
  , askCoinsPerUtxoUnit
  , askNetworkId
  , asksConstraints
  , liftContract
  , liftEitherContract
  , withBalancerConstraints
  )
import Ctl.Internal.BalanceTx.UtxoMinAda (utxoMinAdaValue)
import Ctl.Internal.CoinSelection.UtxoIndex (UtxoIndex, buildUtxoIndex)
import Ctl.Internal.Contract (getProtocolParameters)
import Ctl.Internal.Contract.Monad (Contract, filterLockedUtxos, getQueryHandle)
import Ctl.Internal.Contract.Wallet
  ( getChangeAddress
  , getWalletCollateral
  , getWalletUtxos
  ) as Wallet
import Ctl.Internal.Helpers (liftEither, pprintTagSet, unsafeFromJust, (??))
import Ctl.Internal.Partition
  ( equipartition
  , equipartitionValueWithTokenQuantityUpperBound
  , partition
  )
import Ctl.Internal.Types.ProtocolParameters
  ( ProtocolParameters(ProtocolParameters)
  )
import Ctl.Internal.Types.Val (Val(Val), pprintVal)
import Ctl.Internal.Types.Val as Val
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
import Data.Bitraversable (ltraverse)
import Data.Either (Either, hush, note)
import Data.Foldable (fold, foldMap, foldr, length, null, sum)
import Data.Lens (view)
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~), (?~))
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag (fromArray) as TagSet
import Data.Map (Map)
import Data.Map
  ( empty
  , filter
  , insert
  , isEmpty
  , lookup
  , singleton
  , toUnfoldable
  , union
  ) as Map
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toInt) as UInt
import Effect.Aff.Class (liftAff)
import JS.BigInt (BigInt)
import Partial.Unsafe (unsafePartial)

-- | Balances an unbalanced transaction using the specified balancer
-- | constraints.
balanceTxWithConstraints
  :: Transaction
  -> Map TransactionInput TransactionOutput
  -> BalanceTxConstraintsBuilder
  -> Contract (Either BalanceTxError Transaction)
balanceTxWithConstraints transaction extraUtxos constraintsBuilder =
  withBalancerConstraints constraintsBuilder $ runExceptT do
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
            do
              logInfo' "balanceTxWithConstraints: syncBackendWithWallet"
              syncBackendWithWallet
          logInfo' "balanceTxWithConstraints: Wallet.getWalletUtxos"
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
      if Array.null (transaction ^. _witnessSet <<< _redeemers)
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

    pparams <- liftContract getProtocolParameters

    -- Balance and finalize the transaction:
    runBalancer
      { strategy: selectionStrategy
      , transaction: unbalancedCollTx
      , changeAddress
      , changeDatum: changeDatum'
      , allUtxos
      , utxos: availableUtxos
      , miscFee: getCertsBalance transaction pparams + getProposalsBalance
          transaction
      }
  where
  getChangeAddress :: BalanceTxM Address
  getChangeAddress = maybe (liftContract Wallet.getChangeAddress) pure
    =<< asksConstraints Constraints._changeAddress

  transactionWithNetworkId :: BalanceTxM Transaction
  transactionWithNetworkId = do
    networkId <- maybe askNetworkId pure
      (transaction ^. _body <<< _networkId)
    pure (transaction # _body <<< _networkId ?~ networkId)

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
          (pprintUtxoMap (TransactionUnspentOutputs.toUtxoMap filteredUtxos))
      pure spendableUtxos
    -- otherwise, get all the utxos, filter out unspendable, and select
    -- collateral using internal algo, that is also used in KeyWallet
    Just utxoMap -> do
      ProtocolParameters params <- liftContract getProtocolParameters
      let
        maxCollateralInputs = UInt.toInt $ params.maxCollateralInputs
        mbCollateral =
          Array.fromFoldable <$>
            selectCollateral params.coinsPerUtxoByte maxCollateralInputs utxoMap
      liftEither $ note (InsufficientCollateralUtxos utxoMap) mbCollateral
  addTxCollateralReturn collateral (addTxCollateral collateral transaction)
    changeAddr

--------------------------------------------------------------------------------
-- Balancing Algorithm
--------------------------------------------------------------------------------

type BalancerParams =
  { strategy :: SelectionStrategy
  , transaction :: Transaction
  , changeAddress :: Address
  , changeDatum :: Maybe OutputDatum
  , allUtxos :: UtxoMap
  , utxos :: UtxoMap
  , miscFee :: BigInt -- can be negative (deregistration)
  }

-- TODO: remove the parameter
type BalancerState tx =
  { transaction :: tx
  , leftoverUtxos :: UtxoIndex
  , changeOutputs :: Array TransactionOutput
  , minFee :: Coin
  }

initBalancerState
  :: Transaction
  -> UtxoMap
  -> BalancerState Transaction
initBalancerState transaction =
  buildUtxoIndex >>>
    { transaction, leftoverUtxos: _, changeOutputs: mempty, minFee: Coin.zero }

data BalancerStep
  = PrebalanceTx (BalancerState Transaction)
  | BalanceChangeAndMinFee (BalancerState Transaction)

runBalancer :: BalancerParams -> BalanceTxM Transaction
runBalancer p = do
  utxos <- partitionAndFilterUtxos
  transaction <- addLovelacesToTransactionOutputs p.transaction
  mainLoop (initBalancerState transaction utxos.spendable)
  where
  referenceInputSet = Set.fromFoldable $ p.transaction ^. _body
    <<< _referenceInputs

  -- We check if the transaction uses a plutusv1 script, so that we can filter
  -- out utxos which use plutusv2 features if so.
  txHasPlutusV1 :: Boolean
  txHasPlutusV1 =
    case p.transaction ^. _witnessSet <<< _plutusScripts of
      [] -> false
      scripts -> flip Array.any scripts case _ of
        PlutusScript (_ /\ PlutusV1) -> true
        _ -> false

  partitionAndFilterUtxos
    :: BalanceTxM { spendable :: UtxoMap, invalidInContext :: UtxoMap }
  partitionAndFilterUtxos = do
    isCip30 <- liftContract $ isCip30Wallet
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
                    Just (OutputDatum _) -> true
                    _ -> false

                  hasScriptRef :: Boolean
                  hasScriptRef = isJust (unwrap output).scriptRef

                  spendable :: Boolean
                  spendable = not $ Set.member oref nonSpendableInputs ||
                    Set.member oref referenceInputSet

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

  mainLoop :: BalancerState Transaction -> BalanceTxM Transaction
  mainLoop = worker <<< PrebalanceTx
    where
    worker :: BalancerStep -> BalanceTxM Transaction
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
              evaluatedTx
            if Array.null $ evaluatedTx ^. _body <<< _inputs then
              do
                selectionState <-
                  performMultiAssetSelection p.strategy leftoverUtxos
                    (Val one Map.empty)
                runNextBalancerStep $ state
                  { transaction = flip editTransaction transaction $
                      _body <<< _inputs %~ appendInputs
                        (Array.fromFoldable $ selectedInputs selectionState)
                  , leftoverUtxos =
                      selectionState ^. _leftoverUtxos
                  }
            else do
              finalizeTransaction evaluatedTx p.allUtxos
          false ->
            runNextBalancerStep $ state
              { transaction = transaction
                  # _body <<< _fee .~ newMinFee
              , minFee = newMinFee
              }

    -- | Determines which balancing step will be performed next.
    -- |
    -- | If the transaction remains unbalanced (i.e. `requiredValue != mempty`)
    -- | after generation of change, the first balancing step `PrebalanceTx`
    -- | is performed, otherwise we proceed to `BalanceChangeAndMinFee`.
    runNextBalancerStep
      :: BalancerState Transaction -> BalanceTxM Transaction
    runNextBalancerStep state@{ transaction } = do
      let txBody = transaction ^. _body
      inputValue <- except $ getInputVal p.allUtxos txBody
      ownWalletAddresses <- asks _.ownAddresses
      inputValue' <- liftValue inputValue
      changeOutputs <- makeChange ownWalletAddresses p.changeAddress
        p.changeDatum
        inputValue'
        p.miscFee
        txBody

      requiredValue <-
        except $ getRequiredValue p.miscFee p.allUtxos
          $ setTxChangeOutputs changeOutputs transaction ^. _body

      worker $
        if requiredValue == mempty then BalanceChangeAndMinFee $ state
          { changeOutputs = changeOutputs, transaction = transaction }
        else PrebalanceTx $ state { changeOutputs = changeOutputs }

    -- | Selects a combination of unspent transaction outputs from the wallet's
    -- | utxo set so that the total input value is sufficient to cover all
    -- | transaction outputs, including generated change and min fee.
    prebalanceTx
      :: BalancerState Transaction -> BalanceTxM (BalancerState Transaction)
    prebalanceTx state@{ transaction, changeOutputs, leftoverUtxos } =
      performCoinSelection <#> \selectionState -> state
        { transaction =
            ( flip editTransaction transaction $
                _body <<< _inputs %~
                  appendInputs
                    (Array.fromFoldable $ selectedInputs selectionState)
            )
        , leftoverUtxos =
            selectionState ^. _leftoverUtxos
        }
      where
      performCoinSelection :: BalanceTxM SelectionState
      performCoinSelection = do
        let
          txBody :: TransactionBody
          txBody = setTxChangeOutputs changeOutputs transaction ^. _body
        except (getRequiredValue p.miscFee p.allUtxos txBody)
          >>= performMultiAssetSelection p.strategy leftoverUtxos

    -- | Calculates execution units for each script in the transaction and sets
    -- | min fee.
    -- |
    -- | The transaction must be pre-balanced before evaluating execution units,
    -- | since this pre-condition is sometimes required for successfull script
    -- | execution during transaction evaluation.
    evaluateTx
      :: BalancerState Transaction -> BalanceTxM (BalancerState Transaction)
    evaluateTx state@{ transaction, changeOutputs } = do
      let
        prebalancedTx :: Transaction
        prebalancedTx = setTxChangeOutputs changeOutputs transaction
      evaluatedTx /\ minFee <- evalExUnitsAndMinFee prebalancedTx p.allUtxos
      pure $ state { transaction = evaluatedTx, minFee = minFee }

-- | For each transaction output, if necessary, adds some number of lovelaces
-- | to cover the utxo min-ada-value requirement.
addLovelacesToTransactionOutputs
  :: Transaction -> BalanceTxM Transaction
addLovelacesToTransactionOutputs transaction =
  map
    ( \txOutputs -> transaction #
        _body <<< _outputs .~ txOutputs
    ) $
    traverse addLovelacesToTransactionOutput
      (transaction ^. _body <<< _outputs)

addLovelacesToTransactionOutput
  :: TransactionOutput -> BalanceTxM TransactionOutput
addLovelacesToTransactionOutput txOutput = do
  coinsPerUtxoUnit <- askCoinsPerUtxoUnit
  let
    txOutputMinAda = Coin $ utxoMinAdaValue coinsPerUtxoUnit txOutput
    txOutputRec = unwrap txOutput

    txOutputValue :: Value
    txOutputValue = txOutputRec.amount

    newCoin :: Coin
    newCoin = max (Value.getCoin txOutputValue) txOutputMinAda

  pure $ wrap txOutputRec
    { amount = mkValue newCoin (getMultiAsset txOutputValue) }

-- removes duplicates
appendInputs
  :: Array TransactionInput
  -> Array TransactionInput
  -> Array TransactionInput
appendInputs a b = Set.toUnfoldable (Set.fromFoldable a <> Set.fromFoldable b)

setTxChangeOutputs
  :: Array TransactionOutput -> Transaction -> Transaction
setTxChangeOutputs outputs tx =
  tx # _body <<< _outputs %~ flip append outputs

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
  -> Maybe OutputDatum
  -> Value
  -> BigInt
  -> TransactionBody
  -> BalanceTxM (Array TransactionOutput)
makeChange
  ownWalletAddresses
  changeAddress
  changeDatum
  inputValue'
  miscFee
  txBody =
  -- Always generate change when a transaction has no outputs to avoid issues
  -- with transaction confirmation:
  -- FIXME: https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
  if excessValue == mempty && (txBody ^. _outputs) /= mempty then pure mempty
  else do
    res <- traverse (ltraverse liftValue) changeValueOutputCoinPairs
      >>= splitOversizedValues
      >>= assignCoinsToChangeValues changeAddress excessCoin
    pure $ mkChangeOutput changeAddress changeDatum <$> res
  where
  inputValue = Val.fromValue inputValue'

  -- | Change `Value`s for all assets, where each change map is paired with a
  -- | corresponding coin from the original outputs.
  -- |
  -- | This array is sorted into ascending order of asset count, where empty
  -- | change `Value`s are all located at the start of the list.
  -- |
  -- | Taken from cardano-wallet:
  -- | https://github.com/input-output-hk/cardano-wallet/blob/4c2eb651d79212157a749d8e69a48fff30862e93/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1447
  changeValueOutputCoinPairs :: NonEmptyArray (Val /\ BigInt)
  changeValueOutputCoinPairs = outputCoins
    # NEArray.zip changeForAssets
    # NEArray.sortWith (Array.length <<< Val.valueAssets <<< fst)
    where
    outputCoins :: NonEmptyArray BigInt
    outputCoins =
      NEArray.fromArray
        ( BigNum.toBigInt <<< unwrap <<< Value.getCoin <<< _.amount <<< unwrap
            <$> ownAddressOutputs
        )
        ?? NEArray.singleton zero

  splitOversizedValues
    :: NonEmptyArray (Value /\ BigInt)
    -> BalanceTxM (NonEmptyArray (Value /\ BigInt))
  splitOversizedValues pairs =
    asksConstraints Constraints._maxChangeOutputTokenQuantity >>= case _ of
      Nothing -> pure pairs
      Just maxTokenQuantity -> do
        traverse bundle pairs <#> \bundled ->
          unbundle <$>
            ( equipartitionValueWithTokenQuantityUpperBound maxTokenQuantity =<<
                bundled
            )
    where
    bundle :: Value /\ BigInt -> BalanceTxM Value
    bundle (Value _ assets /\ coin) = do
      coin' <- liftEither
        (note (NumericOverflowError Nothing) $ BigNum.fromBigInt coin)
      pure $ mkValue (wrap coin') assets

    unbundle :: Value -> Value /\ BigInt
    unbundle (Value coin assets) = mkValue mempty assets /\ BigNum.toBigInt
      (unwrap coin)

  -- outputs belonging to one of the wallet's addresses.
  ownAddressOutputs :: Array TransactionOutput
  ownAddressOutputs = Array.filter isOwnWalletAddress $ txBody ^. _outputs
    where
    isOwnWalletAddress = unwrap >>> _.address >>> flip Set.member
      ownWalletAddresses

  changeForAssets :: NonEmptyArray Val
  changeForAssets = foldr
    (NEArray.zipWith (<>) <<< makeChangeForAsset ownAddressOutputs)
    (NEArray.replicate (length ownAddressOutputs) mempty)
    excessAssets

  excessAssets :: Array (AssetClass /\ BigInt)
  excessAssets = Val.valueAssets excessValue

  excessCoin :: BigInt
  excessCoin = case excessValue of
    Val c _ -> c

  excessValue :: Val
  excessValue = posVal $
    (inputValue <> mintValue txBody) `Val.minus`
      (outputValue txBody <> minFeeValue txBody <> Val miscFee Map.empty)

  posVal :: Val -> Val
  posVal (Val coin nonAdaAsset) =
    Val (max coin zero)
      $ Map.filter (not <<< Map.isEmpty)
      $ map (Map.filter (\x -> x > zero)) nonAdaAsset

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
  -> NonEmptyArray Val
makeChangeForAsset
  ownAddressOutputs
  (assetClass@(AssetClass scriptHash assetName) /\ excess) =
  mkVal <$>
    partition excess weights ?? equipartition excess (length weights)
  where
  mkVal n = Val zero (Map.singleton scriptHash $ Map.singleton assetName n)

  weights :: NonEmptyArray BigInt
  weights = NEArray.fromArray assetQuantities ?? NEArray.singleton one

  assetQuantities :: Array BigInt
  assetQuantities =
    ownAddressOutputs <#> BigNum.toBigInt <<< Value.getAssetQuantity assetClass
      <<< _.amount
      <<<
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
makeChangeForCoin :: NonEmptyArray BigInt -> BigInt -> NonEmptyArray Val
makeChangeForCoin weights excess =
  flip Val Map.empty <$>
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
    unsafeFromJust "assignCoinsToChangeValues" <<< Val.toValue <$> worker
      (adaRequiredAtStart changeValues)
      changeValues
  where
  worker :: BigInt -> NonEmptyArray ChangeValue -> Array Val
  worker adaRequired changeValues = changeValues # NEArray.uncons >>> case _ of
    { head: x, tail }
      | Just xs <- NEA.fromArray tail
      , adaAvailable < adaRequired && noTokens x ->
          worker (adaRequired - x.minCoin) xs
    _ ->
      let
        adaRemaining :: BigInt
        adaRemaining = max zero (adaAvailable - adaRequired)

        changeValuesForOutputCoins :: NonEmptyArray Val
        changeValuesForOutputCoins =
          let
            weights = _.outputAda <$> changeValues
          in
            makeChangeForCoin weights adaRemaining

        changeValuesWithMinCoins :: NonEmptyArray Val
        changeValuesWithMinCoins = assignMinCoin <$> changeValues
      in
        NEArray.toArray $
          NEArray.zipWith append changeValuesWithMinCoins
            changeValuesForOutputCoins
    where
    noTokens :: ChangeValue -> Boolean
    noTokens = null <<< Val.getAssets <<< _.value

    assignMinCoin :: ChangeValue -> Val
    assignMinCoin { value: (Val _ assets), minCoin } =
      Val minCoin assets

  adaRequiredAtStart :: NonEmptyArray ChangeValue -> BigInt
  adaRequiredAtStart = sum <<< map _.minCoin

  changeValuesAtStart :: BalanceTxM (NonEmptyArray ChangeValue)
  changeValuesAtStart =
    for pairsAtStart \(value /\ outputAda) ->
      { value: Val.fromValue value, outputAda, minCoin: _ } <$> minCoinFor value

  minCoinFor :: Value -> BalanceTxM BigInt
  minCoinFor value = do
    let
      -- NOTE: Datum here doesn't matter, we deconstruct UTxO immediately anyway
      txOutput = mkChangeOutput changeAddress Nothing value
    coinsPerUtxoByte <- askCoinsPerUtxoUnit
    pure $ BigNum.toBigInt $ utxoMinAdaValue coinsPerUtxoByte txOutput

type ChangeValue = { value :: Val, outputAda :: BigInt, minCoin :: BigInt }

mkChangeOutput :: Address -> Maybe OutputDatum -> Value -> TransactionOutput
mkChangeOutput changeAddress datum amount = wrap
  { address: changeAddress, amount, datum, scriptRef: Nothing }

--------------------------------------------------------------------------------
-- Getters for various `Value`s
--------------------------------------------------------------------------------

getRequiredValue
  :: BigInt -> UtxoMap -> TransactionBody -> Either BalanceTxError Val
getRequiredValue miscFee utxos txBody = do
  getInputVal utxos txBody <#> \inputValue ->
    ( outputValue txBody <> minFeeValue txBody <> Val miscFee Map.empty
    )
      `Val.minus` (inputValue <> mintValue txBody)

getAmount :: TransactionOutput -> Value
getAmount = _.amount <<< unwrap

getInputVal :: UtxoMap -> TransactionBody -> Either BalanceTxError Val
getInputVal utxos txBody =
  foldMap (view _amount >>> Val.fromValue) <$>
    for (Array.fromFoldable $ txBody ^. _inputs) \oref ->
      note (UtxoLookupFailedFor oref utxos) (Map.lookup oref utxos)

outputValue :: TransactionBody -> Val
outputValue txBody = foldMap (view _amount >>> Val.fromValue)
  (txBody ^. _outputs)

minFeeValue :: TransactionBody -> Val
minFeeValue txBody = Val.fromCoin $ txBody ^. _fee

mintValue :: TransactionBody -> Val
mintValue txBody = maybe mempty Val.fromMint (txBody ^. _mint)

getProposalsBalance :: Transaction -> BigInt
getProposalsBalance tx =
  let
    deposits :: BigInt
    deposits =
      sum $ map (BigNum.toBigInt <<< _.deposit <<< unwrap)
        (tx ^. _body <<< _votingProposals)
  in
    deposits

getCertsBalance :: Transaction -> ProtocolParameters -> BigInt
getCertsBalance tx (ProtocolParameters pparams) =
  let
    stakeAddressDeposit :: BigInt
    stakeAddressDeposit = BigNum.toBigInt $ unwrap pparams.stakeAddressDeposit

    toBi :: Coin -> BigInt
    toBi = BigNum.toBigInt <<< unwrap

    deposits :: BigInt
    deposits =
      (tx ^. _body <<< _certs) #
        map
          ( case _ of
              StakeRegistration _ ->
                stakeAddressDeposit

              StakeDeregistration _ ->
                negate $ stakeAddressDeposit

              StakeRegDelegCert _ _ stakeCredDeposit ->
                toBi stakeCredDeposit

              VoteRegDelegCert _ _ stakeCredDeposit ->
                toBi stakeCredDeposit

              StakeVoteRegDelegCert _ _ _ stakeCredDeposit ->
                toBi stakeCredDeposit

              RegDrepCert _ drepDeposit _ ->
                toBi drepDeposit

              UnregDrepCert _ drepDeposit ->
                negate $ toBi drepDeposit

              _ -> zero
          )
          >>> sum

    withdrawals :: BigInt
    withdrawals =
      sum $ map (BigNum.toBigInt <<< unwrap) $ tx ^. _body <<<
        _withdrawals
  in
    deposits - withdrawals

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

logBalancerState
  :: String
  -> UtxoMap
  -> BalancerState Transaction
  -> BalanceTxM Unit
logBalancerState message utxos { transaction, changeOutputs } =
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
    txBody :: TransactionBody
    txBody = tx ^. _body

    outputValuesTagSet :: Maybe (Array TransactionOutput) -> Array TagSet
    outputValuesTagSet Nothing =
      [ "Output Value" `tagSetTag` pprintVal (outputValue txBody) ]
    outputValuesTagSet (Just changeOutputs) =
      [ "Output Value without change" `tagSetTag` pprintVal
          (outputValue txBody)
      , "Change Value" `tagSetTag` pprintValue
          (unsafePartial $ foldMap (getAmount) changeOutputs)
      ]

    transactionInfo :: Val -> TagSet
    transactionInfo inputValue =
      TagSet.fromArray $
        [ "Input Value" `tagSetTag` pprintVal inputValue
        , "Mint Value" `tagSetTag` pprintVal (mintValue txBody)
        , "Fees" `tag` BigNum.toString (unwrap (txBody ^. _fee))
        ] <> outputValuesTagSet mChangeOutputs
  in
    do
      except (getInputVal utxos txBody)
        >>= (flip Logger.info (message <> ":") <<< transactionInfo)

liftValue :: forall a. MonadError BalanceTxError a => Val -> a Value
liftValue val = liftEither $ note (NumericOverflowError $ Just val) $
  Val.toValue val
