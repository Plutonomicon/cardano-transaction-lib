module Ctl.Internal.ProcessConstraints
  ( mkUnbalancedTxImpl
  ) where

import Prelude

import Cardano.Transaction.Edit
  ( DetachedRedeemer
  , RedeemerPurpose(ForSpend, ForMint, ForReward, ForCert)
  , attachRedeemers
  , mkRedeemersContext
  )
import Cardano.Types
  ( Certificate
      ( StakeDelegation
      , PoolRetirement
      , PoolRegistration
      , StakeDeregistration
      , StakeRegistration
      )
  , DataHash
  , NetworkId
  , PlutusData
  , PlutusScript
  , ScriptHash
  , ScriptRef(NativeScriptRef, PlutusScriptRef)
  , StakeCredential(StakeCredential)
  , Transaction
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionUnspentOutput(TransactionUnspentOutput)
  , UtxoMap
  , Value(Value)
  , _body
  , _certs
  , _inputs
  , _isValid
  , _mint
  , _networkId
  , _outputs
  , _referenceInputs
  , _requiredSigners
  , _withdrawals
  , _witnessSet
  )
import Cardano.Types as Cardano
import Cardano.Types.Address
  ( Address(BaseAddress, EnterpriseAddress)
  , getPaymentCredential
  )
import Cardano.Types.Coin as Coin
import Cardano.Types.Credential
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  , asScriptHash
  )
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.DataHash as Datum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash, OutputDatum))
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.Transaction as Transaction
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Trans (get, gets, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Contract (getProtocolParameters)
import Ctl.Internal.Contract.Monad (Contract, getQueryHandle, wrapQueryM)
import Ctl.Internal.Helpers (liftEither, liftM, unsafeFromJust)
import Ctl.Internal.ProcessConstraints.Error
  ( MkUnbalancedTxError
      ( CannotSatisfyAny
      , CannotWithdrawRewardsNativeScript
      , CannotWithdrawRewardsPlutusScript
      , CannotWithdrawRewardsPubKey
      , DatumWrongHash
      , CannotMintZero
      , ExpectedPlutusScriptGotNativeScript
      , CannotFindDatum
      , CannotQueryDatum
      , CannotGetValidatorHashFromAddress
      , TxOutRefWrongType
      , CannotConvertPOSIXTimeRange
      , WrongRefScriptHash
      , ValidatorHashNotFound
      , MintingPolicyNotFound
      , DatumNotFound
      , TxOutRefNotFound
      , CannotSolveTimeConstraints
      , NumericOverflow
      , OwnPubKeyAndStakeKeyMissing
      , CannotAttachRedeemer
      )
  )
import Ctl.Internal.ProcessConstraints.State
  ( ConstraintProcessingState
  , ConstraintsM
  , ValueSpentBalances(ValueSpentBalances)
  , _costModels
  , _cpsTransaction
  , _cpsUsedUtxos
  , _datums
  , _lookups
  , _redeemers
  , _refScriptsUtxoMap
  , _valueSpentBalancesInputs
  , _valueSpentBalancesOutputs
  , provideValue
  , requireValue
  , totalMissingValue
  )
import Ctl.Internal.QueryM.Pools
  ( getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  )
import Ctl.Internal.Transaction
  ( attachDatum
  , attachNativeScript
  , attachPlutusScript
  , setScriptDataHash
  )
import Ctl.Internal.Types.Interval
  ( POSIXTimeRange
  , always
  , intersection
  , isEmpty
  , posixTimeRangeToTransactionValidity
  )
import Ctl.Internal.Types.ScriptLookups (ScriptLookups)
import Ctl.Internal.Types.TxConstraints
  ( DatumPresence(DatumWitness, DatumInline)
  , InputWithScriptRef(SpendInput, RefInput)
  , TxConstraint
      ( MustNotBeValid
      , MustSatisfyAnyOf
      , MustWithdrawStakeNativeScript
      , MustWithdrawStakePlutusScript
      , MustWithdrawStakePubKey
      , MustDelegateStakeNativeScript
      , MustDelegateStakePlutusScript
      , MustDelegateStakePubKey
      , MustRetirePool
      , MustRegisterPool
      , MustDeregisterStakeNativeScript
      , MustDeregisterStakePlutusScript
      , MustRegisterStakeScript
      , MustDeregisterStakePubKey
      , MustRegisterStakePubKey
      , MustHashDatum
      , MustPayToNativeScript
      , MustPayToScript
      , MustPayToPubKeyAddress
      , MustMintValueUsingNativeScript
      , MustMintValue
      , MustReferenceOutput
      , MustSpendNativeScriptOutput
      , MustSpendScriptOutput
      , MustSpendPubKeyOutput
      , MustProduceAtLeast
      , MustSpendAtLeast
      , MustBeSignedBy
      , MustValidateIn
      , MustIncludeDatum
      )
  , TxConstraints
  , utxoWithScriptRef
  )
import Ctl.Internal.Types.Val as Val
import Data.Array (cons, partition, toUnfoldable, zip)
import Data.Array (mapMaybe, singleton, (:)) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either, hush, isRight, note)
import Data.Foldable (foldM)
import Data.Lens ((%=), (.=), (.~), (<>=))
import Data.Lens.Getter (use)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List(Nil, Cons))
import Data.Map (Map, empty, fromFoldable, lookup, union)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
import Data.Traversable (for, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)
import Prelude (join) as Bind

-- The constraints don't precisely match those of Plutus:
-- `forall v. (FromData (DatumType v), ToData (DatumType v), ToData (RedeemerType v))`
-- as we don't have the same granularity on the classes, but the type `v` fixes
-- types `d` and `r` as seen below. We could alternatively create specific typeclasses:
-- ToData (DatumType v) <-> (DatumType v d, ToData d) <= ToDataDatumType v d
-- if we require granular control, similarly FromDataDatumType v d etc.
-- We could use `MonadError` to clean up the `ExceptT`s below although we can't
-- use the type alias because they need to be fully applied so this is perhaps
-- more readable.
-- | Resolve some `TxConstraints` by modifying the `UnbalancedTx` in the
-- | `ConstraintProcessingState`
processLookupsAndConstraints
  :: TxConstraints
  -> ConstraintsM (Either MkUnbalancedTxError Unit)
processLookupsAndConstraints constraints = runExceptT do
  -- Hash all the MintingPolicys and Scripts beforehand. These maps are lost
  -- after we `runReaderT`, unlike Plutus that has a `Map` instead of `Array`.
  lookups <- use _lookups <#> unwrap

  let
    plutusMintingPolicies =
      fromFoldable $
        zip (PlutusScript.hash <$> lookups.plutusMintingPolicies)
          lookups.plutusMintingPolicies
    plutusScripts =
      fromFoldable $
        zip (PlutusScript.hash <$> lookups.scripts) lookups.scripts
    ctx = { plutusMintingPolicies, plutusScripts }

  timeConstraintsSolved <- except $ resumeTimeConstraints constraints

  ExceptT $ foldConstraints (processConstraint ctx) timeConstraintsSolved
  ExceptT addFakeScriptDataHash
  ExceptT addMissingValueSpent
  ExceptT updateUsedUtxos

  where
  -- Don't write the output in terms of ExceptT because we can't write a
  -- partially applied `ConstraintsM` meaning this is more readable.
  foldConstraints
    :: forall (constr :: Type)
     . (constr -> ConstraintsM (Either MkUnbalancedTxError Unit))
    -> Array constr
    -> ConstraintsM (Either MkUnbalancedTxError Unit)
  foldConstraints handler =
    runExceptT <<< traverse_ (ExceptT <<< handler)

-- To build a transaction that satisfies the 'MustSpendAtLeast' and
-- `MustProduceAtLeast` constraints, we keep a tally of the required and
-- actual values we encounter on either side of the transaction. Then we
-- compute the missing value on both sides, and add an input with the
-- join of the positive parts of the missing values.

-- Helper to run the stack and get back to `QueryM`. See comments in
-- `processLookupsAndConstraints` regarding constraints.
runConstraintsM
  :: ScriptLookups
  -> TxConstraints
  -> Contract (Either MkUnbalancedTxError ConstraintProcessingState)
runConstraintsM lookups txConstraints = do
  { costModels } <- unwrap <$> getProtocolParameters
  let
    initCps :: ConstraintProcessingState
    initCps =
      { transaction: Transaction.empty
      , usedUtxos: Map.empty
      , valueSpentBalancesInputs:
          ValueSpentBalances { required: mempty, provided: mempty }
      , valueSpentBalancesOutputs:
          ValueSpentBalances { required: mempty, provided: mempty }
      , datums: mempty
      , redeemers: []
      , lookups
      , refScriptsUtxoMap: empty
      , costModels
      }

    unpackTuple
      :: Either MkUnbalancedTxError Unit /\ ConstraintProcessingState
      -> Either MkUnbalancedTxError ConstraintProcessingState
    unpackTuple (Left err /\ _) = Left err
    unpackTuple (_ /\ cps) = Right cps
  unpackTuple <$>
    flip runStateT initCps (processLookupsAndConstraints txConstraints)

-- | Adds a placeholder for ScriptDataHash. It will be wrong at this stage,
-- | because ExUnits hasn't been estimated yet. It will serve as a
-- | placeholder that will have the same size as the correct value.
addFakeScriptDataHash
  :: ConstraintsM (Either MkUnbalancedTxError Unit)
addFakeScriptDataHash = runExceptT do
  dats <- use _datums
  costModels <- use _costModels
  -- Use both script and minting redeemers in the order they were appended.
  tx <- use _cpsTransaction
  let
    ctx = mkRedeemersContext tx
  reds <- ExceptT $ use _redeemers <#> attachRedeemers ctx >>> lmap
    CannotAttachRedeemer
  tx' <- ExceptT $ liftEffect $ setScriptDataHash costModels reds dats tx <#>
    Right
  _cpsTransaction .= (tx' # _witnessSet <<< Cardano._redeemers .~ reds)

-- | Add the remaining balance of the total value that the tx must spend.
-- | See note [Balance of value spent]
addMissingValueSpent
  :: ConstraintsM (Either MkUnbalancedTxError Unit)
addMissingValueSpent = do
  missing <- gets totalMissingValue
  networkId <- getNetworkId
  if missing == mempty then pure $ Right unit
  else runExceptT do
    -- add 'missing' to the transaction's outputs. This ensures that the
    -- wallet will add a corresponding input when balancing the
    -- transaction.
    -- Step 4 of the process described in [Balance of value spent]
    lookups <- use _lookups <#> unwrap
    let
      pkh' = map unwrap lookups.ownPaymentPubKeyHash
      skh' = map unwrap lookups.ownStakePubKeyHash
    -- Potential fix me: This logic may be suspect:
    txOutAddress <- case pkh', skh' of
      Just pkh, Just skh -> pure $ BaseAddress
        { networkId
        , paymentCredential: wrap $ PubKeyHashCredential pkh
        , stakeCredential: wrap $ PubKeyHashCredential skh
        }
      Just pkh, Nothing -> pure $ EnterpriseAddress
        { networkId, paymentCredential: wrap $ PubKeyHashCredential pkh }
      Nothing, _ -> throwError OwnPubKeyAndStakeKeyMissing
    missingValue <- maybe (throwError NumericOverflow) pure $ Val.toValue
      missing
    let
      txOut = TransactionOutput
        { address: txOutAddress
        , amount: missingValue
        , datum: Nothing
        , scriptRef: Nothing
        }
    _cpsTransaction <<< _body <<< _outputs %= Array.(:) txOut

updateUsedUtxos
  :: ConstraintsM (Either MkUnbalancedTxError Unit)
updateUsedUtxos = runExceptT do
  txOutputs <- use _lookups <#> unwrap >>> _.txOutputs
  refScriptsUtxoMap <- use _refScriptsUtxoMap
  let
    cTxOutputs :: Map TransactionInput TransactionOutput
    cTxOutputs =
      (txOutputs `union` refScriptsUtxoMap)
  -- Left bias towards original map, hence `flip`:
  _cpsUsedUtxos %= flip union cTxOutputs

resumeTimeConstraints
  :: Array TxConstraint -> Either MkUnbalancedTxError (Array TxConstraint)
resumeTimeConstraints constraints = do
  let
    { no: nonTimeConstraints, yes: timeConstraints } = partition
      isTimeConstraint
      constraints
    intervals = Array.mapMaybe constraintToInterval timeConstraints
  newInterval <- foldM mergeIntervals always intervals
  pure $ cons (MustValidateIn newInterval) nonTimeConstraints
  where
  mergeIntervals
    :: POSIXTimeRange
    -> POSIXTimeRange
    -> Either MkUnbalancedTxError POSIXTimeRange
  mergeIntervals interval1 interval2 =
    let
      newInterval :: POSIXTimeRange
      newInterval = intersection interval1 interval2
    in
      if isEmpty newInterval then Left $ CannotSolveTimeConstraints interval1
        interval2
      else pure newInterval

  constraintToInterval :: TxConstraint -> Maybe POSIXTimeRange
  constraintToInterval = case _ of
    MustValidateIn x -> Just x
    _ -> Nothing

  isTimeConstraint :: TxConstraint -> Boolean
  isTimeConstraint (MustValidateIn _) = true
  isTimeConstraint _ = false

lookupTxOutRef
  :: TransactionInput
  -> Maybe InputWithScriptRef
  -> ConstraintsM (Either MkUnbalancedTxError TransactionOutput)
lookupTxOutRef oref = case _ of
  Just inputWithRefScript ->
    lookup oref (utxoWithScriptRef inputWithRefScript)
      # maybe (lookupTxOutRef oref Nothing) (pure <<< Right)
  Nothing ->
    runExceptT do
      utxos <- use _lookups <#> unwrap >>> _.txOutputs
      liftM (TxOutRefNotFound oref) (lookup oref utxos)

lookupDatum
  :: DataHash
  -> ConstraintsM (Either MkUnbalancedTxError PlutusData)
lookupDatum dh = do
  otherDt <- use _lookups <#> unwrap >>> _.datums
  pure $ note (DatumNotFound dh) $ lookup dh otherDt

lookupMintingPolicy
  :: forall a
   . ScriptHash
  -> Map ScriptHash a
  -> Either MkUnbalancedTxError a
lookupMintingPolicy mph mpsMap =
  note (MintingPolicyNotFound mph) $ lookup mph mpsMap

lookupValidator
  :: ScriptHash
  -> Map ScriptHash PlutusScript
  -> Either MkUnbalancedTxError PlutusScript
lookupValidator vh osMap =
  note (ValidatorHashNotFound vh) $ lookup vh osMap

-- Ensures uniqueness
appendInputs
  :: Array TransactionInput -> Array TransactionInput -> Array TransactionInput
appendInputs a b = Set.toUnfoldable (Set.fromFoldable a <> Set.fromFoldable b)

processScriptRefUnspentOut
  :: ScriptHash
  -> InputWithScriptRef
  -> ConstraintsM (Either MkUnbalancedTxError Unit)
processScriptRefUnspentOut scriptHash inputWithRefScript = do
  unspentOut <- case inputWithRefScript of
    SpendInput unspentOut -> do
      _cpsTransaction <<< _body <<< _inputs %=
        appendInputs [ _.input <<< unwrap $ unspentOut ]
      pure unspentOut
    RefInput unspentOut -> do
      let refInput = (unwrap unspentOut).input
      _cpsTransaction <<< _body <<< _referenceInputs %=
        appendInputs [ refInput ]
      pure unspentOut

  updateRefScriptsUtxoMap unspentOut
  checkScriptRef unspentOut
  where
  updateRefScriptsUtxoMap
    :: TransactionUnspentOutput -> ConstraintsM Unit
  updateRefScriptsUtxoMap (TransactionUnspentOutput { input, output }) =
    _refScriptsUtxoMap %= Map.insert input output

  checkScriptRef
    :: TransactionUnspentOutput
    -> ConstraintsM (Either MkUnbalancedTxError Unit)
  checkScriptRef (TransactionUnspentOutput { output }) =
    let
      refScriptHash :: Maybe ScriptHash
      refScriptHash = (unwrap output).scriptRef <#> case _ of
        NativeScriptRef ns -> NativeScript.hash ns
        PlutusScriptRef ps -> PlutusScript.hash ps

      err :: ConstraintsM (Either MkUnbalancedTxError Unit)
      err = pure $ throwError $ WrongRefScriptHash refScriptHash output
    in
      if Just scriptHash /= refScriptHash then err
      else pure (Right unit)

checkRefNative
  :: InputWithScriptRef
  -> ConstraintsM (Either MkUnbalancedTxError Boolean)
checkRefNative scriptRef =
  let
    out = (unwrap uout).output
  in
    pure $ note (WrongRefScriptHash Nothing out) $ isNative
      (unwrap (unwrap uout).output).scriptRef
  where
  isNative ref = ref >>=
    ( case _ of
        NativeScriptRef _ -> pure true
        _ -> pure false
    )

  uout :: TransactionUnspentOutput
  uout = case scriptRef of
    RefInput ref' -> ref'
    SpendInput ref' -> ref'

-- | Modify the `UnbalancedTx` so that it satisfies the constraints, if
-- | possible. Fails if a hash is missing from the lookups, or if an output
-- | of the wrong type is spent.
processConstraint
  :: { plutusMintingPolicies :: Map ScriptHash PlutusScript
     , plutusScripts :: Map ScriptHash PlutusScript
     }
  -> TxConstraint
  -> ConstraintsM (Either MkUnbalancedTxError Unit)
processConstraint
  ctx@{ plutusMintingPolicies, plutusScripts }
  c = do
  queryHandle <- lift $ getQueryHandle
  case c of
    MustIncludeDatum dat -> pure <$> addDatum dat
    MustValidateIn posixTimeRange -> do
      { systemStart } <- asks _.ledgerConstants
      eraSummaries <- liftAff $
        queryHandle.getEraSummaries
          >>= either (liftEffect <<< throw <<< show) pure
      runExceptT do
        ({ timeToLive, validityStartInterval }) <- liftEither $
          posixTimeRangeToTransactionValidity eraSummaries systemStart
            posixTimeRange # lmap (CannotConvertPOSIXTimeRange posixTimeRange)
        _cpsTransaction <<< _body <<< _Newtype %=
          _
            { ttl = timeToLive
            , validityStartInterval = validityStartInterval
            }
    MustBeSignedBy pkh -> runExceptT do
      -- FIXME This is incompatible with Plutus' version, which requires
      -- the corresponding `paymentPubKey` lookup. In the next major version,
      -- we might wish to revise this
      -- See https://github.com/Plutonomicon/cardano-transaction-lib/issues/569
      _cpsTransaction <<< _body <<< _requiredSigners <>=
        [ wrap $ unwrap $ unwrap pkh ]
    MustSpendAtLeast value -> do
      runExceptT $ _valueSpentBalancesInputs <>= requireValue value
    MustProduceAtLeast value -> do
      runExceptT $ _valueSpentBalancesOutputs <>= requireValue value
    MustSpendPubKeyOutput txo -> runExceptT do
      TransactionOutput { amount } <- ExceptT $ lookupTxOutRef txo Nothing
      -- POTENTIAL FIX ME: Plutus has Tx.TxIn and Tx.PubKeyTxIn -- TxIn
      -- keeps track TransactionInput and TxInType (the input type, whether
      -- consuming script, public key or simple script)
      _cpsTransaction <<< _body <<< _inputs %= appendInputs [ txo ]
      _valueSpentBalancesInputs <>= provideValue amount
    MustSpendScriptOutput txo red scriptRefUnspentOut -> runExceptT do
      txOut <- ExceptT $ lookupTxOutRef txo scriptRefUnspentOut
      case txOut of
        TransactionOutput { datum: Nothing } ->
          throwError $ TxOutRefWrongType txo
        TransactionOutput { address, amount, datum: datum' } ->
          do
            vHash <- liftM
              (CannotGetValidatorHashFromAddress address)
              (getPaymentCredential address >>= unwrap >>> asScriptHash)
            case scriptRefUnspentOut of
              Nothing -> do
                plutusScript <-
                  except $ lookupValidator vHash plutusScripts
                lift $ attachToCps (map pure <<< attachPlutusScript)
                  plutusScript
              Just scriptRefUnspentOut' ->
                ExceptT $ processScriptRefUnspentOut vHash scriptRefUnspentOut'
            -- Note: Plutus uses `TxIn` to attach a redeemer and datum.
            -- Use the datum hash inside the lookup
            case datum' of
              Just (OutputDatumHash dHash) -> do
                dat <- ExceptT do
                  mDatumLookup <- lookupDatum dHash
                  if isRight mDatumLookup then
                    pure mDatumLookup
                  else
                    liftAff $ queryHandle.getDatumByHash dHash <#> hush
                      >>> Bind.join
                      >>> note
                        (CannotQueryDatum dHash)
                lift $ addDatum dat
              Just (OutputDatum _) -> pure unit
              Nothing -> throwError CannotFindDatum
            _cpsTransaction <<< _body <<< _inputs %= appendInputs [ txo ]
            let
              dRedeemer :: DetachedRedeemer
              dRedeemer =
                { purpose: ForSpend txo
                , datum: red
                }
            _redeemers <>= [ dRedeemer ]
            _valueSpentBalancesInputs <>= provideValue amount
    MustSpendNativeScriptOutput txo ns -> runExceptT do
      _cpsTransaction <<< _body <<< _inputs %= appendInputs [ txo ]
      lift $ attachToCps (map pure <<< attachNativeScript) ns
    MustReferenceOutput refInput -> runExceptT do
      _cpsTransaction <<< _body <<< _referenceInputs <>= [ refInput ]
    MustMintValue scriptHash red tn i scriptRefUnspentOut -> runExceptT do
      case scriptRefUnspentOut of
        Nothing -> do
          mp <- except $ lookupMintingPolicy scriptHash plutusMintingPolicies
          lift $ attachToCps (map pure <<< attachPlutusScript) mp
        Just scriptRefUnspentOut' -> do
          isNative <- ExceptT $ checkRefNative scriptRefUnspentOut'
          when isNative
            $ throwError
            $ ExpectedPlutusScriptGotNativeScript scriptHash
          (ExceptT $ processScriptRefUnspentOut scriptHash scriptRefUnspentOut')

      let
        mkValue = Value Coin.zero <<< MultiAsset.singleton scriptHash tn
        mint = Mint.singleton scriptHash tn i
      -- If i is negative we are burning tokens. The tokens burned must
      -- be provided as an input. So we add the value burnt to
      -- 'valueSpentBalancesInputs'. If i is positive then new tokens are
      -- created which must be added to 'valueSpentBalancesOutputs'.
      -- If i is zero we raise error, because of
      -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1156
      if Int.toBigInt i < zero then do
        let
          value =
            mkValue $ unsafeFromJust "processConstraints" $ Int.asNegative i
        _valueSpentBalancesInputs <>= provideValue value
      else if Int.toBigInt i == zero then do
        throwError $ CannotMintZero scriptHash tn
      else do
        let
          value =
            mkValue $ unsafeFromJust "processConstraints" $ Int.asPositive i
        _valueSpentBalancesOutputs <>= provideValue value
      _redeemers <>=
        [ { purpose: ForMint scriptHash, datum: red } ]
      -- Remove mint redeemers from array before reindexing.
      unsafePartial $ _cpsTransaction <<< _body <<< _mint <>= Just mint

    MustMintValueUsingNativeScript ns tn i -> runExceptT do
      let
        cs = NativeScript.hash ns
        mkValue = Value Coin.zero <<< MultiAsset.singleton cs tn
        mint = Mint.singleton cs tn i
      lift $ attachToCps (map pure <<< attachNativeScript) ns
      -- If i is negative we are burning tokens. The tokens burned must
      -- be provided as an input. So we add the value burnt to
      -- 'valueSpentBalancesInputs'. If i is positive then new tokens are
      -- created which must be added to 'valueSpentBalancesOutputs'.
      if Int.toBigInt i < zero then do
        let
          value =
            mkValue $ unsafeFromJust "processConstraints" $ Int.asNegative i
        _valueSpentBalancesInputs <>= provideValue value
      else if Int.toBigInt i == zero then do
        throwError $ CannotMintZero cs tn
      else do
        let
          value =
            mkValue $ unsafeFromJust "processConstraints" $ Int.asPositive i
        _valueSpentBalancesOutputs <>= provideValue value

      unsafePartial $ _cpsTransaction <<< _body <<< _mint <>= Just mint

    MustPayToPubKeyAddress pkh skh mDatum scriptRef amount -> do
      networkId <- getNetworkId
      runExceptT do
        -- If non-inline datum is presented, add it to 'datumWitnesses' and
        -- Array of datums.
        datum <- for mDatum \(dat /\ datp) -> do
          when (datp == DatumWitness) $ lift $ addDatum dat
          pure $ outputDatum dat datp
        let
          address = case skh of
            Just skh' -> BaseAddress
              { networkId
              , paymentCredential: wrap $ PubKeyHashCredential $ unwrap pkh
              , stakeCredential: wrap $ PubKeyHashCredential $ unwrap skh'
              }
            Nothing -> EnterpriseAddress
              { networkId
              , paymentCredential: wrap $ PubKeyHashCredential $ unwrap pkh
              }
          txOut = TransactionOutput
            { address
            , amount
            , datum
            , scriptRef: scriptRef
            }
        _cpsTransaction <<< _body <<< _outputs %= Array.(:) txOut
        _valueSpentBalancesOutputs <>= provideValue amount
    MustPayToScript vlh mbCredential dat datp scriptRef amount -> do
      networkId <- getNetworkId
      runExceptT do
        let
          datum' = outputDatum dat datp
          txOut = TransactionOutput
            { address: case mbCredential of
                Nothing -> EnterpriseAddress
                  { networkId
                  , paymentCredential: wrap $ ScriptHashCredential vlh
                  }
                Just cred -> BaseAddress
                  { networkId
                  , paymentCredential: wrap $ ScriptHashCredential vlh
                  , stakeCredential: wrap cred
                  }
            , amount
            , datum: Just datum'
            , scriptRef: scriptRef
            }
        -- Note we don't `addDatum` as this included as part of `mustPayToScript`
        -- constraint already.
        _cpsTransaction <<< _body <<< _outputs %= Array.(:) txOut
        _valueSpentBalancesOutputs <>= provideValue amount
    MustPayToNativeScript nsh mbCredential amount -> do
      networkId <- getNetworkId
      runExceptT do
        let
          txOut = TransactionOutput
            { address: case mbCredential of
                Nothing -> EnterpriseAddress
                  { networkId
                  , paymentCredential: wrap $ ScriptHashCredential nsh
                  }
                Just cred -> BaseAddress
                  { networkId
                  , paymentCredential: wrap $ ScriptHashCredential nsh
                  , stakeCredential: wrap cred
                  }
            , amount
            , datum: Nothing
            , scriptRef: Nothing
            }
        _cpsTransaction <<< _body <<< _outputs %= Array.(:) txOut
        _valueSpentBalancesOutputs <>= provideValue amount
    MustHashDatum dh dt -> do
      let dh' = hashPlutusData dt
      if dh' == dh then pure <$> addDatum dt
      else pure $ throwError $ DatumWrongHash dh dt
    MustRegisterStakePubKey skh -> runExceptT do
      void $ lift $ addCertificate
        $ StakeRegistration
        $ StakeCredential
        $ PubKeyHashCredential
        $ unwrap skh
    MustDeregisterStakePubKey pubKey -> runExceptT do
      void $ lift $ addCertificate
        $ StakeDeregistration
        $ StakeCredential
        $ PubKeyHashCredential
        $ unwrap pubKey
    MustRegisterStakeScript scriptHash -> runExceptT do
      void $ lift $ addCertificate
        $ StakeRegistration
        $ StakeCredential
        $ ScriptHashCredential scriptHash
    MustDeregisterStakePlutusScript plutusScript redeemerData -> runExceptT do
      let
        cert = StakeDeregistration $ StakeCredential $ ScriptHashCredential
          ( PlutusScript.hash plutusScript
          )
      _redeemers <>=
        [ { purpose: ForCert cert, datum: redeemerData } ]
      void $ lift $ addCertificate cert
      lift $ attachToCps (map pure <<< attachPlutusScript) plutusScript
    MustDeregisterStakeNativeScript stakeValidator -> do
      void $ addCertificate $ StakeDeregistration
        $ wrap
        $ ScriptHashCredential
        $ NativeScript.hash stakeValidator
      pure <$> attachToCps (map pure <<< attachNativeScript) stakeValidator
    MustRegisterPool poolParams -> runExceptT do
      void $ lift $ addCertificate $ PoolRegistration poolParams
    MustRetirePool poolKeyHash epoch -> runExceptT do
      void $ lift $ addCertificate $ PoolRetirement { poolKeyHash, epoch }
    MustDelegateStakePubKey stakePubKeyHash poolKeyHash -> runExceptT do
      void $ lift $ addCertificate $
        StakeDelegation
          ( StakeCredential $ PubKeyHashCredential $ unwrap $
              stakePubKeyHash
          )
          poolKeyHash
    MustDelegateStakePlutusScript stakeValidator redeemerData poolKeyHash -> do
      runExceptT do
        let
          cert = StakeDelegation
            ( StakeCredential $ ScriptHashCredential
                $ PlutusScript.hash stakeValidator
            )
            poolKeyHash
        lift $ addCertificate cert
        _redeemers <>=
          [ { purpose: ForCert cert, datum: redeemerData } ]
        lift $ attachToCps (map pure <<< attachPlutusScript) stakeValidator
    MustDelegateStakeNativeScript stakeValidator poolKeyHash -> do
      void $ addCertificate $ StakeDelegation
        ( StakeCredential $ ScriptHashCredential $ NativeScript.hash
            stakeValidator
        )
        poolKeyHash
      pure <$> attachToCps (map pure <<< attachNativeScript) stakeValidator
    MustWithdrawStakePubKey spkh -> runExceptT do
      networkId <- lift getNetworkId
      mbRewards <- lift $ lift $ wrapQueryM $ getPubKeyHashDelegationsAndRewards
        spkh
      ({ rewards }) <- ExceptT $ pure $ note (CannotWithdrawRewardsPubKey spkh)
        mbRewards
      let
        rewardAddress =
          { networkId
          , stakeCredential: wrap $ PubKeyHashCredential $ unwrap spkh
          }
      _cpsTransaction <<< _body <<< _withdrawals %=
        Map.insert rewardAddress (fromMaybe Coin.zero rewards)
    MustWithdrawStakePlutusScript stakeValidator redeemerData -> runExceptT do
      let hash = PlutusScript.hash stakeValidator
      networkId <- lift getNetworkId
      mbRewards <- lift $ lift $ wrapQueryM
        $ getValidatorHashDelegationsAndRewards
        $ wrap hash
      let
        rewardAddress =
          { networkId
          , stakeCredential: StakeCredential $ ScriptHashCredential hash
          }
      ({ rewards }) <- ExceptT $ pure $ note
        (CannotWithdrawRewardsPlutusScript stakeValidator)
        mbRewards
      _cpsTransaction <<< _body <<< _withdrawals %=
        Map.insert rewardAddress (fromMaybe Coin.zero rewards)
      _redeemers <>=
        [ { purpose: ForReward rewardAddress, datum: redeemerData } ]
      lift $ attachToCps (map pure <<< attachPlutusScript) stakeValidator
    MustWithdrawStakeNativeScript stakeValidator -> runExceptT do
      let hash = NativeScript.hash stakeValidator
      networkId <- lift getNetworkId
      mbRewards <- lift $ lift $ wrapQueryM
        $ getValidatorHashDelegationsAndRewards
        $ wrap hash
      let
        rewardAddress =
          { networkId
          , stakeCredential: StakeCredential $ ScriptHashCredential hash
          }
      ({ rewards }) <- ExceptT $ pure $ note
        (CannotWithdrawRewardsNativeScript stakeValidator)
        mbRewards
      _cpsTransaction <<< _body <<< _withdrawals %=
        Map.insert rewardAddress (fromMaybe Coin.zero rewards)
      lift $ attachToCps (map pure <<< attachNativeScript) stakeValidator
    MustSatisfyAnyOf xs -> do
      cps <- get
      let
        -- For each sublist, process the constraint from left to right, using the
        -- new state in the subsequent call. If we fail, reset to the initial
        -- state, `cps` and attempt for the next sublist. If a sublist is
        -- processed successfully, we can stop early - I think this is how Plutus
        -- behaves (POTENTIAL FIX ME). If all sublists fail, we fail overall as
        -- seen in the base case.
        tryNext
          :: List (List TxConstraint)
          -> ConstraintsM (Either MkUnbalancedTxError Unit)
        tryNext Nil = pure $ throwError CannotSatisfyAny
        tryNext (Cons ys zs) =
          -- Note this implicitly resets state to original cps upon failure (see
          -- `put`)
          foldM
            ( \_ constr -> runExceptT do
                let continue = put cps *> tryNext zs
                ( ExceptT $ processConstraint ctx constr
                    `catchError` \_ -> continue
                )
                  `catchError` \_ -> ExceptT continue
            )
            (Right unit)
            ys
      tryNext (toUnfoldable $ map toUnfoldable xs)
    MustNotBeValid -> runExceptT do
      _cpsTransaction <<< _isValid .= false
  where
  outputDatum
    :: PlutusData
    -> DatumPresence
    -> OutputDatum
  outputDatum dat = case _ of
    DatumInline -> OutputDatum dat
    DatumWitness -> OutputDatumHash $ Datum.hashPlutusData dat

-- Attach a Datum, Redeemer, or PlutusScript depending on the handler. They
-- share error type anyway.
attachToCps
  :: forall (a :: Type)
   . (a -> Transaction -> Effect Transaction)
  -> a -- Redeemer, Datum, or PlutusScript.
  -> ConstraintsM Unit
attachToCps handler object = do
  tx <- use _cpsTransaction
  newTx <- liftEffect $ handler object tx
  _cpsTransaction .= newTx

-- Attaches datum to the transaction and to Array of datums in the state.
addDatum
  :: PlutusData
  -> ConstraintsM Unit
addDatum dat = do
  attachToCps (map pure <<< attachDatum) dat
  _datums <>= Array.singleton dat

-- | Returns an index pointing to the location of the newly inserted certificate
-- | in the array of transaction certificates.
addCertificate
  :: Certificate
  -> ConstraintsM Unit
addCertificate cert = do
  _cpsTransaction <<< _body <<< _certs %= Array.(:) cert

getNetworkId
  :: ConstraintsM NetworkId
getNetworkId = use (_cpsTransaction <<< _body <<< _networkId)
  >>= maybe (asks _.networkId) pure

mkUnbalancedTxImpl
  :: ScriptLookups
  -> TxConstraints
  -> Contract (Either MkUnbalancedTxError (Transaction /\ UtxoMap))
mkUnbalancedTxImpl scriptLookups txConstraints =
  runConstraintsM scriptLookups txConstraints <#> map
    \({ transaction, usedUtxos }) -> transaction /\ usedUtxos
