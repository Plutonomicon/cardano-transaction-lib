module Ctl.Internal.ProcessConstraints
  ( mkUnbalancedTxImpl
  ) where

import Prelude

import Contract.Hashing (plutusScriptStakeValidatorHash)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Trans (get, gets, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Address (addressPaymentValidatorHash)
import Ctl.Internal.BalanceTx.RedeemerIndex
  ( RedeemerPurpose(ForReward, ForCert, ForMint, ForSpend)
  , UnindexedRedeemer(UnindexedRedeemer)
  , unindexedRedeemerToRedeemer
  )
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef(NativeScriptRef))
import Ctl.Internal.Cardano.Types.Transaction
  ( Certificate
      ( StakeDelegation
      , PoolRetirement
      , PoolRegistration
      , StakeDeregistration
      , StakeRegistration
      )
  , Transaction
  , TransactionOutput(TransactionOutput)
  , TransactionWitnessSet(TransactionWitnessSet)
  , _body
  , _certs
  , _inputs
  , _isValid
  , _mint
  , _networkId
  , _outputs
  , _referenceInputs
  , _requiredSigners
  , _scriptDataHash
  , _withdrawals
  , _witnessSet
  )
import Ctl.Internal.Cardano.Types.Value
  ( Coin(Coin)
  , getNonAdaAsset
  , isZero
  , mkSingletonValue'
  , mpsSymbol
  )
import Ctl.Internal.Contract (getProtocolParameters)
import Ctl.Internal.Contract.Monad (Contract, getQueryHandle, wrapQueryM)
import Ctl.Internal.Hashing (datumHash) as Hashing
import Ctl.Internal.Helpers (liftEither, liftM)
import Ctl.Internal.NativeScripts (nativeScriptHash)
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusTxOutputWithRefScript
  , fromPlutusValue
  )
import Ctl.Internal.Plutus.Types.Credential
  ( Credential(ScriptCredential, PubKeyCredential)
  )
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutputWithRefScript) as Plutus
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Ctl.Internal.ProcessConstraints.Error
  ( MkUnbalancedTxError
      ( CannotSatisfyAny
      , CannotWithdrawRewardsNativeScript
      , CannotWithdrawRewardsPlutusScript
      , CannotWithdrawRewardsPubKey
      , DatumWrongHash
      , CannotMakeValue
      , MintingPolicyHashNotCurrencySymbol
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
      , OwnPubKeyAndStakeKeyMissing
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
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx)
import Ctl.Internal.QueryM.Pools
  ( getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  )
import Ctl.Internal.Scripts
  ( mintingPolicyHash
  , nativeScriptStakeValidatorHash
  , validatorHash
  , validatorHashEnterpriseAddress
  )
import Ctl.Internal.Serialization.Address
  ( NetworkId
  , StakeCredential
  , baseAddress
  , baseAddressToAddress
  , keyHashCredential
  , scriptHashCredential
  )
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Transaction
  ( attachDatum
  , attachNativeScript
  , attachPlutusScript
  , setScriptDataHash
  )
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Ctl.Internal.Types.Interval
  ( POSIXTimeRange
  , always
  , intersection
  , isEmpty
  , posixTimeRangeToTransactionValidity
  )
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  )
import Ctl.Internal.Types.PubKeyHash
  ( payPubKeyHashBaseAddress
  , payPubKeyHashEnterpriseAddress
  , stakePubKeyHashRewardAddress
  )
import Ctl.Internal.Types.RewardAddress
  ( stakePubKeyHashRewardAddress
  , stakeValidatorHashRewardAddress
  ) as RewardAddress
import Ctl.Internal.Types.ScriptLookups (ScriptLookups)
import Ctl.Internal.Types.Scripts
  ( MintingPolicy(NativeMintingPolicy, PlutusMintingPolicy)
  , MintingPolicyHash
  , Validator
  , ValidatorHash
  )
import Ctl.Internal.Types.Transaction (TransactionInput)
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
  , TxConstraints(TxConstraints)
  , utxoWithScriptRef
  )
import Data.Array (cons, partition, toUnfoldable, zip)
import Data.Array (mapMaybe, singleton, (:)) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either, hush, isRight, note)
import Data.Foldable (foldM)
import Data.Lens (non, (%=), (%~), (.=), (.~), (<>=))
import Data.Lens.Getter (to, use)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List(Nil, Cons))
import Data.Map (Map, empty, fromFoldable, lookup, union)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Set (insert) as Set
import Data.Traversable (for, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
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
processLookupsAndConstraints
  (TxConstraints { constraints }) = runExceptT do
  -- Hash all the MintingPolicys and Scripts beforehand. These maps are lost
  -- after we `runReaderT`, unlike Plutus that has a `Map` instead of `Array`.
  lookups <- use _lookups <#> unwrap

  let
    mps = lookups.mps
    scripts = lookups.scripts
    mpsHashes = map mintingPolicyHash mps
    validatorHashes = map validatorHash scripts
    mpsMap = fromFoldable $ zip mpsHashes mps
    osMap = fromFoldable $ zip validatorHashes scripts

  timeConstraintsSolved <- except $ resumeTimeConstraints constraints

  ExceptT $ foldConstraints (processConstraint mpsMap osMap)
    timeConstraintsSolved
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
      { transaction: mempty
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
  reds <- use (_redeemers <<< to (map unindexedRedeemerToRedeemer))
  tx <- use _cpsTransaction
  tx' <- ExceptT $ liftEffect $ setScriptDataHash costModels reds dats tx <#>
    Right
  _cpsTransaction .= tx'

-- | Add the remaining balance of the total value that the tx must spend.
-- | See note [Balance of value spent]
addMissingValueSpent
  :: ConstraintsM (Either MkUnbalancedTxError Unit)
addMissingValueSpent = do
  missing <- gets totalMissingValue
  networkId <- getNetworkId
  if isZero missing then pure $ Right unit
  else runExceptT do
    -- add 'missing' to the transaction's outputs. This ensures that the
    -- wallet will add a corresponding input when balancing the
    -- transaction.
    -- Step 4 of the process described in [Balance of value spent]
    lookups <- use _lookups <#> unwrap
    let
      pkh' = lookups.ownPaymentPubKeyHash
      skh' = lookups.ownStakePubKeyHash
    -- Potential fix me: This logic may be suspect:
    txOutAddress <- case pkh', skh' of
      Nothing, Nothing -> throwError OwnPubKeyAndStakeKeyMissing
      Just pkh, Just skh -> pure $ payPubKeyHashBaseAddress networkId pkh skh
      Just pkh, Nothing -> pure $ payPubKeyHashEnterpriseAddress networkId pkh
      Nothing, Just skh -> pure $ stakePubKeyHashRewardAddress networkId skh
    let
      txOut = TransactionOutput
        { address: txOutAddress
        , amount: missing
        , datum: NoOutputDatum
        , scriptRef: Nothing
        }
    _cpsTransaction <<< _body <<< _outputs %= Array.(:) txOut

updateUsedUtxos
  :: ConstraintsM (Either MkUnbalancedTxError Unit)
updateUsedUtxos = runExceptT do
  txOutputs <- use _lookups <#> unwrap >>> _.txOutputs
  refScriptsUtxoMap <- use _refScriptsUtxoMap
  networkId <- lift getNetworkId
  let
    cTxOutputs :: Map TransactionInput TransactionOutput
    cTxOutputs =
      (txOutputs `union` refScriptsUtxoMap)
        <#> fromPlutusTxOutputWithRefScript networkId
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
      # maybe (lookupTxOutRef oref Nothing) (map Right <<< convertTxOutput)
  Nothing ->
    runExceptT do
      utxos <- use _lookups <#> unwrap >>> _.txOutputs
      txOutput <- liftM (TxOutRefNotFound oref) (lookup oref utxos)
      lift $ convertTxOutput txOutput
  where
  convertTxOutput
    :: Plutus.TransactionOutputWithRefScript -> ConstraintsM TransactionOutput
  convertTxOutput txOutput =
    flip fromPlutusTxOutputWithRefScript txOutput <$> getNetworkId

lookupDatum
  :: DataHash
  -> ConstraintsM (Either MkUnbalancedTxError Datum)
lookupDatum dh = do
  otherDt <- use _lookups <#> unwrap >>> _.datums
  pure $ note (DatumNotFound dh) $ lookup dh otherDt

lookupMintingPolicy
  :: MintingPolicyHash
  -> Map MintingPolicyHash MintingPolicy
  -> Either MkUnbalancedTxError MintingPolicy
lookupMintingPolicy mph mpsMap =
  note (MintingPolicyNotFound mph) $ lookup mph mpsMap

lookupValidator
  :: ValidatorHash
  -> Map ValidatorHash Validator
  -> Either MkUnbalancedTxError Validator
lookupValidator vh osMap =
  note (ValidatorHashNotFound vh) $ lookup vh osMap

processScriptRefUnspentOut
  :: forall (scriptHash :: Type)
   . Newtype scriptHash ScriptHash
  => scriptHash
  -> InputWithScriptRef
  -> ConstraintsM (Either MkUnbalancedTxError Unit)
processScriptRefUnspentOut scriptHash inputWithRefScript = do
  unspentOut <- case inputWithRefScript of
    SpendInput unspentOut -> do
      _cpsTransaction <<< _body <<< _inputs %= Set.insert
        (_.input <<< unwrap $ unspentOut)
      pure unspentOut
    RefInput unspentOut -> do
      let refInput = (unwrap unspentOut).input
      _cpsTransaction <<< _body <<< _referenceInputs %= Set.insert refInput
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
      refScriptHash = _.referenceScript $ unwrap $ (unwrap output).output

      err :: ConstraintsM (Either MkUnbalancedTxError Unit)
      err = pure $ throwError $ WrongRefScriptHash refScriptHash
        (unwrap output).output
    in
      if Just (unwrap scriptHash) /= refScriptHash then err
      else pure (Right unit)

checkRefNative
  :: InputWithScriptRef
  -> ConstraintsM (Either MkUnbalancedTxError Boolean)
checkRefNative scriptRef =
  let
    out = (unwrap ((unwrap uout).output)).output
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
  :: Map MintingPolicyHash MintingPolicy
  -> Map ValidatorHash Validator
  -> TxConstraint
  -> ConstraintsM (Either MkUnbalancedTxError Unit)
processConstraint mpsMap osMap c = do
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
      _cpsTransaction <<< _body <<< _requiredSigners <>= Just
        [ wrap $ unwrap $ unwrap pkh ]
    MustSpendAtLeast plutusValue -> do
      let value = fromPlutusValue plutusValue
      runExceptT $ _valueSpentBalancesInputs <>= requireValue value
    MustProduceAtLeast plutusValue -> do
      let value = fromPlutusValue plutusValue
      runExceptT $ _valueSpentBalancesOutputs <>= requireValue value
    MustSpendPubKeyOutput txo -> runExceptT do
      TransactionOutput { amount } <- ExceptT $ lookupTxOutRef txo Nothing
      -- POTENTIAL FIX ME: Plutus has Tx.TxIn and Tx.PubKeyTxIn -- TxIn
      -- keeps track TransactionInput and TxInType (the input type, whether
      -- consuming script, public key or simple script)
      _cpsTransaction <<< _body <<< _inputs %= Set.insert txo
      _valueSpentBalancesInputs <>= provideValue amount
    MustSpendScriptOutput txo red scriptRefUnspentOut -> runExceptT do
      txOut <- ExceptT $ lookupTxOutRef txo scriptRefUnspentOut
      case txOut of
        TransactionOutput { datum: NoOutputDatum } ->
          throwError $ TxOutRefWrongType txo
        TransactionOutput { address, amount, datum: datum' } ->
          do
            vHash <- liftM
              (CannotGetValidatorHashFromAddress address)
              (addressPaymentValidatorHash address)
            case scriptRefUnspentOut of
              Nothing -> do
                plutusScript <-
                  except $ unwrap <$> lookupValidator vHash osMap
                lift $ attachToCps attachPlutusScript plutusScript
              Just scriptRefUnspentOut' ->
                ExceptT $ processScriptRefUnspentOut vHash scriptRefUnspentOut'
            -- Note: Plutus uses `TxIn` to attach a redeemer and datum.
            -- Use the datum hash inside the lookup
            case datum' of
              OutputDatumHash dHash -> do
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
              OutputDatum _ -> pure unit
              NoOutputDatum -> throwError CannotFindDatum
            _cpsTransaction <<< _body <<< _inputs %= Set.insert txo
            let
              uiRedeemer = UnindexedRedeemer
                { purpose: ForSpend txo
                , datum: unwrap red
                }
            _redeemers <>= [ uiRedeemer ]
            _valueSpentBalancesInputs <>= provideValue amount
    MustSpendNativeScriptOutput txo ns -> runExceptT do
      _cpsTransaction <<< _body <<< _inputs %= Set.insert txo
      lift $ attachToCps (map pure <<< attachNativeScript) ns
    MustReferenceOutput refInput -> runExceptT do
      _cpsTransaction <<< _body <<< _referenceInputs %= Set.insert refInput
    MustMintValue mpsHash red tn i scriptRefUnspentOut -> runExceptT do
      case scriptRefUnspentOut of
        Nothing -> do
          mp <- except $ lookupMintingPolicy mpsHash mpsMap
          ( case mp of
              PlutusMintingPolicy p ->
                ( lift $ attachToCps
                    attachPlutusScript
                    p
                )
              NativeMintingPolicy _ -> throwError $
                ExpectedPlutusScriptGotNativeScript mpsHash
          )
        Just scriptRefUnspentOut' -> do
          isNative <- ExceptT $ checkRefNative scriptRefUnspentOut'
          when isNative $ throwError $ ExpectedPlutusScriptGotNativeScript
            mpsHash
          (ExceptT $ processScriptRefUnspentOut mpsHash scriptRefUnspentOut')

      cs <-
        liftM (MintingPolicyHashNotCurrencySymbol mpsHash) (mpsSymbol mpsHash)
      let value = mkSingletonValue' cs tn
      -- If i is negative we are burning tokens. The tokens burned must
      -- be provided as an input. So we add the value burnt to
      -- 'valueSpentBalancesInputs'. If i is positive then new tokens are
      -- created which must be added to 'valueSpentBalancesOutputs'.
      -- If i is zero we raise error, because of
      -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1156
      mintVal <-
        if i < zero then do
          v <- liftM (CannotMakeValue cs tn i) (value $ negate i)
          _valueSpentBalancesInputs <>= provideValue v
          pure $ map getNonAdaAsset $ value i
        else if i == zero then do
          throwError $ CannotMintZero cs tn
        else do
          v <- liftM (CannotMakeValue cs tn i) (value i)
          _valueSpentBalancesOutputs <>= provideValue v
          pure $ map getNonAdaAsset $ value i
      _redeemers <>=
        [ UnindexedRedeemer { purpose: ForMint mpsHash, datum: unwrap red } ]
      -- Remove mint redeemers from array before reindexing.
      _cpsTransaction <<< _body <<< _mint <>= map wrap mintVal

    MustMintValueUsingNativeScript ns tn i -> runExceptT do
      let mpHash = wrap <<< unwrap <<< nativeScriptHash $ ns

      lift $ attachToCps (map pure <<< attachNativeScript) ns

      cs <- liftM (MintingPolicyHashNotCurrencySymbol mpHash) (mpsSymbol mpHash)
      let value = mkSingletonValue' cs tn
      -- If i is negative we are burning tokens. The tokens burned must
      -- be provided as an input. So we add the value burnt to
      -- 'valueSpentBalancesInputs'. If i is positive then new tokens are
      -- created which must be added to 'valueSpentBalancesOutputs'.
      mintVal <-
        if i < zero then do
          v <- liftM (CannotMakeValue cs tn i) (value $ negate i)
          _valueSpentBalancesInputs <>= provideValue v
          pure $ map getNonAdaAsset $ value i
        else do
          v <- liftM (CannotMakeValue cs tn i) (value i)
          _valueSpentBalancesOutputs <>= provideValue v
          pure $ map getNonAdaAsset $ value i

      _cpsTransaction <<< _body <<< _mint <>= map wrap mintVal

    MustPayToPubKeyAddress pkh skh mDatum scriptRef plutusValue -> do
      networkId <- getNetworkId
      let amount = fromPlutusValue plutusValue
      runExceptT do
        -- If non-inline datum is presented, add it to 'datumWitnesses' and
        -- Array of datums.
        datum' <- for mDatum \(dat /\ datp) -> do
          when (datp == DatumWitness) $ lift $ addDatum dat
          pure $ outputDatum dat datp
        let
          address = case skh of
            Just skh' -> payPubKeyHashBaseAddress networkId pkh skh'
            Nothing -> payPubKeyHashEnterpriseAddress networkId pkh
          txOut = TransactionOutput
            { address
            , amount
            , datum: fromMaybe NoOutputDatum datum'
            , scriptRef: scriptRef
            }
        _cpsTransaction <<< _body <<< _outputs %= Array.(:) txOut
        _valueSpentBalancesOutputs <>= provideValue amount
    MustPayToScript vlh mbCredential dat datp scriptRef plutusValue -> do
      networkId <- getNetworkId
      let amount = fromPlutusValue plutusValue
      runExceptT do
        let
          datum' = outputDatum dat datp
          txOut = TransactionOutput
            { address: case mbCredential of
                Nothing -> validatorHashEnterpriseAddress networkId vlh
                Just cred -> baseAddressToAddress $ baseAddress
                  { network: networkId
                  , paymentCred: scriptHashCredential (unwrap vlh)
                  , delegationCred: credentialToStakeCredential cred
                  }
            , amount
            , datum: datum'
            , scriptRef: scriptRef
            }
        -- Note we don't `addDatum` as this included as part of `mustPayToScript`
        -- constraint already.
        _cpsTransaction <<< _body <<< _outputs %= Array.(:) txOut
        _valueSpentBalancesOutputs <>= provideValue amount
    MustPayToNativeScript nsh mbCredential plutusValue -> do
      networkId <- getNetworkId
      let amount = fromPlutusValue plutusValue
      runExceptT do
        let
          txOut = TransactionOutput
            { address: case mbCredential of
                Nothing -> validatorHashEnterpriseAddress networkId
                  (wrap $ unwrap nsh)
                Just cred -> baseAddressToAddress $ baseAddress
                  { network: networkId
                  , paymentCred: scriptHashCredential (unwrap nsh)
                  , delegationCred: credentialToStakeCredential cred
                  }
            , amount
            , datum: NoOutputDatum
            , scriptRef: Nothing
            }
        _cpsTransaction <<< _body <<< _outputs %= Array.(:) txOut
        _valueSpentBalancesOutputs <>= provideValue amount
    MustHashDatum dh dt -> do
      let dh' = Hashing.datumHash dt
      if dh' == dh then pure <$> addDatum dt
      else pure $ throwError $ DatumWrongHash dh dt
    MustRegisterStakePubKey skh -> runExceptT do
      void $ lift $ addCertificate
        $ StakeRegistration
        $ keyHashCredential
        $ unwrap
        $ unwrap skh
    MustDeregisterStakePubKey pubKey -> runExceptT do
      void $ lift $ addCertificate
        $ StakeDeregistration
        $ keyHashCredential
        $ unwrap
        $ unwrap pubKey
    MustRegisterStakeScript scriptHash -> runExceptT do
      void $ lift $ addCertificate
        $ StakeRegistration
        $ scriptHashCredential
        $ unwrap scriptHash
    MustDeregisterStakePlutusScript plutusScript redeemerData -> runExceptT do
      let
        cert = StakeDeregistration
          ( scriptHashCredential $ unwrap $ plutusScriptStakeValidatorHash
              plutusScript
          )
      _redeemers <>=
        [ UnindexedRedeemer
            { purpose: ForCert cert, datum: unwrap redeemerData }
        ]
      void $ lift $ addCertificate cert
      lift $ attachToCps attachPlutusScript (unwrap plutusScript)
    MustDeregisterStakeNativeScript stakeValidator -> do
      void $ addCertificate $ StakeDeregistration
        $ scriptHashCredential
        $ unwrap
        $ nativeScriptStakeValidatorHash
            stakeValidator
      pure <$> attachToCps (map pure <<< attachNativeScript)
        (unwrap stakeValidator)
    MustRegisterPool poolParams -> runExceptT do
      void $ lift $ addCertificate $ PoolRegistration poolParams
    MustRetirePool poolKeyHash epoch -> runExceptT do
      void $ lift $ addCertificate $ PoolRetirement { poolKeyHash, epoch }
    MustDelegateStakePubKey stakePubKeyHash poolKeyHash -> runExceptT do
      void $ lift $ addCertificate $
        StakeDelegation (keyHashCredential $ unwrap $ unwrap $ stakePubKeyHash)
          poolKeyHash
    MustDelegateStakePlutusScript stakeValidator redeemerData poolKeyHash ->
      runExceptT do
        let
          cert = StakeDelegation
            ( scriptHashCredential $ unwrap $ plutusScriptStakeValidatorHash
                stakeValidator
            )
            poolKeyHash
        lift $ addCertificate cert
        _redeemers <>=
          [ UnindexedRedeemer
              { purpose: ForCert cert, datum: unwrap redeemerData }
          ]
        lift $ attachToCps attachPlutusScript (unwrap stakeValidator)
    MustDelegateStakeNativeScript stakeValidator poolKeyHash -> do
      void $ addCertificate $ StakeDelegation
        ( scriptHashCredential $ unwrap $ nativeScriptStakeValidatorHash
            stakeValidator
        )
        poolKeyHash
      pure <$> attachToCps (map pure <<< attachNativeScript)
        (unwrap stakeValidator)
    MustWithdrawStakePubKey spkh -> runExceptT do
      networkId <- lift getNetworkId
      mbRewards <- lift $ lift $ wrapQueryM $ getPubKeyHashDelegationsAndRewards
        spkh
      ({ rewards }) <- ExceptT $ pure $ note (CannotWithdrawRewardsPubKey spkh)
        mbRewards
      let
        rewardAddress =
          RewardAddress.stakePubKeyHashRewardAddress networkId spkh
      _cpsTransaction <<< _body <<< _withdrawals <<< non Map.empty %=
        Map.insert rewardAddress (fromMaybe (Coin zero) rewards)
    MustWithdrawStakePlutusScript stakeValidator redeemerData -> runExceptT do
      let hash = plutusScriptStakeValidatorHash stakeValidator
      networkId <- lift getNetworkId
      mbRewards <- lift $ lift $ wrapQueryM $
        getValidatorHashDelegationsAndRewards hash
      let
        rewardAddress = RewardAddress.stakeValidatorHashRewardAddress networkId
          hash
      ({ rewards }) <- ExceptT $ pure $ note
        (CannotWithdrawRewardsPlutusScript stakeValidator)
        mbRewards
      _cpsTransaction <<< _body <<< _withdrawals <<< non Map.empty %=
        Map.insert rewardAddress (fromMaybe (Coin zero) rewards)
      _redeemers <>=
        [ UnindexedRedeemer
            { purpose: ForReward rewardAddress, datum: unwrap redeemerData }
        ]
      lift $ attachToCps attachPlutusScript (unwrap stakeValidator)
    MustWithdrawStakeNativeScript stakeValidator -> runExceptT do
      let hash = nativeScriptStakeValidatorHash stakeValidator
      networkId <- lift getNetworkId
      mbRewards <- lift $ lift $ wrapQueryM $
        getValidatorHashDelegationsAndRewards hash
      let
        rewardAddress = RewardAddress.stakeValidatorHashRewardAddress networkId
          hash
      ({ rewards }) <- ExceptT $ pure $ note
        (CannotWithdrawRewardsNativeScript stakeValidator)
        mbRewards
      _cpsTransaction <<< _body <<< _withdrawals <<< non Map.empty %=
        Map.insert rewardAddress (fromMaybe (Coin zero) rewards)
      lift $ attachToCps (map pure <<< attachNativeScript)
        (unwrap stakeValidator)
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
                ( ExceptT $ processConstraint mpsMap osMap constr
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
    :: Datum
    -> DatumPresence
    -> OutputDatum
  outputDatum dat = case _ of
    DatumInline -> OutputDatum dat
    DatumWitness -> OutputDatumHash $ Hashing.datumHash dat

credentialToStakeCredential :: Credential -> StakeCredential
credentialToStakeCredential cred = case cred of
  PubKeyCredential pubKeyHash -> keyHashCredential (unwrap pubKeyHash)
  ScriptCredential scriptHash -> scriptHashCredential (unwrap scriptHash)

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
  (.=) _cpsTransaction newTx

-- Attaches datum to the transaction and to Array of datums in the state.
addDatum
  :: Datum
  -> ConstraintsM Unit
addDatum dat = do
  attachToCps attachDatum dat
  _datums <>= Array.singleton dat

-- | Returns an index pointing to the location of the newly inserted certificate
-- | in the array of transaction certificates.
addCertificate
  :: Certificate
  -> ConstraintsM Unit
addCertificate cert = do
  _cpsTransaction <<< _body <<< _certs <<< non [] %= Array.(:) cert

getNetworkId
  :: ConstraintsM NetworkId
getNetworkId = use (_cpsTransaction <<< _body <<< _networkId)
  >>= maybe (asks _.networkId) pure

mkUnbalancedTxImpl
  :: forall (validator :: Type) (datum :: Type) (redeemer :: Type)
   . ScriptLookups
  -> TxConstraints
  -> Contract (Either MkUnbalancedTxError UnbalancedTx)
mkUnbalancedTxImpl scriptLookups txConstraints =
  runConstraintsM scriptLookups txConstraints <#> map
    \{ transaction, datums, redeemers, usedUtxos } ->
      wrap
        { transaction: stripDatumsRedeemers $ stripScriptDataHash transaction
        , datums
        , redeemers
        , usedUtxos
        }
  where
  stripScriptDataHash :: Transaction -> Transaction
  stripScriptDataHash =
    _body <<< _scriptDataHash .~ Nothing

  stripDatumsRedeemers :: Transaction -> Transaction
  stripDatumsRedeemers = _witnessSet %~
    over TransactionWitnessSet
      _ { plutusData = Nothing, redeemers = Nothing }
