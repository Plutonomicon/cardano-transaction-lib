module Types.ScriptLookups
  ( MkUnbalancedTxError(..)
  , ScriptLookups(..)
  , mintingPolicy
  , mkUnbalancedTx
  , otherData
  , otherScript
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , paymentPubKey
  , typedValidatorLookups
  , unspentOutputs
  ) where

import Prelude hiding (join)
import Address (addressValidatorHash, ogmiosAddressToAddress)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.State.Trans (StateT, get, gets, put, runStateT)
import Data.Array (mapMaybe, singleton) as Array
import Data.Array ((:), concat, elem, fromFoldable, toUnfoldable)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (foldM)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Generic.Rep (class Generic)
import Data.Lattice (join)
import Data.Lens ((%=), (<>=), (.=))
import Data.Lens.At (at)
import Data.Lens.Getter (use)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.List (List(Nil, Cons))
import Data.Map (Map, empty, lookup, mapMaybe, singleton, union, values)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Helpers ((<\>), liftEither, liftM, liftMWith)
import QueryM (QueryConfig, QueryM)
import Scripts
  ( mintingPolicyHash
  , scriptHash
  , validatorHash
  , validatorHashAddress
  )
import Serialization.Address (Address, NetworkId)
import Types.Datum (Datum, DatumHash, datumHash)
import Types.Interval
  ( POSIXTimeRange
  , SlotConfig
  , defaultSlotConfig
  , posixTimeRangeToTransactionSlot
  )
import Types.JsonWsp (OgmiosTxOut)
import Types.Redeemer (Redeemer)
import Types.RedeemerTag (RedeemerTag(Mint, Spend))
import Types.Scripts
  ( MintingPolicy
  , MintingPolicyHash(MintingPolicyHash)
  , TypedValidator(TypedValidator)
  , Validator
  , ValidatorHash
  )
import Data.Symbol (SProxy(SProxy))
import Transaction
  ( ModifyTxError
  , attachDatum
  , attachPlutusScript
  , attachRedeemer
  , setScriptDataHash
  )
import Types.Transaction (Redeemer) as Transaction
import Types.Transaction
  ( Transaction
  , TransactionOutput(TransactionOutput)
  , TxBody
  , _body
  , _inputs
  , _mint
  , _networkId
  , _outputs
  , _plutusScripts
  , _requiredSigners
  , _witnessSet
  )
import Types.TxConstraints
  ( TxConstraint
      ( MustBeSignedBy
      , MustHashDatum
      , MustIncludeDatum
      , MustMintValue
      , MustPayToOtherScript
      , MustPayToPubKeyAddress
      , MustProduceAtLeast
      , MustSatisfyAnyOf
      , MustSpendAtLeast
      , MustSpendPubKeyOutput
      , MustSpendScriptOutput
      , MustValidateIn
      )
  , TxConstraints(TxConstraints)
  )
import Types.UnbalancedTransaction
  ( TxOutRef
  , PaymentPubKey
  , PaymentPubKeyHash
  , StakePubKeyHash
  , UnbalancedTx
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  , payPubKeyHash
  , payPubKeyHashAddress
  , payPubKeyRequiredSigner
  , stakePubKeyHashAddress
  )
import Types.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , isZero
  , mkSingletonValue'
  , mpsSymbol
  , negation
  , split
  )
import TxOutput (ogmiosDatumHashToDatumHash, ogmiosTxOutToScriptOutput)
import Undefined (undefined)

-- Taken mainly from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/Ledger-Constraints-OffChain.html
-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26
-- It should be noted that `ScriptOutput` came later and this was already apart
-- of our codebase so I had to mix & match Plutus revs.

--------------------------------------------------------------------------------
-- Misc. helpers (can remove after `convertRedeemer` defined)
--------------------------------------------------------------------------------
-- TO DO: remove this functionality and convert Redeemers as an Array T.Redeemer
-- https://github.com/Plutonomicon/cardano-browser-tx/issues/156 (closed now)
-- https://github.com/Plutonomicon/cardano-browser-tx/issues/167
convertRedeemer
  :: forall (sl :: Type)
   . Redeemer
  -> ConstraintsM sl Transaction.Redeemer
convertRedeemer = undefined

--------------------------------------------------------------------------------
-- ScriptLookups type
--------------------------------------------------------------------------------
newtype ScriptLookups (a :: Type) = ScriptLookups
  { mps :: Map MintingPolicyHash MintingPolicy -- Minting policies that the script interacts with
  , txOutputs :: Map TxOutRef OgmiosTxOut -- Unspent outputs that the script may want to spend
  , otherScripts :: Map ValidatorHash Validator -- Validators of scripts other than "our script"
  , otherData :: Map DatumHash Datum --  Datums that we might need
  , paymentPubKeyHashes :: Map PaymentPubKeyHash PaymentPubKey -- Public keys that we might need
  , typedValidator :: Maybe (TypedValidator a) -- The script instance with the typed validator hash & actual compiled program
  -- NOTE: not sure how to make sense of Typed Validators ATM. https://github.com/Plutonomicon/cardano-browser-tx/issues/166
  , ownPaymentPubKeyHash :: Maybe PaymentPubKeyHash -- The contract's payment public key hash, used for depositing tokens etc.
  , ownStakePubKeyHash :: Maybe StakePubKeyHash -- The contract's stake public key hash (optional)
  }

derive instance Generic (ScriptLookups a) _
derive instance Newtype (ScriptLookups a) _
derive newtype instance Eq (ScriptLookups a)

instance Show (ScriptLookups a) where
  show = genericShow

-- Using `Data.Map.union`, we can replicate left-biased <> from Data.Map used
-- in Plutus (*not* Plutus' internal Map that uses something like unionWith (<>))
instance Semigroup (ScriptLookups a) where
  append (ScriptLookups l) (ScriptLookups r) =
    ScriptLookups
      { mps: l.mps `union` r.mps
      , txOutputs: l.txOutputs `union` r.txOutputs
      , otherScripts: l.otherScripts `union` r.otherScripts
      , otherData: l.otherData `union` r.otherData
      , paymentPubKeyHashes: l.paymentPubKeyHashes `union` r.paymentPubKeyHashes
      -- 'First' to match the semigroup instance of Map (left-biased)
      , typedValidator: l.typedValidator <\> r.typedValidator
      , ownPaymentPubKeyHash: l.ownPaymentPubKeyHash <\> r.ownPaymentPubKeyHash
      , ownStakePubKeyHash: l.ownStakePubKeyHash <\> r.ownStakePubKeyHash
      }

instance Monoid (ScriptLookups a) where
  mempty = ScriptLookups
    { mps: empty
    , txOutputs: empty
    , otherScripts: empty
    , otherData: empty
    , paymentPubKeyHashes: empty
    , typedValidator: Nothing
    , ownPaymentPubKeyHash: Nothing
    , ownStakePubKeyHash: Nothing
    }

--------------------------------------------------------------------------------
-- Create ScriptLookups helpers for `Contract` monad.
--------------------------------------------------------------------------------
-- https://github.com/Plutonomicon/cardano-browser-tx/issues/166
-- FIX ME: Need to work out what to do with TypedValidator and constraints.
-- Also, some of these functions are in Monadic contexts which differs from
-- Plutus. Not sure if there's anything we can do about this.

-- https://github.com/Plutonomicon/cardano-browser-tx/issues/166
-- FIX ME: this just replicates Plutus API, it's meaningless ATM.
-- | A script lookups value with a script instance. For convenience this also
-- | includes the minting policy script that forwards all checks to the
-- | instance's validator.
typedValidatorLookups :: forall (a :: Type). TypedValidator a -> ScriptLookups a
typedValidatorLookups tv@(TypedValidator inst) =
  over ScriptLookups
    _
      { mps = singleton (inst.forwardingMPSHash) (inst.forwardingMPS)
      , typedValidator = Just tv
      }
    mempty

-- | A script lookups value that uses the map of unspent outputs to resolve
-- | input constraints.
unspentOutputs
  :: forall (a :: Type). Map TxOutRef OgmiosTxOut -> ScriptLookups a
unspentOutputs mp = over ScriptLookups _ { txOutputs = mp } mempty

-- | A script lookups value with a minting policy script. This can fail because
-- | we invoke `mintingPolicyHash`.
mintingPolicy :: forall (a :: Type). MintingPolicy -> Maybe (ScriptLookups a)
mintingPolicy pl = do
  hsh <- mintingPolicyHash pl
  pure $ over ScriptLookups _ { mps = singleton hsh pl } mempty

-- | A script lookups value with a validator script. This can fail because we
-- |invoke `validatorHash`.
otherScript :: forall (a :: Type). Validator -> Maybe (ScriptLookups a)
otherScript vl = do
  vh <- validatorHash vl
  pure $ over ScriptLookups _ { otherScripts = singleton vh vl } mempty

-- | A script lookups value with a datum. This can fail because we invoke
-- | `datumHash`.
otherData :: forall (a :: Type). Datum -> Maybe (ScriptLookups a)
otherData dt = do
  dh <- datumHash dt
  pure $ over ScriptLookups _ { otherData = singleton dh dt } mempty

-- | A script lookups value with a payment public key. This can fail because we
-- | invoke `payPubKeyHash`.
paymentPubKey :: forall (a :: Type). PaymentPubKey -> Maybe (ScriptLookups a)
paymentPubKey ppk = do
  pkh <- payPubKeyHash ppk
  pure $ over ScriptLookups
    _ { paymentPubKeyHashes = singleton pkh ppk }
    mempty

ownPaymentPubKeyHash :: forall (a :: Type). PaymentPubKeyHash -> ScriptLookups a
ownPaymentPubKeyHash pkh =
  over ScriptLookups _ { ownPaymentPubKeyHash = Just pkh } mempty

ownStakePubKeyHash :: forall (a :: Type). StakePubKeyHash -> ScriptLookups a
ownStakePubKeyHash skh =
  over ScriptLookups _ { ownStakePubKeyHash = Just skh } mempty

-- -Note [Balance of value spent]

-- To build a transaction that satisfies the 'MustSpendAtLeast' and
-- `MustProduceAtLeast` constraints, we keep a tally of the required and
-- actual values we encounter on either side of the transaction. Then we
-- compute the missing value on both sides, and add an input with the
-- join of the positive parts of the missing values.

-- | The balances we track for computing the missing 'Value' (if any)
-- | that needs to be added to the transaction.
-- | See note [Balance of value spent].
newtype ValueSpentBalances = ValueSpentBalances
  { required :: Value
  -- Required value spent by the transaction.
  , provided :: Value
  -- Value provided by an input or output of the transaction.
  }

derive instance Generic ValueSpentBalances _

instance Show ValueSpentBalances where
  show = genericShow

instance Semigroup ValueSpentBalances where
  append (ValueSpentBalances l) (ValueSpentBalances r) = ValueSpentBalances
    { required: l.required `join` r.required -- least upper bound on Value
    , provided: l.provided `join` r.provided
    }

-- This is the state for essentially creating an unbalanced transaction.
type ConstraintProcessingState =
  { unbalancedTx :: UnbalancedTx
  -- The unbalanced transaction that we're building
  , mintRedeemers :: Map MintingPolicyHash Redeemer
  -- Redeemers for minting policies
  , valueSpentBalancesInputs :: ValueSpentBalances
  -- Balance of the values given and required for the transaction's inputs
  , valueSpentBalancesOutputs :: ValueSpentBalances
  -- Balance of the values produced and required for the transaction's outputs
  , datums :: Array Datum
  -- *Set* of accumulating datums so we can use `setScriptDataHash`
  , redeemers :: Map RedeemerTag (Array Redeemer)
  -- Map of accumulating redeemers separated by `RedeemerTag`. Note that adding
  -- to this discards the ordering of redeemers from constraints (should that
  -- matter).
  }

-- We could make these signatures polymorphic but they're not exported so don't
-- bother.
_unbalancedTx :: Lens' ConstraintProcessingState UnbalancedTx
_unbalancedTx = prop (SProxy :: SProxy "unbalancedTx")

_mintRedeemers
  :: Lens' ConstraintProcessingState (Map MintingPolicyHash Redeemer)
_mintRedeemers = prop (SProxy :: SProxy "mintRedeemers")

_valueSpentBalancesInputs :: Lens' ConstraintProcessingState ValueSpentBalances
_valueSpentBalancesInputs = prop (SProxy :: SProxy "valueSpentBalancesInputs")

_valueSpentBalancesOutputs :: Lens' ConstraintProcessingState ValueSpentBalances
_valueSpentBalancesOutputs = prop (SProxy :: SProxy "valueSpentBalancesOutputs")

_datums :: Lens' ConstraintProcessingState (Array Datum)
_datums = prop (SProxy :: SProxy "datums")

_redeemers :: Lens' ConstraintProcessingState (Map RedeemerTag (Array Redeemer))
_redeemers = prop (SProxy :: SProxy "redeemers")

missingValueSpent :: ValueSpentBalances -> Value
missingValueSpent (ValueSpentBalances { required, provided }) =
  let
    difference = required <> negation provided
    _ /\ missing = split difference
  in
    missing

totalMissingValue :: ConstraintProcessingState -> Value
totalMissingValue { valueSpentBalancesInputs, valueSpentBalancesOutputs } =
  missingValueSpent valueSpentBalancesInputs `join`
    missingValueSpent valueSpentBalancesOutputs

provide :: Value -> ValueSpentBalances
provide provided = ValueSpentBalances { provided, required: mempty }

require :: Value -> ValueSpentBalances
require required = ValueSpentBalances { required, provided: mempty }

type ConstraintsConfig (sl :: Type) =
  { scriptLookups :: ScriptLookups sl
  , slotConfig :: SlotConfig
  }

-- A `ReaderT` and `StateT` ontop of `QueryM` ~ ReaderT QueryConfig Aff`.
-- The config is `ConstraintsConfig`  which holds the scriptlookups and a
-- `defaultSlotConfig`. The state is `ConstraintProcessingState` which keeps
-- track of the unbalanced transaction etc.
-- We write `ReaderT QueryConfig Aff` below since type synonyms need to be fully
-- applied.
type ConstraintsM (sl :: Type) (a :: Type) =
  ReaderT (ConstraintsConfig sl)
    (StateT ConstraintProcessingState (ReaderT QueryConfig Aff))
    a

-- TO DO: Plutus uses a bunch of FromData and ToData constraints we'll probably
-- need to replicate if we uses `InputConstraint`s and `OutputConstraint`s
-- i.e. ~ foldM addOwnInput txOwnInputs then foldM addOwnOutput txOwnOutputs
-- see https://github.com/Plutonomicon/cardano-browser-tx/issues/166
-- | Resolve some `TxConstraints` by modifying the `UnbalancedTx` in the
-- | `ConstraintProcessingState`
processLookupsAndConstraints
  :: forall (sl :: Type) (o :: Type)
   . TxConstraints o o
  -> ConstraintsM sl (Either MkUnbalancedTxError Unit)
processLookupsAndConstraints (TxConstraints { constraints }) = runExceptT do
  ExceptT $ foldM (\_ constr -> runExceptT $ ExceptT $ processConstraint constr)
    (Right unit)
    constraints
  ExceptT addScriptDataHash
  ExceptT addMintingRedeemers
  ExceptT addMissingValueSpent
  ExceptT updateUtxoIndex

-- Helper to run the stack and get back to `QueryM`.
runConstraintsM
  :: forall (sl :: Type) (o :: Type)
   . ScriptLookups sl
  -> TxConstraints o o
  -> QueryM (Either MkUnbalancedTxError ConstraintProcessingState)
runConstraintsM scriptLookups txConstraints =
  let
    config :: ConstraintsConfig sl
    config = { scriptLookups, slotConfig: defaultSlotConfig }

    initCps :: ConstraintProcessingState
    initCps =
      { unbalancedTx: emptyUnbalancedTx
      , mintRedeemers: empty
      , valueSpentBalancesInputs:
          ValueSpentBalances { required: mempty, provided: mempty }
      , valueSpentBalancesOutputs:
          ValueSpentBalances { required: mempty, provided: mempty }
      , datums: mempty
      , redeemers: empty
      }

    unpackTuple
      :: Either MkUnbalancedTxError Unit /\ ConstraintProcessingState
      -> Either MkUnbalancedTxError ConstraintProcessingState
    unpackTuple (Left err /\ _) = Left err
    unpackTuple (_ /\ cps) = Right cps
  in
    unpackTuple <$>
      ( flip runStateT initCps $ flip runReaderT config $
          processLookupsAndConstraints txConstraints
      )

-- | Create an `UnbalancedTx` given `ScriptLookups` and `TxConstraints`.
mkUnbalancedTx
  :: forall (sl :: Type) (o :: Type)
   . ScriptLookups sl
  -> TxConstraints o o
  -> QueryM (Either MkUnbalancedTxError UnbalancedTx)
mkUnbalancedTx scriptLookups txConstraints =
  runConstraintsM scriptLookups txConstraints <#> map _.unbalancedTx

addScriptDataHash
  :: forall (sl :: Type)
   . ConstraintsM sl (Either MkUnbalancedTxError Unit)
addScriptDataHash = runExceptT do
  dats <- use _datums
  redsMap <- use _redeemers
  tx <- use (_unbalancedTx <<< _transaction)
  -- Note: we don't care about the order of redeemers, hopefully this is not
  -- problematic.
  let reds = concat $ fromFoldable $ values redsMap
  -- FIX ME once we get `convertRedeemer`
  reds' <- ExceptT $ traverse convertRedeemer reds <#> Right
  tx' <- ExceptT $ liftEffect $ setScriptDataHash reds' dats tx <#> Right
  _unbalancedTx <<< _transaction .= tx'

-- | Add the remaining balance of the total value that the tx must spend.
-- | See note [Balance of value spent]
addMissingValueSpent
  :: forall (sl :: Type)
   . ConstraintsM sl (Either MkUnbalancedTxError Unit)
addMissingValueSpent = do
  missing <- gets totalMissingValue
  if isZero missing then pure $ Right unit
  else runExceptT do
    -- add 'missing' to the transaction's outputs. This ensures that the
    -- wallet will add a corresponding input when balancing the
    -- transaction.
    -- Step 4 of the process described in [Balance of value spent]
    lookups <- asks (_.scriptLookups >>> unwrap)
    let
      pkh' = lookups.ownPaymentPubKeyHash
      skh' = lookups.ownStakePubKeyHash
    networkId <- ExceptT getNetworkId
    txOut <- case pkh', skh' of
      Nothing, Nothing -> throwError OwnPubKeyAndStakeKeyMissing
      -- Prioritise pkh:
      Just pkh, _ -> liftEither $ Right $ TransactionOutput
        { address: payPubKeyHashAddress networkId pkh
        , amount: missing
        , data_hash: Nothing
        }
      _, Just skh -> liftEither $ Right $ TransactionOutput
        { address: stakePubKeyHashAddress networkId skh
        , amount: missing
        , data_hash: Nothing
        }
    _cpsToTxBody <<< _outputs %= (:) txOut

-- FIX ME: Conversion of all redeemers will be done together once Haskell
-- server is ready.
addMintingRedeemers
  :: forall (sl :: Type)
   . ConstraintsM sl (Either MkUnbalancedTxError Unit)
addMintingRedeemers = do
  mPs <-
    use (_unbalancedTx <<< _transaction <<< _witnessSet <<< _plutusScripts)
  case mPs of
    Nothing -> pure $ Right unit -- no side effects
    Just ps -> runExceptT do
      -- Convert to CLS-style Redeemers:
      mintReds <- use _mintRedeemers
      reds <- ExceptT $ traverse convertRedeemer mintReds <#> Right
      ExceptT $ foldWithIndexM
        ( \mpsHash _ red -> runExceptT do
            -- POTENTIAL FIX ME: We are wrapping MintingPolicyHash on Array of
            --  generic PlutusScripts mapped to their ScriptHash.
            containsHash <- liftEither $ Right $ elem mpsHash $
              Array.mapMaybe (map MintingPolicyHash <<< scriptHash) ps
            if containsHash then do
              ExceptT $ attachToCps attachRedeemer red
            else liftEither $ throwError $ MintingPolicyNotFound mpsHash
        )
        (Right unit)
        reds

updateUtxoIndex
  :: forall (sl :: Type)
   . ConstraintsM sl (Either MkUnbalancedTxError Unit)
updateUtxoIndex = runExceptT do
  txOutputs <- asks (_.scriptLookups >>> unwrap >>> _.txOutputs)
  let txOutsMap = mapMaybe ogmiosTxOutToScriptOutput txOutputs
  -- Left bias towards original map, hence `flip`:
  _unbalancedTx <<< _utxoIndex %= flip union txOutsMap

data MkUnbalancedTxError
  = ModifyTx ModifyTxError
  | TxOutRefNotFound TxOutRef
  | TxOutRefWrongType TxOutRef
  | DatumNotFound DatumHash
  | MintingPolicyNotFound MintingPolicyHash
  | MintingPolicyHashNotCurrencySymbol MintingPolicyHash
  | CannotMakeValue CurrencySymbol TokenName BigInt
  | ValidatorHashNotFound ValidatorHash
  | NetworkIdMissing
  | OwnPubKeyAndStakeKeyMissing
  -- | TypedValidatorMissing
  | DatumWrongHash DatumHash Datum
  | CannotHashDatum Datum
  | CannotConvertPOSIXTimeRange POSIXTimeRange
  | CannotConvertOgmiosAddress String -- Conversion from an Ogmios Address ~ String to `Address`
  | CannotConvertOgmiosDatumHash String -- Conversion from an Ogmios DatumHash ~ `Maybe String` to` Maybe DatumHash`
  | CannotGetValidatorHashFromAddress Address -- Get `ValidatorHash` from internal `Address`
  | CannotSatisfyAny

derive instance Generic MkUnbalancedTxError _
derive instance Eq MkUnbalancedTxError

instance Show MkUnbalancedTxError where
  show = genericShow

lookupTxOutRef
  :: forall (sl :: Type)
   . TxOutRef
  -> ConstraintsM sl (Either MkUnbalancedTxError OgmiosTxOut)
lookupTxOutRef outRef = do
  txOutputs <- asks (_.scriptLookups >>> unwrap >>> _.txOutputs)
  let err = pure $ throwError $ TxOutRefNotFound outRef
  maybe err (pure <<< Right) $ lookup outRef txOutputs

lookupDatum
  :: forall (sl :: Type)
   . DatumHash
  -> ConstraintsM sl (Either MkUnbalancedTxError Datum)
lookupDatum dh = do
  otherDt <- asks (_.scriptLookups >>> unwrap >>> _.otherData)
  let err = pure $ throwError $ DatumNotFound dh
  maybe err (pure <<< Right) $ lookup dh otherDt

lookupMintingPolicy
  :: forall (sl :: Type)
   . MintingPolicyHash
  -> ConstraintsM sl (Either MkUnbalancedTxError MintingPolicy)
lookupMintingPolicy mph = do
  mps <- asks (_.scriptLookups >>> unwrap >>> _.mps)
  let err = pure $ throwError $ MintingPolicyNotFound mph
  maybe err (pure <<< Right) $ lookup mph mps

lookupValidator
  :: forall (sl :: Type)
   . ValidatorHash
  -> ConstraintsM sl (Either MkUnbalancedTxError Validator)
lookupValidator vh = do
  otherScripts <- asks (_.scriptLookups >>> unwrap >>> _.otherScripts)
  let err = pure $ throwError $ ValidatorHashNotFound vh
  maybe err (pure <<< Right) $ lookup vh otherScripts

-- | Modify the `UnbalancedTx` so that it satisfies the constraints, if
-- | possible. Fails if a hash is missing from the lookups, or if an output
-- | of the wrong type is spent.
processConstraint
  :: forall (sl :: Type)
   . TxConstraint
  -> ConstraintsM sl (Either MkUnbalancedTxError Unit)
processConstraint = do
  case _ of
    MustIncludeDatum datum -> addDatums datum
    MustValidateIn posixTimeRange -> runExceptT do
      sc <- asks _.slotConfig
      case posixTimeRangeToTransactionSlot sc posixTimeRange of
        Nothing ->
          liftEither $ throwError $ CannotConvertPOSIXTimeRange posixTimeRange
        Just { timeToLive, validityStartInterval } ->
          _cpsToTxBody <<< _Newtype %=
            _
              { ttl = timeToLive
              , validity_start_interval = validityStartInterval
              }
    MustBeSignedBy pkh -> runExceptT do
      ppkh <- asks (_.scriptLookups >>> unwrap >>> _.paymentPubKeyHashes)
      let sigs = lookup pkh ppkh <#> payPubKeyRequiredSigner >>> Array.singleton
      _cpsToTxBody <<< _requiredSigners <>= sigs
    MustSpendAtLeast vl ->
      runExceptT $ _valueSpentBalancesInputs <>= require vl
    MustProduceAtLeast vl ->
      runExceptT $ _valueSpentBalancesOutputs <>= require vl
    MustSpendPubKeyOutput txo -> runExceptT do
      txOut <- ExceptT $ lookupTxOutRef txo
      -- Recall an Ogmios datum is a `Maybe String` where `Nothing` implies a
      -- wallet address and `Just` as script address.
      case txOut of
        { value, datum: Nothing } -> do
          -- POTENTIAL FIX ME: Plutus has Tx.TxIn and Tx.PubKeyTxIn -- TxIn
          -- keeps track TxOutRef and TxInType (the input type, whether
          -- consuming script, public key or simple script)
          _cpsToTxBody <<< _inputs %= (:) txo
          _valueSpentBalancesInputs <>= provide value
        _ -> liftEither $ throwError $ TxOutRefWrongType txo
    MustSpendScriptOutput txo red -> runExceptT do
      txOut <- ExceptT $ lookupTxOutRef txo
      -- Recall an Ogmios datum is a `Maybe String` where `Nothing` implies a
      -- wallet address and `Just` as script address.
      case txOut of
        { address, datum: Just datumStr, value } -> do
          -- Convert the address into a validator hash to use the lookup
          address' <- liftM
            (CannotConvertOgmiosAddress address)
            (ogmiosAddressToAddress address)
          vHash <- liftM
            (CannotGetValidatorHashFromAddress address')
            (addressValidatorHash address')
          plutusScript <- ExceptT $ lookupValidator vHash <#> map unwrap
          -- Note: Plutus uses `TxIn` to attach a redeemer and datum.
          -- Use the datum hash inside the lookup
          -- Note: if we get `Nothing`, we have to throw eventhough that's a
          -- valid input, because our `txOut` above is a Script address via
          -- `Just`.
          dHash <- liftM
            (CannotConvertOgmiosDatumHash datumStr)
            (ogmiosDatumHashToDatumHash datumStr)
          dataValue <- ExceptT $ lookupDatum dHash
          ExceptT $ attachToCps attachPlutusScript plutusScript
          ExceptT $ addDatums dataValue
          -- FIX ME: we could conveniently convert redeemers as we go along
          -- so the state contains Transaction.Redeemer instead.
          -- redT <- ExceptT $ Right <$> convertRedeemer red
          -- redCps <- ExceptT $ attachToCps attachRedeemer redT
          _cpsToTxBody <<< _inputs %= (:) txo
          _valueSpentBalancesInputs <>= provide value
          -- Append redeemers for spending.
          _redeemers <<< at Spend <>= Just (Array.singleton red)
        _ -> liftEither $ throwError $ TxOutRefWrongType txo
    MustMintValue mpsHash red tn i -> runExceptT do
      plutusScript <- ExceptT $ lookupMintingPolicy mpsHash <#> map unwrap
      cs <-
        liftM (MintingPolicyHashNotCurrencySymbol mpsHash) (mpsSymbol mpsHash)
      let value = mkSingletonValue' cs tn
      -- If i is negative we are burning tokens. The tokens burned must
      -- be provided as an input. So we add the value burnt to
      -- 'valueSpentBalancesInputs'. If i is positive then new tokens are
      -- created which must be added to 'valueSpentBalancesOutputs'.
      mintVal <-
        if i < zero then do
          v <- liftM (CannotMakeValue cs tn i) (value $ negate i)
          _valueSpentBalancesInputs <>= provide v
          liftEither $ Right v
        else do
          v <- liftM (CannotMakeValue cs tn i) (value i)
          _valueSpentBalancesOutputs <>= provide v
          liftEither $ Right v
      ExceptT $ attachToCps attachPlutusScript plutusScript
      _cpsToTxBody <<< _mint <>= Just (wrap mintVal)
      _mintRedeemers <<< at mpsHash .= Just red
      _redeemers <<< at Mint <>= Just (Array.singleton red)
    MustPayToPubKeyAddress pkh _ mDatum amount -> runExceptT do
      -- If datum is presented, add it to 'datumWitnesses' and Array of datums.
      -- Otherwise continue, hence `liftEither $ Right unit`.
      maybe (liftEither $ Right unit) (ExceptT <<< addDatums) mDatum
      networkId <- ExceptT getNetworkId
      -- [DatumHash Note]
      -- The behaviour below is subtle because of `datumHash`'s `Maybe` context.
      -- In particular, if `mDatum` is `Nothing`, then return nothing (note: we
      -- don't want to fail). However, if we have a datum value, we attempt to
      -- hash, which may fail. We want to capture this failure.
      -- Given `data_hash` ~ `Maybe DatumHash`, we don't want return this
      -- failure in the output. It's possible that this is okay for
      -- `MustPayToPubKeyAddress` because datums are essentially redundant
      -- for wallet addresses, but let's fail for now. It is important to
      -- capture failure for `MustPayToOtherScript` however, because datums
      -- at script addresses matter.
      -- e.g. in psuedo code:
      -- If mDatum = Nothing -> data_hash = Nothing (don't fail)
      -- If mDatum = Just datum ->
      --     If datumHash datum = Nothing -> FAIL
      --     If datumHash datum = Just dHash -> data_hash = dHash
      -- As mentioned, we could remove this fail behaviour for
      -- `MustPayToPubKeyAddress`
      data_hash <- maybe
        (liftEither $ Right Nothing) -- Don't throw an error if Nothing.
        (\datum -> liftMWith (CannotHashDatum datum) Just (datumHash datum))
        mDatum
      let
        txOut = TransactionOutput
          { address: payPubKeyHashAddress networkId pkh, amount, data_hash }
      _cpsToTxBody <<< _outputs %= (:) txOut
      _valueSpentBalancesOutputs <>= provide amount
    MustPayToOtherScript vlh datum amount -> runExceptT do
      networkId <- ExceptT getNetworkId
      -- Don't write `let data_hash = datumHash datum`, see [datumHash Note]
      data_hash <- liftM (CannotHashDatum datum) (datumHash datum <#> Just)
      let
        txOut = TransactionOutput
          { address: validatorHashAddress networkId vlh
          , amount
          , data_hash
          }
      ExceptT $ addDatums datum
      _cpsToTxBody <<< _outputs %= (:) txOut
      _valueSpentBalancesOutputs <>= provide amount
    MustHashDatum dh dt ->
      if datumHash dt == Just dh then runExceptT do
        ExceptT $ attachToCps attachDatum dt
        _datums %= (:) dt
      else pure $ throwError $ DatumWrongHash dh dt
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
          -> ConstraintsM sl (Either MkUnbalancedTxError Unit)
        tryNext Nil = pure $ throwError CannotSatisfyAny
        tryNext (Cons ys zs) =
          -- Note this implicitly resets state to original cps upon failure (see
          -- `put`)
          foldM
            ( \_ constr -> runExceptT do
                ExceptT $ processConstraint constr
                  `catchError` \_ -> put cps *> tryNext zs
            )
            (Right unit)
            ys
      tryNext (toUnfoldable $ map toUnfoldable xs)
  where
  -- Attachs datum to the transaction and to Array of datums in the state.
  addDatums :: Datum -> ConstraintsM sl (Either MkUnbalancedTxError Unit)
  addDatums datum = runExceptT do
    ExceptT $ attachToCps attachDatum datum
    _datums %= (:) datum

-- Attach a Datum, Redeemer, or PlutusScript depending on the handler. They
-- share error type anyway.
attachToCps
  :: forall (a :: Type) (sl :: Type)
   . (a -> Transaction -> Effect (Either ModifyTxError Transaction))
  -> a -- Redeemer, Datum, or PlutusScript.
  -> ConstraintsM sl (Either MkUnbalancedTxError Unit)
attachToCps handler object = do
  tx <- use (_unbalancedTx <<< _transaction)
  newTx <- liftEffect $ handler object tx <#> lmap ModifyTx
  either
    (pure <<< throwError)
    (map Right <<< (.=) (_unbalancedTx <<< _transaction))
    newTx

-- Helper to focus from `ConstraintProcessingState` down to `TxBody`.
_cpsToTxBody :: Lens' ConstraintProcessingState TxBody
_cpsToTxBody = _unbalancedTx <<< _transaction <<< _body

getNetworkId
  :: forall (sl :: Type)
   . ConstraintsM sl (Either MkUnbalancedTxError NetworkId)
getNetworkId = runExceptT $
  use (_cpsToTxBody <<< _networkId) >>= liftM NetworkIdMissing
