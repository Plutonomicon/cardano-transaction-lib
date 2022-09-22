module Ctl.Internal.Types.ScriptLookups
  ( MkUnbalancedTxError
      ( CannotConvertPOSIXTimeRange
      , CannotConvertPaymentPubKeyHash
      , CannotFindDatum
      , CannotGetMintingPolicyScriptIndex
      , CannotGetValidatorHashFromAddress
      , CannotHashDatum
      , CannotHashMintingPolicy
      , CannotHashValidator
      , CannotMakeValue
      , CannotQueryDatum
      , CannotSatisfyAny
      , DatumNotFound
      , DatumWrongHash
      , MintingPolicyHashNotCurrencySymbol
      , MintingPolicyNotFound
      , MkTypedTxOutFailed
      , ModifyTx
      , OwnPubKeyAndStakeKeyMissing
      , TxOutRefNotFound
      , TxOutRefWrongType
      , TypeCheckFailed
      , TypedTxOutHasNoDatumHash
      , TypedValidatorMissing
      , ValidatorHashNotFound
      , WrongRefScriptHash
      )
  , ScriptLookups(ScriptLookups)
  , UnattachedUnbalancedTx(UnattachedUnbalancedTx)
  , generalise
  , mintingPolicy
  , mintingPolicyM
  , mkUnbalancedTx
  , mkUnbalancedTx'
  , datum
  , validator
  , validatorM
  , ownPaymentPubKeyHash
  , ownPaymentPubKeyHashM
  , ownStakePubKeyHash
  , ownStakePubKeyHashM
  , typedValidatorLookups
  , typedValidatorLookupsM
  , unspentOutputs
  , unspentOutputsM
  ) where

import Prelude hiding (join)

import Aeson (class EncodeAeson)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, liftMaybe, throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Trans (StateT, get, gets, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Address (enterpriseAddressValidatorHash)
import Ctl.Internal.Cardano.Types.Transaction
  ( Costmdls
  , Transaction
  , TransactionOutput(TransactionOutput)
  , TransactionWitnessSet(TransactionWitnessSet)
  , TxBody
  , _body
  , _inputs
  , _isValid
  , _mint
  , _networkId
  , _outputs
  , _referenceInputs
  , _requiredSigners
  , _scriptDataHash
  , _witnessSet
  )
import Ctl.Internal.Cardano.Types.Transaction (Redeemer(Redeemer)) as T
import Ctl.Internal.Cardano.Types.Value
  ( CurrencySymbol
  , Value
  , getNonAdaAsset
  , isZero
  , mkSingletonValue'
  , mpsSymbol
  , negation
  , split
  )
import Ctl.Internal.Hashing (datumHash) as Hashing
import Ctl.Internal.Helpers (liftM, (<\>))
import Ctl.Internal.IsData (class IsData)
import Ctl.Internal.QueryM (QueryM, QueryMExtended, getDatumByHash)
import Ctl.Internal.QueryM.EraSummaries (getEraSummaries)
import Ctl.Internal.QueryM.ProtocolParameters (getProtocolParameters)
import Ctl.Internal.QueryM.SystemStart (getSystemStart)
import Ctl.Internal.Scripts
  ( mintingPolicyHash
  , nativeScriptHashEnterpriseAddress
  , validatorHash
  , validatorHashEnterpriseAddress
  )
import Ctl.Internal.Serialization.Address (Address, NetworkId)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Transaction
  ( ModifyTxError
  , attachDatum
  , attachNativeScript
  , attachPlutusScript
  , attachRedeemer
  , setScriptDataHash
  )
import Ctl.Internal.Types.Any (Any)
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Ctl.Internal.Types.Interval
  ( POSIXTimeRange
  , PosixTimeToSlotError
  , posixTimeRangeToTransactionValidity
  )
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  )
import Ctl.Internal.Types.PubKeyHash
  ( PaymentPubKeyHash
  , StakePubKeyHash
  , payPubKeyHashBaseAddress
  , payPubKeyHashEnterpriseAddress
  , stakePubKeyHashRewardAddress
  )
import Ctl.Internal.Types.RedeemerTag (RedeemerTag(Mint, Spend))
import Ctl.Internal.Types.Scripts
  ( MintingPolicy
  , MintingPolicyHash
  , Validator
  , ValidatorHash
  )
import Ctl.Internal.Types.TokenName (TokenName)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Ctl.Internal.Types.TxConstraints
  ( DatumPresence(DatumWitness, DatumInline)
  , InputConstraint(InputConstraint)
  , InputWithScriptRef(RefInput, SpendInput)
  , OutputConstraint(OutputConstraint)
  , TxConstraint
      ( MustBeSignedBy
      , MustHashDatum
      , MustIncludeDatum
      , MustMintValue
      , MustNotBeValid
      , MustPayToNativeScript
      , MustPayToPubKeyAddress
      , MustPayToScript
      , MustProduceAtLeast
      , MustReferenceOutput
      , MustSatisfyAnyOf
      , MustSpendAtLeast
      , MustSpendNativeScriptOutput
      , MustSpendPubKeyOutput
      , MustSpendScriptOutput
      , MustValidateIn
      )
  , TxConstraints(TxConstraints)
  )
import Ctl.Internal.Types.TypedTxOut
  ( TypeCheckError
  , mkTypedTxOut
  , typeTxOutRef
  , typedTxOutDatumHash
  , typedTxOutRefValue
  , typedTxOutTxOut
  )
import Ctl.Internal.Types.TypedValidator
  ( class DatumType
  , class ValidatorTypes
  , TypedValidator(TypedValidator)
  )
import Ctl.Internal.Types.TypedValidator (generalise) as TV
import Ctl.Internal.Types.UnbalancedTransaction
  ( PaymentPubKey
  , UnbalancedTx
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  )
import Ctl.Plutus.Conversion
  ( fromPlutusTxOutputWithRefScript
  , fromPlutusValue
  )
import Ctl.Plutus.Types.Transaction (TransactionOutputWithRefScript) as Plutus
import Ctl.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Data.Array (filter, mapWithIndex, toUnfoldable, zip)
import Data.Array (singleton, union, (:)) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt)
import Data.Either (Either(Left, Right), either, note)
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Lattice (join)
import Data.Lens ((%=), (%~), (.=), (.~), (<>=))
import Data.Lens.Getter (to, use)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens')
import Data.List (List(Nil, Cons))
import Data.Map (Map, empty, fromFoldable, lookup, singleton, union)
import Data.Map (insert, toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Set (insert) as Set
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(SProxy))
import Data.Traversable (for, traverse_)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Type.Proxy (Proxy(Proxy))

-- Taken mainly from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/Ledger-Constraints-OffChain.html
-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26
-- It should be noted that `ScriptOutput` came later and this was already apart
-- of our codebase so I had to mix & match Plutus revs.

--------------------------------------------------------------------------------
-- ScriptLookups type
--------------------------------------------------------------------------------
-- We write `mps` and `scripts` as an `Array` instead of `Map`, meaning
-- our lookup helpers aren't required to hash (`mintingPolicy`, `validator`)
-- and therefore not lifted to `QueryM`. The downside is the lookups contain
-- less information. All hashing is done inside `ConstraintsM`, see
-- `processLookupsAndConstraints`.
-- The lookups uses the Plutus type `TransactionOutput` and does internal
-- conversions to the Serialization/Cardano to append to the `TxBody` as needed.
newtype ScriptLookups (a :: Type) = ScriptLookups
  { mps ::
      Array MintingPolicy -- Minting policies that the script interacts with
  , txOutputs ::
      Map TransactionInput Plutus.TransactionOutputWithRefScript -- Unspent outputs that the script may want to spend
  , scripts ::
      Array Validator -- Script validators
  , datums :: Map DataHash Datum --  Datums that we might need
  -- FIXME there's currently no way to set this field
  -- See https://github.com/Plutonomicon/cardano-transaction-lib/issues/569
  , paymentPubKeyHashes ::
      Map PaymentPubKeyHash PaymentPubKey -- Public keys that we might need
  , typedValidator ::
      Maybe (TypedValidator a) -- The script instance with the typed validator hash & actual compiled program
  , ownPaymentPubKeyHash ::
      Maybe PaymentPubKeyHash -- The contract's payment public key hash, used for depositing tokens etc.
  , ownStakePubKeyHash ::
      Maybe StakePubKeyHash -- The contract's stake public key hash (optional)
  }

derive instance Generic (ScriptLookups a) _
derive instance Newtype (ScriptLookups a) _
derive newtype instance Eq (ScriptLookups a)

instance Show (ScriptLookups a) where
  show = genericShow

generalise :: forall (a :: Type). ScriptLookups a -> ScriptLookups Any
generalise (ScriptLookups sl) =
  let
    tv = TV.generalise <$> sl.typedValidator
  in
    wrap sl { typedValidator = tv }

-- Using `Data.Map.union`, we can replicate left-biased <> from Data.Map used
-- in Plutus (*not* Plutus' internal Map that uses something like unionWith (<>))
instance Semigroup (ScriptLookups a) where
  append (ScriptLookups l) (ScriptLookups r) =
    ScriptLookups
      { mps: l.mps `Array.union` r.mps
      , txOutputs: l.txOutputs `union` r.txOutputs
      , scripts: l.scripts `Array.union` r.scripts
      , datums: l.datums `union` r.datums
      , paymentPubKeyHashes: l.paymentPubKeyHashes `union` r.paymentPubKeyHashes
      -- 'First' to match the semigroup instance of Map (left-biased)
      , typedValidator: l.typedValidator <\> r.typedValidator
      , ownPaymentPubKeyHash: l.ownPaymentPubKeyHash <\> r.ownPaymentPubKeyHash
      , ownStakePubKeyHash: l.ownStakePubKeyHash <\> r.ownStakePubKeyHash
      }

instance Monoid (ScriptLookups a) where
  mempty = ScriptLookups
    { mps: mempty
    , txOutputs: empty
    , scripts: mempty
    , datums: empty
    , paymentPubKeyHashes: empty
    , typedValidator: Nothing
    , ownPaymentPubKeyHash: Nothing
    , ownStakePubKeyHash: Nothing
    }

--------------------------------------------------------------------------------
-- Create ScriptLookups helpers
--------------------------------------------------------------------------------
-- | The lookup functions come in pairs with the exception of `datum`.
-- | If the function cannot fail, there is another version contained in a `Maybe`
-- | context (that also does not fail).
-- | This is to aid users who wish to utilise the underlying `ScriptLookups`
-- | `Monoid` for `foldMap` etc.
-- |
-- | Otherwise, there are lookups that may fail with `Maybe` (because of
-- | hashing) and an unsafe counterpart via `fromJust`.

-- | A script lookups value with a script instance. For convenience this also
-- | includes the minting policy script that forwards all checks to the
-- | instance's validator.
typedValidatorLookups :: forall (a :: Type). TypedValidator a -> ScriptLookups a
typedValidatorLookups tv@(TypedValidator inst) =
  over ScriptLookups
    _
      { mps = Array.singleton inst.forwardingMPS
      , typedValidator = Just tv
      }
    mempty

-- | Same as `typedValidatorLookups` but in `Maybe` context for convenience.
-- | This should not fail.
typedValidatorLookupsM
  :: forall (a :: Type). TypedValidator a -> Maybe (ScriptLookups a)
typedValidatorLookupsM = pure <<< typedValidatorLookups

-- FIX ME: https://github.com/Plutonomicon/cardano-transaction-lib/issues/200
-- | A script lookups value that uses the map of unspent outputs to resolve
-- | input constraints.
unspentOutputs
  :: forall (a :: Type)
   . Map TransactionInput Plutus.TransactionOutputWithRefScript
  -> ScriptLookups a
unspentOutputs mp = over ScriptLookups _ { txOutputs = mp } mempty

-- FIX ME: https://github.com/Plutonomicon/cardano-transaction-lib/issues/200
-- | Same as `unspentOutputs` but in `Maybe` context for convenience. This
-- | should not fail.
unspentOutputsM
  :: forall (a :: Type)
   . Map TransactionInput Plutus.TransactionOutputWithRefScript
  -> Maybe (ScriptLookups a)
unspentOutputsM = pure <<< unspentOutputs

-- | A script lookups value with a minting policy script.
mintingPolicy :: forall (a :: Type). MintingPolicy -> ScriptLookups a
mintingPolicy pl = over ScriptLookups _ { mps = Array.singleton pl } mempty

-- | Same as `mintingPolicy` but in `Maybe` context for convenience. This
-- | should not fail.
mintingPolicyM :: forall (a :: Type). MintingPolicy -> Maybe (ScriptLookups a)
mintingPolicyM = pure <<< mintingPolicy

-- | A script lookups value with a validator script.
validator :: forall (a :: Type). Validator -> ScriptLookups a
validator vl =
  over ScriptLookups _ { scripts = Array.singleton vl } mempty

-- | Same as `validator` but in `Maybe` context for convenience. This
-- | should not fail.
validatorM :: forall (a :: Type). Validator -> Maybe (ScriptLookups a)
validatorM = pure <<< validator

-- | A script lookups value with a datum.
datum :: forall (a :: Type). Datum -> Maybe (ScriptLookups a)
datum dt =
  Hashing.datumHash dt
    <#> \dh -> over ScriptLookups _ { datums = singleton dh dt } mempty

-- | Add your own `PaymentPubKeyHash` to the lookup.
ownPaymentPubKeyHash :: forall (a :: Type). PaymentPubKeyHash -> ScriptLookups a
ownPaymentPubKeyHash pkh =
  over ScriptLookups _ { ownPaymentPubKeyHash = Just pkh } mempty

-- | Same as `ownPaymentPubKeyHash` but in `Maybe` context for convenience. This
-- | should not fail.
ownPaymentPubKeyHashM
  :: forall (a :: Type). PaymentPubKeyHash -> Maybe (ScriptLookups a)
ownPaymentPubKeyHashM = pure <<< ownPaymentPubKeyHash

-- | Add your own `StakePubKeyHash` to the lookup.
ownStakePubKeyHash :: forall (a :: Type). StakePubKeyHash -> ScriptLookups a
ownStakePubKeyHash skh =
  over ScriptLookups _ { ownStakePubKeyHash = Just skh } mempty

-- | Same as `ownStakePubKeyHash` but in `Maybe` context for convenience. This
-- | should not fail.
ownStakePubKeyHashM
  :: forall (a :: Type). StakePubKeyHash -> Maybe (ScriptLookups a)
ownStakePubKeyHashM = pure <<< ownStakePubKeyHash

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
type ConstraintProcessingState (a :: Type) =
  { unbalancedTx :: UnbalancedTx
  -- The unbalanced transaction that we're building
  , valueSpentBalancesInputs :: ValueSpentBalances
  -- Balance of the values given and required for the transaction's inputs
  , valueSpentBalancesOutputs :: ValueSpentBalances
  -- Balance of the values produced and required for the transaction's outputs
  , datums :: Array Datum
  -- Ordered accumulation of datums so we can use to `setScriptDataHash`
  , redeemersTxIns :: Array (T.Redeemer /\ Maybe TransactionInput)
  -- Ordered accumulation of redeemers so we can use to `setScriptDataHash` and
  -- add execution units via Ogmios
  , mintRedeemers :: Map MintingPolicyHash T.Redeemer
  -- Mint redeemers with the corresponding minting policy hashes.
  -- We need to reindex mint redeemers every time we add a new one, since
  -- indexing relies on policy id ordering.
  , lookups :: ScriptLookups a
  -- ScriptLookups for resolving constraints. Should be treated as an immutable
  -- value despite living inside the processing state
  , costModels :: Costmdls
  }

-- We could make these signatures polymorphic but they're not exported so don't
-- bother.
_unbalancedTx
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) UnbalancedTx
_unbalancedTx = prop (SProxy :: SProxy "unbalancedTx")

_valueSpentBalancesInputs
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) ValueSpentBalances
_valueSpentBalancesInputs = prop (SProxy :: SProxy "valueSpentBalancesInputs")

_valueSpentBalancesOutputs
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) ValueSpentBalances
_valueSpentBalancesOutputs = prop (SProxy :: SProxy "valueSpentBalancesOutputs")

_datums
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) (Array Datum)
_datums = prop (SProxy :: SProxy "datums")

_costModels
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) Costmdls
_costModels = prop (SProxy :: SProxy "costModels")

_redeemersTxIns
  :: forall (a :: Type)
   . Lens' (ConstraintProcessingState a)
       (Array (T.Redeemer /\ Maybe TransactionInput))
_redeemersTxIns = prop (SProxy :: SProxy "redeemersTxIns")

_mintRedeemers
  :: forall (a :: Type)
   . Lens' (ConstraintProcessingState a) (Map MintingPolicyHash T.Redeemer)
_mintRedeemers = prop (SProxy :: SProxy "mintRedeemers")

_lookups
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) (ScriptLookups a)
_lookups = prop (SProxy :: SProxy "lookups")

missingValueSpent :: ValueSpentBalances -> Value
missingValueSpent (ValueSpentBalances { required, provided }) =
  let
    difference = required <> negation provided
    _ /\ missing = split difference
  in
    missing

totalMissingValue :: forall (a :: Type). ConstraintProcessingState a -> Value
totalMissingValue { valueSpentBalancesInputs, valueSpentBalancesOutputs } =
  missingValueSpent valueSpentBalancesInputs `join`
    missingValueSpent valueSpentBalancesOutputs

provideValue :: Value -> ValueSpentBalances
provideValue provided = ValueSpentBalances { provided, required: mempty }

requireValue :: Value -> ValueSpentBalances
requireValue required = ValueSpentBalances { required, provided: mempty }

-- A `StateT` ontop of `QueryM` ~ ReaderT QueryConfig Aff`.
-- The state is `ConstraintProcessingState`, which keeps track of the unbalanced
-- transaction etc and additionally holds a `ConstraintsConfig` containing the
-- scriptlookups and a `defaultSlotConfig`.
-- We write `ReaderT QueryConfig Aff` below since type synonyms need to be fully
-- applied.
type ConstraintsM (a :: Type) (b :: Type) =
  StateT (ConstraintProcessingState a) (QueryMExtended ()) b

-- The constraints don't precisely match those of Plutus:
-- `forall v. (FromData (DatumType v), ToData (DatumType v), ToData (RedeemerType v))`
-- as we don't have the same granularity on the classes, but the type `v` fixes
-- types `d` and `r` as seen below. We could alternatively create specific typeclasses:
-- ToData (DatumType v) <-> (DatumType v d, ToData d) <= ToDataDatumType v d
-- if we require granular control, similarly FromDataDatumType v d etc.
-- We could use `MonadError` to clean up the `ExceptT`s below although we can't
-- use the type alias because they need to be fully applied so this is perhaps
-- more readable.
-- Fix me: add execution units from Ogmios where this function should be
-- inside QueryM https://github.com/Plutonomicon/cardano-transaction-lib/issues/174
-- | Resolve some `TxConstraints` by modifying the `UnbalancedTx` in the
-- | `ConstraintProcessingState`
processLookupsAndConstraints
  :: forall (validator :: Type) (datum :: Type) (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => TxConstraints redeemer datum
  -> ConstraintsM validator (Either MkUnbalancedTxError Unit)
processLookupsAndConstraints
  (TxConstraints { constraints, ownInputs, ownOutputs }) = runExceptT do
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

  ExceptT $ foldConstraints (processConstraint mpsMap osMap) constraints

  -- Attach mint redeemers to witness set.
  mintRedeemers :: Array _ <- use _mintRedeemers <#> Map.toUnfoldable
  lift $ traverse_ (attachToCps attachRedeemer <<< snd) mintRedeemers

  ExceptT $ foldConstraints (addOwnInput (Proxy :: Proxy datum)) ownInputs
  ExceptT $ foldConstraints addOwnOutput ownOutputs
  ExceptT addScriptDataHash
  ExceptT addMissingValueSpent
  ExceptT updateUtxoIndex
  where
  -- Don't write the output in terms of ExceptT because we can't write a
  -- partially applied `ConstraintsM` meaning this is more readable.
  foldConstraints
    :: forall (constr :: Type) (c :: Type)
     . (constr -> ConstraintsM c (Either MkUnbalancedTxError Unit))
    -> Array constr
    -> ConstraintsM c (Either MkUnbalancedTxError Unit)
  foldConstraints handler =
    runExceptT <<< traverse_ (ExceptT <<< handler)

-- Helper to run the stack and get back to `QueryM`. See comments in
-- `processLookupsAndConstraints` regarding constraints.
runConstraintsM
  :: forall (validator :: Type) (datum :: Type) (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups validator
  -> TxConstraints redeemer datum
  -> QueryM (Either MkUnbalancedTxError (ConstraintProcessingState validator))
runConstraintsM lookups txConstraints = do
  costModels <- getProtocolParameters <#> unwrap >>> _.costModels
  let
    initCps :: ConstraintProcessingState validator
    initCps =
      { unbalancedTx: emptyUnbalancedTx
      , valueSpentBalancesInputs:
          ValueSpentBalances { required: mempty, provided: mempty }
      , valueSpentBalancesOutputs:
          ValueSpentBalances { required: mempty, provided: mempty }
      , datums: mempty
      , redeemersTxIns: mempty
      , mintRedeemers: empty
      , lookups
      , costModels
      }

    unpackTuple
      :: Either MkUnbalancedTxError Unit /\
           (ConstraintProcessingState validator)
      -> Either MkUnbalancedTxError (ConstraintProcessingState validator)
    unpackTuple (Left err /\ _) = Left err
    unpackTuple (_ /\ cps) = Right cps
  unpackTuple <$>
    flip runStateT initCps (processLookupsAndConstraints txConstraints)

-- See comments in `processLookupsAndConstraints` regarding constraints.
-- | Create an `UnbalancedTx` given `ScriptLookups` and `TxConstraints`.
mkUnbalancedTx'
  :: forall (validator :: Type) (datum :: Type) (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups validator
  -> TxConstraints redeemer datum
  -> QueryM (Either MkUnbalancedTxError UnbalancedTx)
mkUnbalancedTx' scriptLookups txConstraints =
  runConstraintsM scriptLookups txConstraints <#> map _.unbalancedTx

-- | A newtype for the unbalanced transaction after creating one with datums
-- | and redeemers not attached
newtype UnattachedUnbalancedTx = UnattachedUnbalancedTx
  { unbalancedTx :: UnbalancedTx -- the unbalanced tx created
  , datums :: Array Datum -- the array of ordered datums that require attaching
  , redeemersTxIns ::
      Array (T.Redeemer /\ Maybe TransactionInput) -- the array of
  -- ordered redeemers that require attaching alongside a potential transaction
  -- input. The input is required to determine the index of `Spend` redeemers
  -- after balancing (when inputs are finalised). The potential input is
  -- `Just` for spending script utxos, i.e. `MustSpendScriptOutput` and
  -- `Nothing` otherwise.
  }

derive instance Generic UnattachedUnbalancedTx _
derive instance Newtype UnattachedUnbalancedTx _
derive newtype instance Eq UnattachedUnbalancedTx
derive newtype instance EncodeAeson UnattachedUnbalancedTx

instance Show UnattachedUnbalancedTx where
  show = genericShow

-- | An implementation that strips `witnessSet` and data hash from
-- | returned `UnbalancedTx` in order to calculate them later on server.
-- | It returns part of the `ConstraintProcessingState` for later consumption by
-- | the server. The `Spend` redeemers will require reindexing and all hardcoded
-- | to `zero` from this function.
mkUnbalancedTx
  :: forall (validator :: Type) (datum :: Type) (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups validator
  -> TxConstraints redeemer datum
  -> QueryM (Either MkUnbalancedTxError UnattachedUnbalancedTx)
mkUnbalancedTx scriptLookups txConstraints =
  runConstraintsM scriptLookups txConstraints <#> map
    \{ unbalancedTx, datums, redeemersTxIns } ->
      let
        stripScriptDataHash :: UnbalancedTx -> UnbalancedTx
        stripScriptDataHash uTx =
          uTx # _transaction <<< _body <<< _scriptDataHash .~ Nothing

        stripDatumsRedeemers :: UnbalancedTx -> UnbalancedTx
        stripDatumsRedeemers uTx = uTx # _transaction <<< _witnessSet %~
          over TransactionWitnessSet
            _ { plutusData = Nothing, redeemers = Nothing }
        tx = stripDatumsRedeemers $ stripScriptDataHash unbalancedTx
      in
        wrap { unbalancedTx: tx, datums, redeemersTxIns }

addScriptDataHash
  :: forall (a :: Type)
   . ConstraintsM a (Either MkUnbalancedTxError Unit)
addScriptDataHash = runExceptT do
  dats <- use _datums
  costModels <- use _costModels
  -- Use both script and minting redeemers in the order they were appended.
  reds <- use (_redeemersTxIns <<< to (map fst))
  tx <- use (_unbalancedTx <<< _transaction)
  tx' <- ExceptT $ liftEffect $ setScriptDataHash costModels reds dats tx <#>
    Right
  _cpsToTransaction .= tx'

-- | Add the remaining balance of the total value that the tx must spend.
-- | See note [Balance of value spent]
addMissingValueSpent
  :: forall (a :: Type)
   . ConstraintsM a (Either MkUnbalancedTxError Unit)
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
    txOut <- case pkh', skh' of
      Nothing, Nothing -> throwError OwnPubKeyAndStakeKeyMissing
      Just pkh, Just skh -> pure $ TransactionOutput
        { address: payPubKeyHashBaseAddress networkId pkh skh
        , amount: missing
        , datum: NoOutputDatum
        , scriptRef: Nothing
        }
      Just pkh, Nothing -> pure $ TransactionOutput
        { address: payPubKeyHashEnterpriseAddress networkId pkh
        , amount: missing
        , datum: NoOutputDatum
        , scriptRef: Nothing
        }
      Nothing, Just skh -> pure $ TransactionOutput
        { address: stakePubKeyHashRewardAddress networkId skh
        , amount: missing
        , datum: NoOutputDatum
        , scriptRef: Nothing
        }
    _cpsToTxBody <<< _outputs %= Array.(:) txOut

updateUtxoIndex
  :: forall (a :: Type)
   . ConstraintsM a (Either MkUnbalancedTxError Unit)
updateUtxoIndex = runExceptT do
  txOutputs <- use _lookups <#> unwrap >>> _.txOutputs
  networkId <- lift getNetworkId
  let cTxOutputs = map (fromPlutusTxOutputWithRefScript networkId) txOutputs
  -- Left bias towards original map, hence `flip`:
  _unbalancedTx <<< _utxoIndex %= flip union cTxOutputs

-- Note, we don't use the redeemer here, unlike Plutus because of our lack of
-- `TxIn` datatype.
-- | Add a typed input, checking the type of the output it spends. Return the value
-- | of the spent output.
addOwnInput
  :: forall (validator :: Type) (datum :: Type) (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => Proxy datum
  -> InputConstraint redeemer
  -> ConstraintsM validator (Either MkUnbalancedTxError Unit)
addOwnInput _pd (InputConstraint { txOutRef }) = do
  networkId <- getNetworkId
  runExceptT do
    ScriptLookups { txOutputs, typedValidator } <- use _lookups
    -- Convert to Cardano type
    let cTxOutputs = map (fromPlutusTxOutputWithRefScript networkId) txOutputs
    inst <- liftM TypedValidatorMissing typedValidator
    -- This line is to type check the `TransactionInput`. Plutus actually creates a `TxIn`
    -- but we don't have such a datatype for our `TxBody`. Therefore, if we pass
    -- this line, we just insert `TransactionInput` into the body.
    typedTxOutRef <- ExceptT $ lift $
      typeTxOutRef networkId (flip lookup cTxOutputs) inst txOutRef
        <#> lmap TypeCheckFailed
    let value = typedTxOutRefValue typedTxOutRef
    -- Must be inserted in order. Hopefully this matches the order under CSL
    _cpsToTxBody <<< _inputs %= Set.insert txOutRef
    _valueSpentBalancesInputs <>= provideValue value

-- | Add a typed output and return its value.
addOwnOutput
  :: forall (validator :: Type) (datum :: Type)
   . DatumType validator datum
  => ToData datum
  => OutputConstraint datum
  -> ConstraintsM validator (Either MkUnbalancedTxError Unit)
addOwnOutput (OutputConstraint { datum: d, value }) = do
  networkId <- getNetworkId
  runExceptT do
    ScriptLookups { typedValidator } <- use _lookups
    inst <- liftM TypedValidatorMissing typedValidator
    let value' = fromPlutusValue value
    typedTxOut <- except $ mkTypedTxOut networkId inst d value'
      # note MkTypedTxOutFailed
    let txOut = typedTxOutTxOut typedTxOut
    -- We are erroring if we don't have a datumhash given the polymorphic datum
    -- in the `OutputConstraint`:
    dHash <- liftM TypedTxOutHasNoDatumHash (typedTxOutDatumHash typedTxOut)
    dat <-
      ExceptT $ lift $ getDatumByHash dHash <#> note (CannotQueryDatum dHash)
    _cpsToTxBody <<< _outputs %= Array.(:) txOut
    ExceptT $ addDatum dat
    _valueSpentBalancesOutputs <>= provideValue value'

data MkUnbalancedTxError
  = TypeCheckFailed TypeCheckError
  | ModifyTx ModifyTxError
  | TxOutRefNotFound TransactionInput
  | TxOutRefWrongType TransactionInput
  | DatumNotFound DataHash
  | MintingPolicyNotFound MintingPolicyHash
  | MintingPolicyHashNotCurrencySymbol MintingPolicyHash
  | CannotMakeValue CurrencySymbol TokenName BigInt
  | ValidatorHashNotFound ValidatorHash
  | WrongRefScriptHash (Maybe ScriptHash)
  | OwnPubKeyAndStakeKeyMissing
  | TypedValidatorMissing
  | DatumWrongHash DataHash Datum
  | CannotFindDatum
  | CannotQueryDatum DataHash
  | CannotHashDatum Datum
  | CannotConvertPOSIXTimeRange POSIXTimeRange PosixTimeToSlotError
  | CannotGetMintingPolicyScriptIndex -- Should be impossible
  | CannotGetValidatorHashFromAddress Address -- Get `ValidatorHash` from internal `Address`
  | MkTypedTxOutFailed
  | TypedTxOutHasNoDatumHash
  | CannotHashMintingPolicy MintingPolicy
  | CannotHashValidator Validator
  | CannotConvertPaymentPubKeyHash PaymentPubKeyHash
  | CannotSatisfyAny

derive instance Generic MkUnbalancedTxError _
derive instance Eq MkUnbalancedTxError

instance Show MkUnbalancedTxError where
  show = genericShow

lookupTxOutRef
  :: forall (a :: Type)
   . TransactionInput
  -> ConstraintsM a (Either MkUnbalancedTxError TransactionOutput)
lookupTxOutRef outRef = runExceptT do
  txOutputs <- use _lookups <#> unwrap >>> _.txOutputs
  txOut <- liftM (TxOutRefNotFound outRef) (lookup outRef txOutputs)
  networkId <- lift getNetworkId
  pure $ fromPlutusTxOutputWithRefScript networkId txOut

lookupDatum
  :: forall (a :: Type)
   . DataHash
  -> ConstraintsM a (Either MkUnbalancedTxError Datum)
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

reindexMintRedeemers
  :: forall (a :: Type)
   . MintingPolicyHash
  -> T.Redeemer
  -> ConstraintsM a (Array T.Redeemer)
reindexMintRedeemers mpsHash redeemer = do
  _mintRedeemers %= Map.insert mpsHash redeemer
  mintRedeemers :: Array _ <- use _mintRedeemers <#> Map.toUnfoldable
  pure $
    mintRedeemers # mapWithIndex \ix (_ /\ T.Redeemer red) ->
      T.Redeemer red { index = fromInt ix }

processScriptRefUnspentOut
  :: forall (scriptHash :: Type) (a :: Type)
   . Newtype scriptHash ScriptHash
  => scriptHash
  -> InputWithScriptRef
  -> ConstraintsM a (Either MkUnbalancedTxError Unit)
processScriptRefUnspentOut scriptHash = case _ of
  SpendInput unspentOut -> do
    _cpsToTxBody <<< _inputs %= Set.insert (_.input <<< unwrap $ unspentOut)
    checkScriptRef unspentOut
  RefInput unspentOut -> do
    let refInput = (unwrap unspentOut).input
    _cpsToTxBody <<< _referenceInputs %= Set.insert refInput
    checkScriptRef unspentOut
  where
  checkScriptRef
    :: TransactionUnspentOutput
    -> ConstraintsM a (Either MkUnbalancedTxError Unit)
  checkScriptRef (TransactionUnspentOutput { output }) =
    let
      refScriptHash :: Maybe ScriptHash
      refScriptHash = _.referenceScript $ unwrap $ (unwrap output).output

      err :: ConstraintsM a (Either MkUnbalancedTxError Unit)
      err = pure $ throwError $ WrongRefScriptHash refScriptHash
    in
      if Just (unwrap scriptHash) /= refScriptHash then err
      else pure (Right unit)

-- | Modify the `UnbalancedTx` so that it satisfies the constraints, if
-- | possible. Fails if a hash is missing from the lookups, or if an output
-- | of the wrong type is spent.
processConstraint
  :: forall (a :: Type)
   . Map MintingPolicyHash MintingPolicy
  -> Map ValidatorHash Validator
  -> TxConstraint
  -> ConstraintsM a (Either MkUnbalancedTxError Unit)
processConstraint mpsMap osMap = do
  case _ of
    MustIncludeDatum dat -> addDatum dat
    MustValidateIn posixTimeRange -> do
      -- Potential improvement: bring these out so we have one source of truth
      -- although they should be static in a single contract call
      es <- lift getEraSummaries
      ss <- lift getSystemStart
      runExceptT do
        { timeToLive, validityStartInterval } <- ExceptT $ liftEffect $
          posixTimeRangeToTransactionValidity es ss posixTimeRange
            <#> lmap (CannotConvertPOSIXTimeRange posixTimeRange)
        _cpsToTxBody <<< _Newtype %=
          _
            { ttl = timeToLive
            , validityStartInterval = validityStartInterval
            }
    MustBeSignedBy pkh -> runExceptT do
      -- FIXME This is incompatible with Plutus' version, which requires
      -- the corresponding `paymentPubKey` lookup. In the next major version,
      -- we might wish to revise this
      -- See https://github.com/Plutonomicon/cardano-transaction-lib/issues/569
      _cpsToTxBody <<< _requiredSigners <>= Just [ wrap $ unwrap $ unwrap pkh ]
    MustSpendAtLeast plutusValue -> do
      let value = fromPlutusValue plutusValue
      runExceptT $ _valueSpentBalancesInputs <>= requireValue value
    MustProduceAtLeast plutusValue -> do
      let value = fromPlutusValue plutusValue
      runExceptT $ _valueSpentBalancesOutputs <>= requireValue value
    MustSpendPubKeyOutput txo -> runExceptT do
      txOut <- ExceptT $ lookupTxOutRef txo
      -- Recall an Ogmios datum is a `Maybe String` where `Nothing` implies a
      -- wallet address and `Just` as script address.
      case txOut of
        TransactionOutput { amount, datum: NoOutputDatum } -> do
          -- POTENTIAL FIX ME: Plutus has Tx.TxIn and Tx.PubKeyTxIn -- TxIn
          -- keeps track TransactionInput and TxInType (the input type, whether
          -- consuming script, public key or simple script)
          _cpsToTxBody <<< _inputs %= Set.insert txo
          _valueSpentBalancesInputs <>= provideValue amount
        _ -> throwError $ TxOutRefWrongType txo
    MustSpendScriptOutput txo red scriptRefUnspentOut -> runExceptT do
      txOut <- ExceptT $ lookupTxOutRef txo
      -- Recall an Ogmios datum is a `Maybe String` where `Nothing` implies a
      -- wallet address and `Just` as script address.
      case txOut of
        TransactionOutput { datum: NoOutputDatum } ->
          throwError $ TxOutRefWrongType txo
        TransactionOutput { address, amount, datum: datum' } ->
          do
            vHash <- liftM
              (CannotGetValidatorHashFromAddress address)
              (enterpriseAddressValidatorHash address)
            case scriptRefUnspentOut of
              Nothing -> do
                plutusScript <-
                  except $ unwrap <$> lookupValidator vHash osMap
                ExceptT $ attachToCps attachPlutusScript plutusScript
              Just scriptRefUnspentOut' ->
                ExceptT $ processScriptRefUnspentOut vHash scriptRefUnspentOut'
            -- Note: Plutus uses `TxIn` to attach a redeemer and datum.
            -- Use the datum hash inside the lookup
            -- Note: if we get `Nothing`, we have to throw eventhough that's a
            -- valid input, because our `txOut` above is a Script address via
            -- `Just`.
            case datum' of
              OutputDatumHash dHash -> do
                dat <- ExceptT do
                  mDatumQuery <- lift $ getDatumByHash dHash <#> note
                    (CannotQueryDatum dHash)
                  mDatumLookup <- lookupDatum dHash
                  pure (mDatumQuery <|> mDatumLookup)
                ExceptT $ addDatum dat
              OutputDatum _ -> pure unit
              NoOutputDatum -> throwError CannotFindDatum
            _cpsToTxBody <<< _inputs %= Set.insert txo
            let
              -- Create a redeemer with hardcoded execution units then call Ogmios
              -- to add the units in at the very end.
              -- Hardcode script spend index then reindex at the end *after
              -- balancing* with `reindexSpentScriptRedeemers`
              redeemer = T.Redeemer
                { tag: Spend
                , index: zero -- hardcoded and tweaked after balancing.
                , data: unwrap red
                , exUnits: zero
                }
            _valueSpentBalancesInputs <>= provideValue amount
            -- Append redeemer for spending to array.
            _redeemersTxIns <>= Array.singleton (redeemer /\ Just txo)
            -- Attach redeemer to witness set.
            ExceptT $ attachToCps attachRedeemer redeemer
    MustSpendNativeScriptOutput txo ns -> runExceptT do
      _cpsToTxBody <<< _inputs %= Set.insert txo
      ExceptT $ attachToCps attachNativeScript ns
    MustReferenceOutput refInput -> runExceptT do
      _cpsToTxBody <<< _referenceInputs %= Set.insert refInput
    MustMintValue mpsHash red tn i scriptRefUnspentOut -> runExceptT do
      case scriptRefUnspentOut of
        Nothing -> do
          plutusScript <-
            except $ unwrap <$> lookupMintingPolicy mpsHash mpsMap
          ExceptT $ attachToCps attachPlutusScript plutusScript
        Just scriptRefUnspentOut' ->
          ExceptT $ processScriptRefUnspentOut mpsHash scriptRefUnspentOut'
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
          _valueSpentBalancesInputs <>= provideValue v
          pure $ map getNonAdaAsset $ value i
        else do
          v <- liftM (CannotMakeValue cs tn i) (value i)
          _valueSpentBalancesOutputs <>= provideValue v
          pure $ map getNonAdaAsset $ value i
      let
        -- Create a redeemer with zero execution units then call Ogmios to
        -- add the units in at the very end.
        -- Hardcode minting policy index, then reindex with
        -- `reindexMintRedeemers`.
        redeemer = T.Redeemer
          { tag: Mint
          , index: zero
          , data: unwrap red
          , exUnits: zero
          }
      -- Remove mint redeemers from array before reindexing.
      _redeemersTxIns %= filter \(T.Redeemer { tag } /\ _) -> tag /= Mint
      -- Reindex mint redeemers.
      mintRedeemers <- lift $ reindexMintRedeemers mpsHash redeemer
      -- Append reindexed mint redeemers to array.
      _redeemersTxIns <>= map (_ /\ Nothing) mintRedeemers
      _cpsToTxBody <<< _mint <>= map wrap mintVal
    MustPayToPubKeyAddress pkh skh mDatum scriptRef plutusValue -> do
      networkId <- getNetworkId
      let amount = fromPlutusValue plutusValue
      runExceptT do
        -- If non-inline datum is presented, add it to 'datumWitnesses' and
        -- Array of datums.
        datum' <- for mDatum \(dat /\ datp) -> do
          when (datp == DatumWitness) $ ExceptT $ addDatum dat
          outputDatum dat datp

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
        _cpsToTxBody <<< _outputs %= Array.(:) txOut
        _valueSpentBalancesOutputs <>= provideValue amount
    MustPayToScript vlh dat datp scriptRef plutusValue -> do
      networkId <- getNetworkId
      let amount = fromPlutusValue plutusValue
      runExceptT do
        datum' <- outputDatum dat datp
        let
          txOut = TransactionOutput
            { address: validatorHashEnterpriseAddress networkId vlh
            , amount
            -- TODO: save correct and scriptRef, should be done in
            -- Constraints API upgrade that follows Vasil
            , datum: datum'
            , scriptRef: scriptRef
            }
        -- Note we don't `addDatum` as this included as part of `mustPayToScript`
        -- constraint already.
        _cpsToTxBody <<< _outputs %= Array.(:) txOut
        _valueSpentBalancesOutputs <>= provideValue amount
    MustPayToNativeScript nsh plutusValue -> do
      networkId <- getNetworkId
      let amount = fromPlutusValue plutusValue
      runExceptT do
        let
          txOut = TransactionOutput
            { address: nativeScriptHashEnterpriseAddress networkId nsh
            , amount
            , datum: NoOutputDatum
            , scriptRef: Nothing
            }
        _cpsToTxBody <<< _outputs %= Array.(:) txOut
        _valueSpentBalancesOutputs <>= provideValue amount
    MustHashDatum dh dt -> do
      let mdh = Hashing.datumHash dt
      if mdh == Just dh then addDatum dt
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
          -> ConstraintsM a (Either MkUnbalancedTxError Unit)
        tryNext Nil = pure $ throwError CannotSatisfyAny
        tryNext (Cons ys zs) =
          -- Note this implicitly resets state to original cps upon failure (see
          -- `put`)
          foldM
            ( \_ constr -> runExceptT do
                ExceptT $ processConstraint mpsMap osMap constr
                  `catchError` \_ -> put cps *> tryNext zs
            )
            (Right unit)
            ys
      tryNext (toUnfoldable $ map toUnfoldable xs)
    MustNotBeValid -> runExceptT do
      _cpsToTransaction <<< _isValid .= false
  where
  outputDatum
    :: Datum
    -> DatumPresence
    -> ExceptT
         MkUnbalancedTxError
         (StateT (ConstraintProcessingState a) (QueryMExtended ()))
         OutputDatum
  outputDatum dat = case _ of
    DatumInline -> pure $ OutputDatum dat
    DatumWitness -> OutputDatumHash <$> liftMaybe (CannotHashDatum dat)
      (Hashing.datumHash dat)

-- Attach a Datum, Redeemer, or PlutusScript depending on the handler. They
-- share error type anyway.
attachToCps
  :: forall (a :: Type) (b :: Type)
   . (a -> Transaction -> Effect (Either ModifyTxError Transaction))
  -> a -- Redeemer, Datum, or PlutusScript.
  -> ConstraintsM b (Either MkUnbalancedTxError Unit)
attachToCps handler object = do
  tx <- use (_unbalancedTx <<< _transaction)
  newTx <- liftEffect $ handler object tx <#> lmap ModifyTx
  either
    (pure <<< throwError)
    (map Right <<< (.=) (_unbalancedTx <<< _transaction))
    newTx

-- Attaches datum to the transaction and to Array of datums in the state.
addDatum
  :: forall (a :: Type)
   . Datum
  -> ConstraintsM a (Either MkUnbalancedTxError Unit)
addDatum dat = runExceptT do
  ExceptT $ attachToCps attachDatum dat
  _datums <>= Array.singleton dat

-- Helper to focus from `ConstraintProcessingState` down to `Transaction`.
_cpsToTransaction
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) Transaction
_cpsToTransaction = _unbalancedTx <<< _transaction

-- Helper to focus from `ConstraintProcessingState` down to `TxBody`.
_cpsToTxBody :: forall (a :: Type). Lens' (ConstraintProcessingState a) TxBody
_cpsToTxBody = _cpsToTransaction <<< _body

getNetworkId
  :: forall (a :: Type)
   . ConstraintsM a NetworkId
getNetworkId = use (_cpsToTxBody <<< _networkId)
  >>= maybe (lift $ asks $ _.config >>> _.networkId) pure
