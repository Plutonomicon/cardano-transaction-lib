module Types.ScriptLookups
  ( MkUnbalancedTxError(..)
  , ScriptLookups(..)
  , generalise
  , mintingPolicy
  , mintingPolicyM
  , mkUnbalancedTx
  , mkUnbalancedTx'
  , otherData
  , otherScript
  , otherScriptM
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

import Address (enterpriseAddressValidatorHash)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT, get, gets, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array (singleton, union) as Array
import Data.Array ((:), length, toUnfoldable, zip)
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
import Data.Map (Map, empty, fromFoldable, lookup, mapMaybe, singleton, union)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(SProxy))
import Data.Traversable (for, sequence)
import Data.Tuple.Nested (type (/\), (/\))
import FromData (class FromData)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Helpers ((<\>), liftEither, liftM)
import QueryM (QueryConfig, QueryM, datumHash, getDatumByHash)
import Scripts
  ( mintingPolicyHash
  , validatorHash
  , validatorHashEnterpriseAddress
  )
import Serialization.Address (Address, NetworkId)
import ToData (class ToData)
import Types.Any (Any)
import Types.Datum (Datum(Datum), DatumHash)
import Types.Interval (POSIXTimeRange, posixTimeRangeToTransactionSlot)
import Types.RedeemerTag (RedeemerTag(Mint, Spend))
import Types.Scripts
  ( MintingPolicy
  , MintingPolicyHash
  , Validator
  , ValidatorHash
  )
import Types.TypedValidator (generalise) as TV
import Transaction
  ( ModifyTxError
  , attachDatum
  , attachPlutusScript
  , attachRedeemer
  , setScriptDataHash
  )
import Types.Transaction (Redeemer(Redeemer)) as T
import Types.Transaction
  ( ExUnits
  , Transaction(Transaction)
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  , TransactionWitnessSet
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
  ( InputConstraint(InputConstraint)
  , OutputConstraint(OutputConstraint)
  , TxConstraint
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
import Types.TypedTxOut
  ( TypeCheckError
  , mkTypedTxOut
  , typedTxOutDatumHash
  , typedTxOutRefValue
  , typedTxOutTxOut
  , typeTxOutRef
  )
import Types.TypedValidator
  ( class DatumType
  , class RedeemerType
  , TypedValidator(TypedValidator)
  )
import Types.UnbalancedTransaction
  ( TxOutRef
  , PaymentPubKey
  , PaymentPubKeyHash
  , StakePubKeyHash
  , UnbalancedTx(UnbalancedTx)
  , _transaction
  , _utxoIndex
  , emptyUnbalancedTx
  -- , payPubKeyHash
  , payPubKeyHashBaseAddress
  , payPubKeyHashEnterpriseAddress
  , payPubKeyRequiredSigner
  , stakePubKeyHashRewardAddress
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
  , getNonAdaAsset
  )
import TxOutput (transactionOutputToScriptOutput)

-- Taken mainly from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/Ledger-Constraints-OffChain.html
-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26
-- It should be noted that `ScriptOutput` came later and this was already apart
-- of our codebase so I had to mix & match Plutus revs.

--------------------------------------------------------------------------------
-- ScriptLookups type
--------------------------------------------------------------------------------
-- We write `mps` and `otherScripts` as an `Array` instead of `Map`, meaning
-- our lookup helpers aren't required to hash (`mintingPolicy`, `otherScript`)
-- and therefore not lifted to `QueryM`. The downside is the lookups contain
-- less information. All hashing is done inside `ConstraintsM`, see
-- `processLookupsAndConstraints`.
newtype ScriptLookups (a :: Type) = ScriptLookups
  { mps :: Array MintingPolicy -- Minting policies that the script interacts with
  , txOutputs :: Map TxOutRef TransactionOutput -- Unspent outputs that the script may want to spend. This may need tweaking to `TransactionOutput`
  , otherScripts :: Array Validator -- Validators of scripts other than "our script"
  , otherData :: Map DatumHash Datum --  Datums that we might need
  , paymentPubKeyHashes :: Map PaymentPubKeyHash PaymentPubKey -- Public keys that we might need
  , typedValidator :: Maybe (TypedValidator a) -- The script instance with the typed validator hash & actual compiled program
  , ownPaymentPubKeyHash :: Maybe PaymentPubKeyHash -- The contract's payment public key hash, used for depositing tokens etc.
  , ownStakePubKeyHash :: Maybe StakePubKeyHash -- The contract's stake public key hash (optional)
  }

derive instance Generic (ScriptLookups a) _
derive instance Newtype (ScriptLookups a) _
derive newtype instance Eq (ScriptLookups a)

instance Show (ScriptLookups a) where
  show = genericShow

generalise :: forall (a :: Type). ScriptLookups a -> ScriptLookups Any
generalise (ScriptLookups sl) =
  let
    validator = TV.generalise <$> sl.typedValidator
  in
    wrap sl { typedValidator = validator }

-- Using `Data.Map.union`, we can replicate left-biased <> from Data.Map used
-- in Plutus (*not* Plutus' internal Map that uses something like unionWith (<>))
instance Semigroup (ScriptLookups a) where
  append (ScriptLookups l) (ScriptLookups r) =
    ScriptLookups
      { mps: l.mps `Array.union` r.mps
      , txOutputs: l.txOutputs `union` r.txOutputs
      , otherScripts: l.otherScripts `Array.union` r.otherScripts
      , otherData: l.otherData `union` r.otherData
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
    , otherScripts: mempty
    , otherData: empty
    , paymentPubKeyHashes: empty
    , typedValidator: Nothing
    , ownPaymentPubKeyHash: Nothing
    , ownStakePubKeyHash: Nothing
    }

--------------------------------------------------------------------------------
-- Create ScriptLookups helpers
--------------------------------------------------------------------------------
-- | The lookup functions come in pairs with the exception of `otherData`.
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

-- FIX ME: https://github.com/Plutonomicon/cardano-browser-tx/issues/200
-- | A script lookups value that uses the map of unspent outputs to resolve
-- | input constraints.
unspentOutputs
  :: forall (a :: Type). Map TxOutRef TransactionOutput -> ScriptLookups a
unspentOutputs mp = over ScriptLookups _ { txOutputs = mp } mempty

-- FIX ME: https://github.com/Plutonomicon/cardano-browser-tx/issues/200
-- | Same as `unspentOutputs` but in `Maybe` context for convenience. This
-- | should not fail.
unspentOutputsM
  :: forall (a :: Type). Map TxOutRef TransactionOutput -> Maybe (ScriptLookups a)
unspentOutputsM = pure <<< unspentOutputs

-- | A script lookups value with a minting policy script.
mintingPolicy :: forall (a :: Type). MintingPolicy -> ScriptLookups a
mintingPolicy pl = over ScriptLookups _ { mps = Array.singleton pl } mempty

-- | Same as `mintingPolicy` but in `Maybe` context for convenience. This
-- | should not fail.
mintingPolicyM :: forall (a :: Type). MintingPolicy -> Maybe (ScriptLookups a)
mintingPolicyM = pure <<< mintingPolicy

-- | A script lookups value with a validator script.
otherScript :: forall (a :: Type). Validator -> ScriptLookups a
otherScript vl =
  over ScriptLookups _ { otherScripts = Array.singleton vl } mempty

-- | Same as `otherScript` but in `Maybe` context for convenience. This
-- | should not fail.
otherScriptM :: forall (a :: Type). Validator -> Maybe (ScriptLookups a)
otherScriptM = pure <<< otherScript

-- | A script lookups value with a datum. This can fail because we invoke
-- | `datumHash` using a server.
otherData :: forall (a :: Type). Datum -> QueryM (Maybe (ScriptLookups a))
otherData dt = do
  mDh <- datumHash dt
  pure $ maybe
    Nothing
    (\dh -> Just $ over ScriptLookups _ { otherData = singleton dh dt } mempty)
    mDh

-- -- | A script lookups value with a payment public key. This can fail because we
-- -- | invoke `payPubKeyHash`.
-- paymentPubKeyM :: forall (a :: Type). PaymentPubKey -> Maybe (ScriptLookups a)
-- paymentPubKeyM ppk = do
--   pkh <- payPubKeyHash ppk
--   pure $ over ScriptLookups
--     _ { paymentPubKeyHashes = singleton pkh ppk }
--     mempty

-- -- | A script lookups value with a payment public key. This is unsafe because
-- -- | the underlying function `paymentPubKeyM` can fail.
-- unsafePaymentPubKey :: forall (a :: Type). PaymentPubKey -> ScriptLookups a
-- unsafePaymentPubKey = unsafePartial fromJust <<< paymentPubKeyM

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
  , redeemers :: Array T.Redeemer
  -- Ordered accumulation of redeemers so we can use to `setScriptDataHash` and
  -- add execution units via Ogmios. Note: this mixes script and minting
  -- redeemers.
  , lookups :: ScriptLookups a
  -- ScriptLookups for resolving constraints. Should be treated as an immutable
  -- value despite living inside the processing state
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

_redeemers
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) (Array T.Redeemer)
_redeemers = prop (SProxy :: SProxy "redeemers")

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

provide :: Value -> ValueSpentBalances
provide provided = ValueSpentBalances { provided, required: mempty }

require :: Value -> ValueSpentBalances
require required = ValueSpentBalances { required, provided: mempty }

-- A `StateT` ontop of `QueryM` ~ ReaderT QueryConfig Aff`.
-- The state is `ConstraintProcessingState`, which keeps track of the unbalanced
-- transaction etc and additionally holds a `ConstraintsConfig` containing the
-- scriptlookups and a `defaultSlotConfig`.
-- We write `ReaderT QueryConfig Aff` below since type synonyms need to be fully
-- applied.
type ConstraintsM (a :: Type) (b :: Type) =
  StateT (ConstraintProcessingState a) (ReaderT QueryConfig Aff) b

-- The constraints don't precisely match those of Plutus:
-- `forall a. (FromData (DatumType a), ToData (DatumType a), ToData (RedeemerType a))`
-- as we don't have the same granularity on the classes, but the type `a` fixes
-- a type `b` as seen below. We could alternatively create specific typeclasses:
-- ToData (Datumtype a) <-> (Datumtype a b, ToData b) <= ToDataDatumType a b
-- if we require granular control, similarly FromDataToDatumType a b etc.
-- We could use `MonadError` to clean up the `ExceptT`s below although we can't
-- use the type alias because they need to be fully applied so this is perhaps
-- more readable.
-- Fix me: add execution units from Ogmios where this function should be
-- inside QueryM https://github.com/Plutonomicon/cardano-browser-tx/issues/174
-- | Resolve some `TxConstraints` by modifying the `UnbalancedTx` in the
-- | `ConstraintProcessingState`
processLookupsAndConstraints
  :: forall (a :: Type) (b :: Type)
   . DatumType a b
  => RedeemerType a b
  => FromData b
  => ToData b
  => TxConstraints b b
  -> ConstraintsM a (Either MkUnbalancedTxError Unit)
processLookupsAndConstraints
  (TxConstraints { constraints, ownInputs, ownOutputs }) = runExceptT do
  -- Hash all the MintingPolicys and Scripts beforehand. These maps are lost
  -- after we `runReaderT`, unlike Plutus that has a `Map` instead of `Array`.
  lookups <- use _lookups <#> unwrap
  let
    mps = lookups.mps
    otherScripts = lookups.otherScripts
  mpsHashes <-
    ExceptT $ hashScripts mintingPolicyHash CannotHashMintingPolicy mps
  otherScriptHashes <-
    ExceptT $ hashScripts validatorHash CannotHashValidator otherScripts
  let
    mpsMap = fromFoldable $ zip mpsHashes mps
    osMap = fromFoldable $ zip otherScriptHashes otherScripts
  ExceptT $ foldConstraints (processConstraint mpsMap osMap) constraints
  ExceptT $ foldConstraints addOwnInput ownInputs
  ExceptT $ foldConstraints addOwnOutput ownOutputs
  ExceptT addScriptDataHash
  ExceptT addMissingValueSpent
  ExceptT updateUtxoIndex
  where
  -- Polymorphic helper to hash an Array of `Validator`s or `MintingPolicy`s
  -- with a way to error.
  hashScripts
    :: forall (script :: Type) (scriptHash :: Type) (c :: Type)
     . (script -> QueryM (Maybe scriptHash))
    -> (script -> MkUnbalancedTxError)
    -> Array script
    -> ConstraintsM c (Either MkUnbalancedTxError (Array scriptHash))
  hashScripts hasher error scripts =
    lift $
      for scripts
        ( \s -> do
            sh <- hasher s
            pure $ note (error s) sh
        ) <#> sequence

  -- Don't write the output in terms of ExceptT because we can't write a
  -- partially applied `ConstraintsM` meaning this is more readable.
  foldConstraints
    :: forall (constr :: Type) (c :: Type)
     . (constr -> ConstraintsM c (Either MkUnbalancedTxError Unit))
    -> Array constr
    -> ConstraintsM c (Either MkUnbalancedTxError Unit)
  foldConstraints handler = foldM
    (\_ constr -> runExceptT $ ExceptT $ handler constr)
    (Right unit)

-- Helper to run the stack and get back to `QueryM`. See comments in
-- `processLookupsAndConstraints` regarding constraints.
runConstraintsM
  :: forall (a :: Type) (b :: Type)
   . DatumType a b
  => RedeemerType a b
  => FromData b
  => ToData b
  => ScriptLookups a
  -> TxConstraints b b
  -> QueryM (Either MkUnbalancedTxError (ConstraintProcessingState a))
runConstraintsM lookups txConstraints =
  let
    initCps :: ConstraintProcessingState a
    initCps =
      { unbalancedTx: emptyUnbalancedTx
      , valueSpentBalancesInputs:
          ValueSpentBalances { required: mempty, provided: mempty }
      , valueSpentBalancesOutputs:
          ValueSpentBalances { required: mempty, provided: mempty }
      , datums: mempty
      , redeemers: mempty
      , lookups
      }

    unpackTuple
      :: Either MkUnbalancedTxError Unit /\ (ConstraintProcessingState a)
      -> Either MkUnbalancedTxError (ConstraintProcessingState a)
    unpackTuple (Left err /\ _) = Left err
    unpackTuple (_ /\ cps) = Right cps
  in
    unpackTuple <$>
      ( flip runStateT initCps $ processLookupsAndConstraints txConstraints
      )

-- See comments in `processLookupsAndConstraints` regarding constraints.
-- | Create an `UnbalancedTx` given `ScriptLookups` and `TxConstraints`.
mkUnbalancedTx
  :: forall (a :: Type) (b :: Type)
   . DatumType a b
  => RedeemerType a b
  => FromData b
  => ToData b
  => ScriptLookups a
  -> TxConstraints b b
  -> QueryM (Either MkUnbalancedTxError UnbalancedTx)
mkUnbalancedTx scriptLookups txConstraints =
  runConstraintsM scriptLookups txConstraints <#> map _.unbalancedTx

-- | An temporary implementation that strips `witness_set` and data hash from
-- | returned `UnbalancedTx` in order to calculate them later on server.
-- | It returns part of the `ConstraintProcessingState` for later consumption by
-- | the server.
mkUnbalancedTx'
  :: forall (a :: Type) (b :: Type)
   . DatumType a b
  => RedeemerType a b
  => FromData b
  => ToData b
  => ScriptLookups a
  -> TxConstraints b b
  -> QueryM
       ( Either MkUnbalancedTxError
           { unbalancedTx :: UnbalancedTx
           , datums :: Array Datum
           , redeemers :: Array T.Redeemer
           }
       )
mkUnbalancedTx' scriptLookups txConstraints =
  runConstraintsM scriptLookups txConstraints <#> map
    \{ unbalancedTx, datums, redeemers } ->
      let
        _transaction = prop (SProxy :: SProxy "transaction")
        _body = prop (SProxy :: SProxy "body")
        _script_data_hash = prop (SProxy :: SProxy "script_data_hash")

        stripScriptDataHash :: UnbalancedTx -> UnbalancedTx
        stripScriptDataHash =
          over UnbalancedTx $
            _transaction %~ over Transaction
              (_body %~ over TxBody (_script_data_hash .~ Nothing))
        tx = stripScriptDataHash unbalancedTx
      in
        { unbalancedTx: tx, datums, redeemers }

addScriptDataHash
  :: forall (a :: Type)
   . ConstraintsM a (Either MkUnbalancedTxError Unit)
addScriptDataHash = runExceptT do
  dats <- use _datums
  -- Use both script and minting redeemers in the order they were appended.
  reds <- use _redeemers
  tx <- use (_unbalancedTx <<< _transaction)
  tx' <- ExceptT $ liftEffect $ setScriptDataHash reds dats tx <#> Right
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
      Just pkh, Just _ -> liftEither $ Right $ TransactionOutput
        { address: payPubKeyHashBaseAddress networkId pkh
        , amount: missing
        , data_hash: Nothing
        }
      Just pkh, Nothing -> liftEither $ Right $ TransactionOutput
        { address: payPubKeyHashEnterpriseAddress networkId pkh
        , amount: missing
        , data_hash: Nothing
        }
      Nothing, Just skh -> liftEither $ Right $ TransactionOutput
        { address: stakePubKeyHashRewardAddress networkId skh
        , amount: missing
        , data_hash: Nothing
        }
    _cpsToTxBody <<< _outputs %= (:) txOut

updateUtxoIndex
  :: forall (a :: Type)
   . ConstraintsM a (Either MkUnbalancedTxError Unit)
updateUtxoIndex = runExceptT do
  txOutputs <- use _lookups <#> unwrap >>> _.txOutputs
  let txOutsMap = mapMaybe transactionOutputToScriptOutput txOutputs
  -- Left bias towards original map, hence `flip`:
  _unbalancedTx <<< _utxoIndex %= flip union txOutsMap

-- Note, we don't use the redeemer here, unlike Plutus because of our lack of
-- `TxIn` datatype.
-- | Add a typed input, checking the type of the output it spends. Return the value
-- | of the spent output.
addOwnInput
  :: forall (a :: Type) (b :: Type)
   . DatumType a b
  => RedeemerType a b
  => FromData b
  => ToData b
  => InputConstraint b
  -> ConstraintsM a (Either MkUnbalancedTxError Unit)
addOwnInput (InputConstraint { txOutRef }) = do
  networkId <- getNetworkId
  runExceptT do
    ScriptLookups { txOutputs, typedValidator } <- use _lookups
    inst <- liftM TypedValidatorMissing typedValidator
    -- This line is to type check the `TxOutRef`. Plutus actually creates a `TxIn`
    -- but we don't have such a datatype for our `TxBody`. Therefore, if we pass
    -- this line, we just insert `TxOutRef` into the body.
    typedTxOutRef <- ExceptT $ lift $
      typeTxOutRef networkId (flip lookup txOutputs) inst txOutRef
        <#> lmap TypeCheckFailed
    let value = typedTxOutRefValue typedTxOutRef
    _cpsToTxBody <<< _inputs %= (:) txOutRef
    _valueSpentBalancesInputs <>= provide value

-- | Add a typed output and return its value.
addOwnOutput
  :: forall (a :: Type) (b :: Type)
   . DatumType a b
  => FromData b
  => ToData b
  => OutputConstraint b
  -> ConstraintsM a (Either MkUnbalancedTxError Unit)
addOwnOutput (OutputConstraint { datum, value }) = do
  networkId <- getNetworkId
  runExceptT do
    ScriptLookups { typedValidator } <- use _lookups
    inst <- liftM TypedValidatorMissing typedValidator
    typedTxOut <- ExceptT $ lift $ mkTypedTxOut networkId inst datum value
      <#> note MkTypedTxOutFailed
    let txOut = typedTxOutTxOut typedTxOut
    -- We are erroring if we don't have a datumhash given the polymorphic datum
    -- in the `OutputConstraint`:
    dHash <- liftM TypedTxOutHasNoDatumHash (typedTxOutDatumHash typedTxOut)
    dat <-
      ExceptT $ lift $ getDatumByHash dHash <#> note (CannotQueryDatum dHash)
    _cpsToTxBody <<< _outputs %= (:) txOut
    ExceptT $ addDatum (wrap dat)
    _valueSpentBalancesOutputs <>= provide value

data MkUnbalancedTxError
  = TypeCheckFailed TypeCheckError
  | ModifyTx ModifyTxError
  | TxOutRefNotFound TxOutRef
  | TxOutRefWrongType TxOutRef
  | DatumNotFound DatumHash
  | MintingPolicyNotFound MintingPolicyHash
  | MintingPolicyHashNotCurrencySymbol MintingPolicyHash
  | CannotMakeValue CurrencySymbol TokenName BigInt
  | ValidatorHashNotFound ValidatorHash
  | OwnPubKeyAndStakeKeyMissing
  | TypedValidatorMissing
  | DatumWrongHash DatumHash Datum
  | CannotQueryDatum DatumHash
  | CannotHashDatum Datum
  | CannotConvertPOSIXTimeRange POSIXTimeRange
  | CannotGetValidatorHashFromAddress Address -- Get `ValidatorHash` from internal `Address`
  | CannotGetMintingPolicyScriptIndex -- Cannot get the Minting Policy Index - this should be impossible.
  | CannotGetMintingValidatorScriptIndex -- Cannot get the Validator Index - this should be impossible.
  | MkTypedTxOutFailed
  | TypedTxOutHasNoDatumHash
  | CannotHashMintingPolicy MintingPolicy
  | CannotHashValidator Validator
  | CannotSatisfyAny

derive instance Generic MkUnbalancedTxError _
derive instance Eq MkUnbalancedTxError

instance Show MkUnbalancedTxError where
  show = genericShow

lookupTxOutRef
  :: forall (a :: Type)
   . TxOutRef
  -> ConstraintsM a (Either MkUnbalancedTxError TransactionOutput)
lookupTxOutRef outRef = do
  txOutputs <- use _lookups <#> unwrap >>> _.txOutputs
  let err = pure $ throwError $ TxOutRefNotFound outRef
  maybe err (pure <<< Right) $ lookup outRef txOutputs

lookupDatum
  :: forall (a :: Type)
   . DatumHash
  -> ConstraintsM a (Either MkUnbalancedTxError Datum)
lookupDatum dh = do
  otherDt <- use _lookups <#> unwrap >>> _.otherData
  let err = pure $ throwError $ DatumNotFound dh
  maybe err (pure <<< Right) $ lookup dh otherDt

lookupMintingPolicy
  :: forall (a :: Type)
   . MintingPolicyHash
  -> Map MintingPolicyHash MintingPolicy
  -> ConstraintsM a (Either MkUnbalancedTxError MintingPolicy)
lookupMintingPolicy mph mpsMap = do
  let err = pure $ throwError $ MintingPolicyNotFound mph
  maybe err (pure <<< Right) $ lookup mph mpsMap

lookupValidator
  :: forall (a :: Type)
   . ValidatorHash
  -> Map ValidatorHash Validator
  -> ConstraintsM a (Either MkUnbalancedTxError Validator)
lookupValidator vh osMap = do
  let err = pure $ throwError $ ValidatorHashNotFound vh
  maybe err (pure <<< Right) $ lookup vh osMap

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
    MustIncludeDatum datum -> addDatum datum
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
      ppkh <- use _lookups <#> unwrap >>> _.paymentPubKeyHashes
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
        TransactionOutput { amount, data_hash: Nothing } -> do
          -- POTENTIAL FIX ME: Plutus has Tx.TxIn and Tx.PubKeyTxIn -- TxIn
          -- keeps track TxOutRef and TxInType (the input type, whether
          -- consuming script, public key or simple script)
          _cpsToTxBody <<< _inputs %= (:) txo
          _valueSpentBalancesInputs <>= provide amount
        _ -> liftEither $ throwError $ TxOutRefWrongType txo
    MustSpendScriptOutput txo red -> runExceptT do
      txOut <- ExceptT $ lookupTxOutRef txo
      -- Recall an Ogmios datum is a `Maybe String` where `Nothing` implies a
      -- wallet address and `Just` as script address.
      case txOut of
        TransactionOutput { address, amount, data_hash: Just dHash } -> do
          vHash <- liftM
            (CannotGetValidatorHashFromAddress address)
            (enterpriseAddressValidatorHash address)
          plutusScript <- ExceptT $ lookupValidator vHash osMap <#> map unwrap
          -- Note: Plutus uses `TxIn` to attach a redeemer and datum.
          -- Use the datum hash inside the lookup
          -- Note: if we get `Nothing`, we have to throw eventhough that's a
          -- valid input, because our `txOut` above is a Script address via
          -- `Just`.
          dataValue <- ExceptT $
            ( lift $ getDatumByHash dHash
                <#> note (CannotQueryDatum dHash) >>> map Datum
            ) <|>
              lookupDatum dHash
          ExceptT $ attachToCps attachPlutusScript plutusScript
          -- Get the redeemer index, which is the current length of scripts - 1
          mIndex <-
            use (_cpsToWitnessSet <<< _plutusScripts <<< to (map lastIndex))
          -- This error should be impossible as we just attached:
          index <- liftM CannotGetMintingValidatorScriptIndex mIndex
          ExceptT $ addDatum dataValue
          _cpsToTxBody <<< _inputs %= (:) txo
          let
            -- Create a redeemer with hardcoded execution units then call Ogmios
            -- to add the units in at the very end.
            redeemer = T.Redeemer
              { tag: Spend
              , index
              , data: unwrap red
              , ex_units: scriptExUnits
              }
          _valueSpentBalancesInputs <>= provide amount
          -- Append redeemer for spending to array.
          _redeemers <>= Array.singleton redeemer
          -- Attach redeemer to witness set.
          ExceptT $ attachToCps attachRedeemer redeemer
        _ -> liftEither $ throwError $ TxOutRefWrongType txo
    MustMintValue mpsHash red tn i -> runExceptT do
      plutusScript <-
        ExceptT $ lookupMintingPolicy mpsHash mpsMap <#> map unwrap
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
          liftEither $ Right $ map getNonAdaAsset $ value i
        else do
          v <- liftM (CannotMakeValue cs tn i) (value i)
          _valueSpentBalancesOutputs <>= provide v
          liftEither $ Right $ map getNonAdaAsset $ value i
      ExceptT $ attachToCps attachPlutusScript plutusScript
      -- Get the redeemer index, which is the current length of scripts - 1
      mIndex <- use (_cpsToWitnessSet <<< _plutusScripts <<< to (map lastIndex))
      -- This error should be impossible as we just attached:
      index <- liftM CannotGetMintingPolicyScriptIndex mIndex
      let
        -- Create a redeemer with zero execution units then call Ogmios to
        -- add the units in at the very end.
        redeemer = T.Redeemer
          { tag: Mint
          , index
          , data: unwrap red
          , ex_units: mintExUnits
          }
      _cpsToTxBody <<< _mint <>= map wrap mintVal
      -- Append redeemer for minting to array.
      _redeemers <>= Array.singleton redeemer
      -- Attach redeemer to witness set.
      ExceptT $ attachToCps attachRedeemer redeemer
    MustPayToPubKeyAddress pkh _ mDatum amount -> do
      networkId <- getNetworkId
      runExceptT do
        -- If datum is presented, add it to 'datumWitnesses' and Array of datums.
        -- Otherwise continue, hence `liftEither $ Right unit`.
        maybe (liftEither $ Right unit) (ExceptT <<< addDatum) mDatum
        let
          liftDatumHash
            :: forall (e :: Type) (dh :: Type)
             . e
            -> Maybe dh
            -> Either e (Maybe dh)
          liftDatumHash e Nothing = Left e
          liftDatumHash _ (Just x) = Right (Just x)
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
          ( \datum -> ExceptT $ lift $
              liftDatumHash (CannotHashDatum datum) <$> datumHash datum
          )
          mDatum
        -- Changed this to enterprise address for Seabug, it could be an issue
        -- down the road as we track all types of Addresses properly
        let
          txOut = TransactionOutput
            { address: payPubKeyHashEnterpriseAddress networkId pkh, amount, data_hash }
        _cpsToTxBody <<< _outputs %= (:) txOut
        _valueSpentBalancesOutputs <>= provide amount
    MustPayToOtherScript vlh datum amount -> do
      networkId <- getNetworkId
      runExceptT do
        -- Don't write `let data_hash = datumHash datum`, see [datumHash Note]
        data_hash <- ExceptT $ lift $ note (CannotHashDatum datum)
          <$> map Just
          <$> datumHash datum
        let
          txOut = TransactionOutput
            { address: validatorHashEnterpriseAddress networkId vlh
            , amount
            , data_hash
            }
        ExceptT $ addDatum datum
        _cpsToTxBody <<< _outputs %= (:) txOut
        _valueSpentBalancesOutputs <>= provide amount
    MustHashDatum dh dt -> do
      mdh <- lift $ datumHash dt
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
  where
  -- The follow hardcoded before calling Ogmios to calculate execution
  -- unit. Calling Ogmios is an outstanding issue:
  -- https://github.com/Plutonomicon/cardano-browser-tx/issues/174
  scriptExUnits :: ExUnits
  scriptExUnits = { mem: fromInt 2000000, steps: fromInt 1000000000 }

  mintExUnits :: ExUnits
  mintExUnits = { mem: fromInt 2000000, steps: fromInt 1000000000 }

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
addDatum datum = runExceptT do
  ExceptT $ attachToCps attachDatum datum
  _datums %= (:) datum

-- Helper to focus from `ConstraintProcessingState` down to `Transaction`.
_cpsToTransaction
  :: forall (a :: Type). Lens' (ConstraintProcessingState a) Transaction
_cpsToTransaction = _unbalancedTx <<< _transaction

-- Helper to focus from `ConstraintProcessingState` down to
-- `TransactionWitnessSet`.
_cpsToWitnessSet
  :: forall (a :: Type)
   . Lens' (ConstraintProcessingState a) TransactionWitnessSet
_cpsToWitnessSet = _cpsToTransaction <<< _witnessSet

-- Helper to focus from `ConstraintProcessingState` down to `TxBody`.
_cpsToTxBody :: forall (a :: Type). Lens' (ConstraintProcessingState a) TxBody
_cpsToTxBody = _cpsToTransaction <<< _body

lastIndex :: forall (a :: Type). Array a -> BigInt
lastIndex = length >>> flip (-) one >>> fromInt

getNetworkId
  :: forall (a :: Type)
   . ConstraintsM a NetworkId
getNetworkId = use (_cpsToTxBody <<< _networkId)
  >>= maybe (lift $ asks _.networkId) pure
