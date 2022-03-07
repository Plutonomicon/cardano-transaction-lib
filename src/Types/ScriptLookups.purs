module Types.ScriptLookups
  ( MkTxError(..)
  , OgmiosTxOut(..)
  , PublicKeyOgmiosTxOut
  , ScriptLookups(..)
  , ScriptOgmiosTxOut
  , _PublicKeyOgmiosTxOut
  , _PublicKeyOgmiosTxOut'
  , _ScriptOgmiosTxOut
  , _ScriptOgmiosTxOut'
  , _address
  , _datum
  , _validator
  , _value
  , fromScriptOutput
  , fromTxOut
  , mintingPolicy
  , mkUnbalancedTx
  , otherData
  , otherScript
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , paymentPubKey
  , toTxOut
  , typedValidatorLookups
  , unspentOutputs
  ) where

import Prelude hiding (join)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Reader.Class (asks)
import Data.Array (singleton) as Array
import Data.Array ((:), catMaybes, elem, toUnfoldable)
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), either, note)
import Data.Foldable (foldM)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Generic.Rep (class Generic)
import Data.Lattice (join)
import Data.Lens ((%~), (.~), (<>~), (^.), lens', view)
import Data.Lens.At (at)
import Data.Lens.Iso (iso)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (prism')
-- import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
-- import Data.Lens.Lens.Tuple (_1, _2)
import Data.Lens.Types (Iso', Lens', Prism', Traversal')
import Data.List (List(Nil, Cons))
import Data.Map (catMaybes) as Map
import Data.Map (Map, empty, lookup, singleton, union)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Helpers ((<\>), liftEither)
import QueryM (QueryM)
import Scripts
  ( mintingPolicyHash
  , scriptHash
  , validatorHash
  , validatorHashAddress
  )
import Serialization.Address
  ( Address
  , NetworkId
  , addressPaymentCred
  , withStakeCredential
  )
import Types.Datum (Datum, DatumHash, Redeemer, datumHash)
import Types.Interval (POSIXTimeRange, posixTimeRangeToTransactionSlot)
import Types.Scripts
  ( MintingPolicy
  , MintingPolicyHash(MintingPolicyHash)
  , TypedValidator(TypedValidator)
  , Validator
  , ValidatorHash(ValidatorHash)
  )
import Data.Symbol (SProxy(SProxy))
import Transaction (ModifyTxError, attachDatum, attachRedeemer)
import Types.Transaction (Redeemer) as Transaction
import Types.Transaction
  ( Transaction
  , TransactionOutput(TransactionOutput)
  , TxBody
  , _body
  , _inputs
  , _mint
  , _network_id
  , _outputs
  , _plutus_scripts
  , _required_signers
  , _witness_set
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
  , ScriptOutput(ScriptOutput)
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
import Undefined (undefined)

--------------------------------------------------------------------------------
-- Misc. helpers (can remove after `convertRedeemer` defined)
--------------------------------------------------------------------------------
-- TO DO: replace with real function call to Haskell server
convertRedeemer :: Redeemer -> QueryM Transaction.Redeemer
convertRedeemer = undefined

--------------------------------------------------------------------------------
-- OgmiotsTxOut type (new version)
--------------------------------------------------------------------------------
-- ChainIndexTxOut will be replaced by OgmiosTxOut. Note we already have an
-- OgmiosTxOut in JsonWsp.OgmiosTxOut but the one in this module includes extra
-- information. OPTION 1 follows ChainIndexTxOut closely although I don't know
--  if we can construct Validator and Datum (see below)
-- OPTION 2 is simpler although less informative and closer to JsonWsp.OgmiosTxOut

-------------------------------- OPTION 1 --------------------------------------
-- If we can deserialise the below information from Ogmios, we should replace
-- the datatype JsonWsp.OgmiosTxOut.
-- 1) Follows ChainIndexTxOut more directly which would benefit users copying
-- code from Haskell including related lenses below.
-- 2) Can be simplified (see OPTION 2) to get rid of sum type although its
-- meaning may be obsecured. This is more closely related to our current
-- OgmiosTxOut
-- 3) Can we coincide this with Ogmios query? In particular, can we
-- even get Validator and Datum directly or are we restricted to just their
-- hashes?
data OgmiosTxOut
  = PublicKeyOgmiosTxOut PublicKeyOgmiosTxOut
  | ScriptOgmiosTxOut ScriptOgmiosTxOut

derive instance Generic OgmiosTxOut _
derive instance Eq OgmiosTxOut

instance Show OgmiosTxOut where
  show = genericShow

-- Write as a type alias for convenience when writing Lenses.
type PublicKeyOgmiosTxOut =
  { address :: Address
  , value :: Value
  }

-- Separate as a type alias for convenience when writing Lenses
type PublicKeyOgmiosTxOutTuple = Address /\ Value

-- Separate as a type alias for convenience when writing Lenses
type ScriptOgmiosTxOut =
  { address :: Address
  , validator :: Either ValidatorHash Validator
  , datum :: Either DatumHash Datum
  , value :: Value
  }

-- Separate as a type alias for convenience when writing Lenses
type ScriptOgmiosTxOutTuple =
  Address /\ Either ValidatorHash Validator /\ Either DatumHash Datum /\ Value

--------------------------------------------------------------------------------
-- OgmiosTxOut Option 1 helpers and lenses
--------------------------------------------------------------------------------
-- | Converts a transaction output from the Ogmios TxOut to the internal
-- | transaction input.
-- |
-- | Note that converting from `OgmiosTxOut` to `TxOutRef` and back to
-- | `OgmiosTxOut` loses precision (`Datum` and `Validator` are changed to
-- | `DatumHash` and `ValidatorHash` respectively).
-- | This can fail because of `datumHash` invocation.
toTxOut :: OgmiosTxOut -> Maybe TransactionOutput
toTxOut (PublicKeyOgmiosTxOut { address, value }) =
  pure $ TransactionOutput
    { address
    , amount: value
    , data_hash: Nothing
    }
toTxOut (ScriptOgmiosTxOut { address, datum: Left datHash, value }) =
  pure $ TransactionOutput
    { address
    , amount: value
    , data_hash: Just datHash
    }
toTxOut (ScriptOgmiosTxOut { address, datum: Right datum, value }) = do
  -- We want failure for `ScriptOgmiosTxOut` if we can't get the datum hash,
  -- as opposed to setting it to `Nothing` which would suggest we have a wallet
  -- address.
  datHash <- datumHash datum
  pure $ TransactionOutput
    { address
    , amount: value
    , data_hash: Just datHash
    }

-- | Converts an internal transaction output to the Ogmios transaction output.
fromTxOut :: TransactionOutput -> Maybe OgmiosTxOut
fromTxOut (TransactionOutput { address, amount: value, data_hash }) = do
  paymentCred <- addressPaymentCred address
  paymentCred # withStakeCredential
    { onKeyHash: const $ pure $ PublicKeyOgmiosTxOut { address, value }
    , onScriptHash: \sh -> data_hash >>= \dh ->
        pure $ ScriptOgmiosTxOut
          { address, validator: Left $ ValidatorHash sh, datum: Left dh, value }
    }

-- | Converts an Ogmios Transaction output to a `ScriptOutput`. This is impure
-- |because of `validatorHash`.
toScriptOutput :: OgmiosTxOut -> Effect (Maybe ScriptOutput)
toScriptOutput (ScriptOgmiosTxOut { validator, datum, value }) = runMaybeT do
  -- Recall: validator is a validator hash or validator
  vHash <- MaybeT $ either (pure <<< Just) validatorHash validator
  dHash <- MaybeT $ pure $ either Just datumHash datum
  MaybeT $ pure $ Just $ ScriptOutput
    { validatorHash: vHash
    , value
    , datumHash: dHash
    }
toScriptOutput _ = pure Nothing

-- | Converts an `ScriptOutput` to Ogmios Transaction output.
fromScriptOutput :: NetworkId -> ScriptOutput -> OgmiosTxOut
fromScriptOutput
  networkId
  (ScriptOutput { validatorHash: vh, value, datumHash: dh }) =
  ScriptOgmiosTxOut
    { address: validatorHashAddress networkId vh
    , validator: Left vh
    , datum: Left dh
    , value
    }

-- | Lenses to replicate the Plutus API
--
-- We don't have Template Purescript, therefore they are created manually.
-- I'm using a Purescript convention to underscore lenses, which appears to be
-- "the opposite" of Haskell, where underscores are used in a record type
-- then TH derives. This could be confusing with FFIs.

_value :: Lens' OgmiosTxOut Value
_value = lens' case _ of
  PublicKeyOgmiosTxOut rec@{ value } ->
    Tuple value \val -> PublicKeyOgmiosTxOut rec { value = val }
  ScriptOgmiosTxOut rec@{ value } ->
    Tuple value \val -> ScriptOgmiosTxOut rec { value = val }

_address :: Lens' OgmiosTxOut Address
_address = lens' case _ of
  PublicKeyOgmiosTxOut rec@{ address } ->
    Tuple address \addr -> PublicKeyOgmiosTxOut rec { address = addr }
  ScriptOgmiosTxOut rec@{ address } ->
    Tuple address \addr -> ScriptOgmiosTxOut rec { address = addr }

-- Can be an `AffineTraversal'` also
_validator :: Traversal' OgmiosTxOut (Either ValidatorHash Validator)
_validator = _ScriptOgmiosTxOut <<< prop (SProxy :: SProxy "validator")

-- Can be an `AffineTraversal'` also
_datum :: Traversal' OgmiosTxOut (Either DatumHash Datum)
_datum = _ScriptOgmiosTxOut <<< prop (SProxy :: SProxy "datum")

_PublicKeyOgmiosTxOut :: Prism' OgmiosTxOut PublicKeyOgmiosTxOut
_PublicKeyOgmiosTxOut = prism' PublicKeyOgmiosTxOut case _ of
  PublicKeyOgmiosTxOut x -> Just x
  ScriptOgmiosTxOut _ -> Nothing

_PublicKeyOgmiosTxOut' :: Prism' OgmiosTxOut PublicKeyOgmiosTxOutTuple
_PublicKeyOgmiosTxOut' = _PublicKeyOgmiosTxOut <<< _PubKeyIso
  where
  _PubKeyIso :: Iso' PublicKeyOgmiosTxOut PublicKeyOgmiosTxOutTuple
  _PubKeyIso = iso recordToTuple tupleToRecord

  recordToTuple :: PublicKeyOgmiosTxOut -> PublicKeyOgmiosTxOutTuple
  recordToTuple { address, value } = address /\ value

  tupleToRecord :: PublicKeyOgmiosTxOutTuple -> PublicKeyOgmiosTxOut
  tupleToRecord (address /\ value) = { address, value }

_ScriptOgmiosTxOut :: Prism' OgmiosTxOut ScriptOgmiosTxOut
_ScriptOgmiosTxOut = prism' ScriptOgmiosTxOut case _ of
  ScriptOgmiosTxOut x -> Just x
  PublicKeyOgmiosTxOut _ -> Nothing

_ScriptOgmiosTxOut' :: Prism' OgmiosTxOut ScriptOgmiosTxOutTuple
_ScriptOgmiosTxOut' = _ScriptOgmiosTxOut <<< _ScriptIso
  where
  _ScriptIso :: Iso' ScriptOgmiosTxOut ScriptOgmiosTxOutTuple
  _ScriptIso = iso recordToTuple tupleToRecord

  recordToTuple :: ScriptOgmiosTxOut -> ScriptOgmiosTxOutTuple
  recordToTuple { address, validator, datum, value } =
    address /\ validator /\ datum /\ value

  tupleToRecord :: ScriptOgmiosTxOutTuple -> ScriptOgmiosTxOut
  tupleToRecord (address /\ validator /\ datum /\ value) =
    { address, validator, datum, value }

-- -------------------------------- OPTION 2 --------------------------------------
-- -- Isomorphic to OPTION 1, is more succinct but less expressive. In particular
-- -- Maybe failure and success implies a public key and script key address
-- -- respectively. The ' is just temporary so we can compile.
-- -- 1) Cleaner but the Maybe doesn't really explain what's going on. Feels like
-- -- a bad use of validating instead of parsing.
-- -- 2) Potentially less flexible in the long run, in case we want to use datum at
-- -- public keys for example (not sure if we'd want to though)
-- -- 3) More changes for Haskell code.
-- -- 4) Less lenses (or none) required given row polymorphism convenience.
-- data OgmiosTxOut' = OgmiosTxOut' OgmiosTxOut''

-- -- Can think of a better name:
-- type OgmiosTxOut'' =
--   { address :: Address
--   , validatorDatum :: Maybe ValidatorDatum -- Nothing = PublicKey, Just = Script
--   , value :: Value
--   }
--
-- --------------------------------------------------------------------------------
-- -- OgmiosTxOut Option 2 helpers and lenses
-- --------------------------------------------------------------------------------
-- toTxOut' :: OgmiosTxOut' -> Maybe TransactionOutput
-- toTxOut' (OgmiosTxOut' { address, validatorDatum: Nothing, value }) =
--   pure $ TransactionOutput
--     { address
--     , amount: value
--     , data_hash: Nothing
--     }
-- toTxOut' (OgmiosTxOut' { address, validatorDatum: Just (_ /\ Left dh), value }) =
--   pure $  TransactionOutput
--     { address
--     , amount: value
--     , data_hash: Just dh
--     }
-- toTxOut' (OgmiosTxOut' { address, validatorDatum: Just (_ /\ Right d), value }) = do
--   -- We want failure if we can't get the datum hash, as opposed to setting it to
--   -- `Nothing` which would suggest we have a wallet address.
--   dh <- datumHash d
--   pure $ TransactionOutput
--     { address
--     , amount: value
--     , data_hash: Just dh
--     }

-- -- Less of a requirement for Lenses here but can still be useful for the user
-- _OgmiosTxOut' :: Lens' OgmiosTxOut' OgmiosTxOut''
-- _OgmiosTxOut' =
--   lens' (\(OgmiosTxOut' record) -> record /\ (\rec -> OgmiosTxOut' rec))

-- -- Can be AffineTraversal' also
-- _validator' :: Traversal' OgmiosTxOut' (Either ValidatorHash Validator)
-- _validator' = _OgmiosTxOut' <<< _validatorDatum <<< _Just <<< _1

-- -- Can be AffineTraversal' also
-- _datum' :: Traversal' OgmiosTxOut' (Either DatumHash Datum)
-- _datum' = _OgmiosTxOut' <<< _validatorDatum <<< _Just <<< _2

-- _validatorDatum
--   :: forall (a :: Type) (r :: Row Type). Lens' { validatorDatum :: a | r } a
-- _validatorDatum = prop (SProxy :: SProxy "validatorDatum")

-- fromTxOut' :: TransactionOutput -> Maybe OgmiosTxOut'
-- fromTxOut' (TransactionOutput { address, amount: value, data_hash }) = do
--   paymentCred <- addressPaymentCred address
--   paymentCred # withStakeCredential
--     { onKeyHash: const
--         $ pure
--         $ OgmiosTxOut' { address, validatorDatum: Nothing, value }
--     , onScriptHash: \sh -> data_hash >>= \dh ->
--         pure $ OgmiosTxOut'
--           { address
--           , validatorDatum: Just (Left (ValidatorHash sh) /\ Left dh)
--           , value
--           }
--     }

-- type ValidatorDatum =
--   (Either ValidatorHash Validator) /\ (Either DatumHash Datum)

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
  -- NOTE: not sure how to make sense of Typed Validators ATM.
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
-- Create ScriptLookups helpers
--------------------------------------------------------------------------------
-- FIX ME: Need to work out what to do with TypedValidator and constraints.
-- Also, some of these functions are impure which doesn't reflect the original
-- library.

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

-- | A script lookups value with a minting policy script. This is impure, unlike
-- | Plutus because we invoke `mintingPolicyHash`.
mintingPolicy
  :: forall (a :: Type). MintingPolicy -> Effect (Maybe (ScriptLookups a))
mintingPolicy pl =
  mintingPolicyHash pl >>= case _ of
    Nothing -> pure Nothing
    Just hsh ->
      pure $ Just $ over ScriptLookups _ { mps = singleton hsh pl } mempty

-- | A script lookups value with a validator script. This is impure, unlike
-- | Plutus because we invoke `validatorHash`.
otherScript :: forall (a :: Type). Validator -> Effect (Maybe (ScriptLookups a))
otherScript vl = do
  validatorHash vl >>= case _ of
    Nothing -> pure Nothing
    Just vh ->
      pure $ Just $
        over ScriptLookups _ { otherScripts = singleton vh vl } mempty

-- | A script lookups value with a datum. This is contained in `Maybe` context
-- | because we invoke `datumHash`.
otherData :: forall (a :: Type). Datum -> Maybe (ScriptLookups a)
otherData dt = do
  dh <- datumHash dt
  pure $ over ScriptLookups _ { otherData = singleton dh dt } mempty

-- | A script lookups value with a payment public key
paymentPubKey :: forall (a :: Type). PaymentPubKey -> ScriptLookups a
paymentPubKey ppk =
  over ScriptLookups
    _ { paymentPubKeyHashes = singleton (payPubKeyHash ppk) ppk }
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

newtype ConstraintProcessingState = ConstraintProcessingState
  { unbalancedTx :: UnbalancedTx
  -- The unbalanced transaction that we're building
  , mintRedeemers :: Map MintingPolicyHash Redeemer
  -- Redeemers for minting policies
  , valueSpentBalancesInputs :: ValueSpentBalances
  -- Balance of the values given and required for the transaction's inputs
  , valueSpentBalancesOutputs :: ValueSpentBalances
  -- Balance of the values produced and required for the transaction's outputs
  }

derive instance Newtype ConstraintProcessingState _

_unbalancedTx :: Lens' ConstraintProcessingState UnbalancedTx
_unbalancedTx = lens'
  \(ConstraintProcessingState rec@{ unbalancedTx }) ->
    Tuple
      unbalancedTx
      \unbalTx -> ConstraintProcessingState rec { unbalancedTx = unbalTx }

_mintRedeemers
  :: Lens' ConstraintProcessingState (Map MintingPolicyHash Redeemer)
_mintRedeemers = lens'
  \(ConstraintProcessingState rec@{ mintRedeemers }) ->
    Tuple
      mintRedeemers
      \mintReds -> ConstraintProcessingState rec { mintRedeemers = mintReds }

_valueSpentBalancesInputs :: Lens' ConstraintProcessingState ValueSpentBalances
_valueSpentBalancesInputs = lens'
  \(ConstraintProcessingState rec@{ valueSpentBalancesInputs }) ->
    Tuple
      valueSpentBalancesInputs
      \vsbi -> ConstraintProcessingState rec { valueSpentBalancesInputs = vsbi }

_valueSpentBalancesOutputs :: Lens' ConstraintProcessingState ValueSpentBalances
_valueSpentBalancesOutputs = lens'
  \(ConstraintProcessingState rec@{ valueSpentBalancesOutputs }) ->
    Tuple
      valueSpentBalancesOutputs
      \vsbo -> ConstraintProcessingState rec { valueSpentBalancesOutputs = vsbo }

missingValueSpent :: ValueSpentBalances -> Value
missingValueSpent (ValueSpentBalances { required, provided }) =
  let
    difference = required <> negation provided
    _ /\ missing = split difference
  in
    missing

totalMissingValue :: ConstraintProcessingState -> Value
totalMissingValue
  ( ConstraintProcessingState
      { valueSpentBalancesInputs, valueSpentBalancesOutputs }
  ) =
  missingValueSpent valueSpentBalancesInputs `join`
    missingValueSpent valueSpentBalancesOutputs

initialCps :: ConstraintProcessingState
initialCps = ConstraintProcessingState
  { unbalancedTx: emptyUnbalancedTx
  , mintRedeemers: empty
  , valueSpentBalancesInputs:
      ValueSpentBalances { required: mempty, provided: mempty }
  , valueSpentBalancesOutputs:
      ValueSpentBalances { required: mempty, provided: mempty }
  }

provide :: Value -> ValueSpentBalances
provide provided = ValueSpentBalances { provided, required: mempty }

require :: Value -> ValueSpentBalances
require required = ValueSpentBalances { required, provided: mempty }

-- TODO: Plutus uses a bunch of FromData and ToData constraints we'll probably
-- need to replicate if we uses `InputConstraint`s and `OutputConstraint`s
-- i.e. ~ foldM addOwnInput txOwnInputs then foldM addOwnOutput txOwnOutputs
-- | Resolve some `TxConstraints` by modifying the `UnbalancedTx` in the
-- |  `ConstraintProcessingState`
processLookupsAndConstraints
  :: forall (a :: Type) (o :: Type)
   . ScriptLookups a
  -> TxConstraints o o
  -> QueryM (Either MkTxError ConstraintProcessingState)
processLookupsAndConstraints
  lookups
  (TxConstraints { constraints }) = runExceptT $
  ExceptT
    ( foldM
        ( \cps constr -> runExceptT do -- FIX ME: factor this out
            cps' <- ExceptT $ pure cps
            ExceptT $ processConstraint lookups cps' constr
        )
        (Right initialCps)
        constraints
    )
    >>= ExceptT <<< addMintingRedeemers
    >>= ExceptT <<< addMissingValueSpent lookups
    >>= ExceptT <<< updateUtxoIndex lookups

mkUnbalancedTx
  :: forall (a :: Type) (o :: Type)
   . ScriptLookups a
  -> TxConstraints o o
  -> QueryM (Either MkTxError UnbalancedTx)
mkUnbalancedTx lookups txConstraints = runExceptT $
  ExceptT (processLookupsAndConstraints lookups txConstraints)
    >>= liftEither <<< Right <<< view _unbalancedTx

-- | Add the remaining balance of the total value that the tx must spend.
-- | See note [Balance of value spent]
addMissingValueSpent
  :: forall (a :: Type)
   . ScriptLookups a
  -> ConstraintProcessingState
  -> QueryM (Either MkTxError ConstraintProcessingState)
addMissingValueSpent (ScriptLookups lookups) cps = do
  let missing = totalMissingValue cps
  if isZero missing then pure $ Right cps
  else runExceptT do
    -- add 'missing' to the transaction's outputs. This ensures that the
    -- wallet will add a corresponding input when balancing the
    -- transaction.
    -- Step 4 of the process described in [Balance of value spent]
    let
      pkh' = lookups.ownPaymentPubKeyHash
      skh' = lookups.ownStakePubKeyHash
      mNetworkId = cps ^. _cpsToTxBody <<< _network_id
    networkId <- liftEither $ note NetworkIdMissing mNetworkId
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
    liftEither $ Right $ cps
      # _cpsToTxBody <<< _outputs %~ (:) txOut

-- FIX ME: Conversion of all redeemers will be done together.
addMintingRedeemers
  :: ConstraintProcessingState
  -> QueryM (Either MkTxError ConstraintProcessingState)
addMintingRedeemers cps = do
  let
    mPs = cps ^.
      _unbalancedTx <<< _transaction <<< _witness_set <<< _plutus_scripts
  case mPs of
    Nothing -> pure $ Right cps -- return as normal if Nothing
    Just ps -> do
      -- CLS-style Redeemers:
      reds <- traverse convertRedeemer (cps ^. _mintRedeemers)
      foldWithIndexM
        ( \mpsHash cps' red -> runExceptT do
            cps'' <- liftEither cps'
            -- FIX ME: Not sure about wrapping MintingPolicyHash on Array of generic
            -- PlutusScripts mapped to their ScriptHash.
            containsHash <- ExceptT $ liftEffect $ traverse scriptHash ps <#>
              catMaybes >>> map MintingPolicyHash >>> elem mpsHash >>> Right
            if containsHash then
              ExceptT $ liftEffect $ attachToCps attachRedeemer cps'' red
            else throwError (MintingPolicyNotFound mpsHash)
        ) -- If Nothing, just return the state.
        (Right cps)
        reds

updateUtxoIndex
  :: forall (a :: Type)
   . ScriptLookups a
  -> ConstraintProcessingState
  -> QueryM (Either MkTxError ConstraintProcessingState)
updateUtxoIndex (ScriptLookups { txOutputs }) cps = do
  txOutsMap <- liftEffect $ Map.catMaybes <$> traverse toScriptOutput txOutputs
  -- Left bias towards original map, hence flip:
  pure $ Right $ cps # _unbalancedTx <<< _utxoIndex %~ flip union txOutsMap

data MkTxError
  -- = TypeCheckFailed ConnectionError
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
  | CannotSatisfyAny
  | CannotConvertPOSIXTimeRange POSIXTimeRange

derive instance Generic MkTxError _
derive instance Eq MkTxError

instance Show MkTxError where
  show = genericShow

lookupTxOutRef
  :: forall (a :: Type)
   . ScriptLookups a
  -> TxOutRef
  -> Either MkTxError OgmiosTxOut
lookupTxOutRef (ScriptLookups { txOutputs }) outRef =
  note (TxOutRefNotFound outRef) (lookup outRef txOutputs)

lookupDatum
  :: forall (a :: Type)
   . ScriptLookups a
  -> DatumHash
  -> Either MkTxError Datum
lookupDatum (ScriptLookups { otherData: otherData' }) dh =
  note (DatumNotFound dh) (lookup dh otherData')

lookupMintingPolicy
  :: forall (a :: Type)
   . ScriptLookups a
  -> MintingPolicyHash
  -> Either MkTxError MintingPolicy
lookupMintingPolicy (ScriptLookups { mps }) mph =
  note (MintingPolicyNotFound mph) (lookup mph mps)

lookupValidator
  :: forall (a :: Type)
   . ScriptLookups a
  -> ValidatorHash
  -> Either MkTxError Validator
lookupValidator (ScriptLookups { otherScripts }) vh =
  note (ValidatorHashNotFound vh) (lookup vh otherScripts)

-- Plutus uses MonadState for `ConstraintProcessingState`
-- Plutus also uses lenses to modify the state in this, we're going to skip this
-- for now and just return in `Either` context.
-- | Modify the `UnbalancedTx` so that it satisfies the constraints, if
-- | possible. Fails if a hash is missing from the lookups, or if an output
-- | of the wrong type is spent.
processConstraint
  :: forall (a :: Type)
   . ScriptLookups a
  -> ConstraintProcessingState -- Plutus uses MonadState with related lenses
  -> TxConstraint
  -> QueryM (Either MkTxError ConstraintProcessingState)
processConstraint lookups cps = case _ of
  MustIncludeDatum datum -> liftEffect $ attachToCps attachDatum cps datum
  MustValidateIn posixTimeRange -> do
    sc <- asks _.slotConfig
    case posixTimeRangeToTransactionSlot sc posixTimeRange of
      Nothing -> pure $ throwError $ CannotConvertPOSIXTimeRange posixTimeRange
      Just { timeToLive, validityStartInterval } ->
        pure $ Right $ cps # _cpsToTxBody <<< _Newtype %~
          _ { ttl = timeToLive, validity_start_interval = validityStartInterval }
  MustBeSignedBy pkh -> do
    let
      sigs = Array.singleton <<< payPubKeyRequiredSigner <$>
        lookup pkh (unwrap lookups).paymentPubKeyHashes
    pure $ Right $ cps
      # _cpsToTxBody <<< _required_signers <>~ sigs
  MustSpendAtLeast vl -> pure $ Right $
    cps # _valueSpentBalancesInputs <>~ require vl
  MustProduceAtLeast vl -> pure $ Right $
    cps # _valueSpentBalancesOutputs <>~ require vl
  MustSpendPubKeyOutput txo ->
    pure case lookupTxOutRef lookups txo of
      Right (PublicKeyOgmiosTxOut { value }) ->
        -- POTENTIAL FIX ME: Plutus has Tx.TxIn and Tx.PubKeyTxIn -- TxIn
        -- keeps track TxOutRef and TxInType (the input type, whether consuming
        -- script, public key or simple script)
        Right $ cps
          # _cpsToTxBody <<< _inputs %~ (:) txo
          # _valueSpentBalancesInputs <>~ provide value
      Left err -> throwError err
      _ -> throwError $ TxOutRefWrongType txo
  MustSpendScriptOutput txo red ->
    case lookupTxOutRef lookups txo of
      Right (ScriptOgmiosTxOut { validator, datum, value }) -> runExceptT do
        -- Check in the `OgmiosTx` for the validator, then look for it in the
        -- `otherScripts` map.
        _val' <-
          liftEither $ either (lookupValidator lookups) pure validator

        -- Check in the `OgmiosTx` for the datum, then look for it in the
        -- `otherData` map.
        dataValue <- liftEither $ either (lookupDatum lookups) pure datum
        -- TODO: Attach Validator once https://github.com/Plutonomicon/cardano-browser-tx/issues/145
        -- is ready.
        redT <- ExceptT $ Right <$> convertRedeemer red
        datCps <- ExceptT $ liftEffect $ attachToCps attachDatum cps dataValue
        redCps <- ExceptT $ liftEffect $ attachToCps attachRedeemer datCps redT
        -- TODO: When witnesses are properly segregated we can
        --       probably get rid of the `otherData` map and of
        --       `lookupDatum`
        -- let input = Tx.scriptTxIn txo validator red dataValue
        liftEither $ Right $ redCps
          # _cpsToTxBody <<< _inputs %~ (:) txo
          # _valueSpentBalancesInputs <>~ provide value
      Left err -> pure $ throwError err
      _ -> pure $ throwError (TxOutRefWrongType txo)
  MustMintValue mpsHash red tn i -> pure do
    _mintingPolicyScript <- lookupMintingPolicy lookups mpsHash
    cs <- note (MintingPolicyHashNotCurrencySymbol mpsHash) $ mpsSymbol mpsHash
    let value = mkSingletonValue' cs tn
    -- If i is negative we are burning tokens. The tokens burned must
    -- be provided as an input. So we add the value burnt to
    -- 'valueSpentBalancesInputs'. If i is positive then new tokens are created
    -- which must be added to 'valueSpentBalancesOutputs'.
    mintVal /\ valCps <-
      if i < zero then do
        val <- note (CannotMakeValue cs tn i) $ value (negate i)
        Right $ val /\ (cps # _valueSpentBalancesInputs <>~ provide val)
      else do
        val <- note (CannotMakeValue cs tn i) $ value i
        Right $ val /\ (cps # _valueSpentBalancesOutputs <>~ provide val)

    -- TODO: Attach MintingPolicy once https://github.com/Plutonomicon/cardano-browser-tx/issues/145
    -- is ready - I presume this will use the same functionality.
    -- unbalancedTx . tx . Tx.mintScripts %= Set.insert mintingPolicyScript
    Right $ valCps
      # _cpsToTxBody <<< _mint <>~ Just (wrap mintVal)
      # _mintRedeemers <<< at mpsHash .~ Just red
  MustPayToPubKeyAddress pkh _mSkh mDt amount -> runExceptT do -- FIX ME, USE _mSkh as backup.
    -- if datum is presented, add it to 'datumWitnesses'
    datCps <- maybe
      (liftEither $ Right cps)
      (ExceptT <<< liftEffect <<< attachToCps attachDatum cps)
      mDt
    let
      data_hash = mDt >>= datumHash
      mNetworkId = datCps ^. _cpsToTxBody <<< _network_id -- FIX ME: refactor into helper?
    networkId <- liftEither $ note NetworkIdMissing mNetworkId
    let
      txOut = TransactionOutput
        { address: payPubKeyHashAddress networkId pkh, amount, data_hash }
    liftEither $ Right $ datCps
      # _cpsToTxBody <<< _outputs %~ (:) txOut
      # _valueSpentBalancesOutputs <>~ provide amount
  MustPayToOtherScript vlh dt amount -> runExceptT do
    let mNetworkId = cps ^. _cpsToTxBody <<< _network_id
    networkId <- liftEither $ note NetworkIdMissing mNetworkId
    let
      data_hash = datumHash dt
      txOut = TransactionOutput
        { address: validatorHashAddress networkId vlh, amount, data_hash }
    datCps <- ExceptT $ liftEffect $ attachToCps attachDatum cps dt
    liftEither $ Right $ datCps
      # _cpsToTxBody <<< _outputs %~ (:) txOut
      # _valueSpentBalancesOutputs <>~ provide amount
  MustHashDatum dh dt ->
    if datumHash dt == Just dh then liftEffect $ attachToCps attachDatum cps dt
    else pure $ throwError $ DatumWrongHash dh dt
  MustSatisfyAnyOf xs ->
    let
      -- For each sublist, process the constraint from left to right, using the
      -- new state in the subsequent call. If we fail, reset to the initial
      -- state, `cps` and attempt for the next sublist. If a sublist is
      -- processed successfully, we can stop early - I think this is how Plutus
      -- behaves (POTENTIAL FIX ME). If all sublists fail, we fail overall as
      -- seen in the base case.
      tryNext
        :: List (List TxConstraint)
        -> QueryM (Either MkTxError ConstraintProcessingState)
      tryNext Nil = pure $ throwError $ CannotSatisfyAny
      tryNext (Cons ys zs) = do
        -- Note this implicitly resets state to original cps (see initial state
        -- of `FoldM` below which is fixed).
        foldM
          ( \cps' constr -> runExceptT do
              cps'' <- ExceptT $ pure cps'
              ExceptT $ processConstraint lookups cps'' constr `catchError`
                \_ -> tryNext zs
          )
          (Right cps) -- This is always the initial state, even upon failure.
          ys
    in
      tryNext (toUnfoldable $ map toUnfoldable xs)

-- Attach a Datum or Redeemer depending on the handler. They share error
-- type so this is fine.
attachToCps
  :: forall (b :: Type)
   . (b -> Transaction -> Effect (Either ModifyTxError Transaction))
  -> ConstraintProcessingState
  -> b -- Redeemer or Datum
  -> Effect (Either MkTxError ConstraintProcessingState)
attachToCps handler cps''@(ConstraintProcessingState cps') object =
  let
    trx = (unwrap cps'.unbalancedTx).transaction
  in
    handler object trx <#>
      bimap
        ModifyTx
        (\trx' -> cps'' # _unbalancedTx <<< _transaction .~ trx')

-- Helper to focus from `ConstraintProcessingState` down to `TxBody`.
_cpsToTxBody :: Lens' ConstraintProcessingState TxBody
_cpsToTxBody = _unbalancedTx <<< _transaction <<< _body