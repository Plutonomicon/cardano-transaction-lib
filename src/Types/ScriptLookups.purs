module Types.ScriptLookups
  ( MkUnbalancedTxError(..)
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
import Data.Lens ((%=), (<>=), (.=), lens')
import Data.Lens.At (at)
import Data.Lens.Getter (use)
import Data.Lens.Iso (iso)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (prism')
import Data.Lens.Record (prop)
import Data.Lens.Types (Iso', Lens', Prism', Traversal')
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
import Helpers ((<\>), liftEither)
import QueryM (QueryConfig, QueryM)
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
import Types.Interval
  ( POSIXTimeRange
  , SlotConfig
  , defaultSlotConfig
  , posixTimeRangeToTransactionSlot
  )
import Types.RedeemerTag (RedeemerTag(Mint, Spend))
import Types.Scripts
  ( MintingPolicy
  , MintingPolicyHash(MintingPolicyHash)
  , TypedValidator(TypedValidator)
  , Validator
  , ValidatorHash(ValidatorHash)
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
-- OgmiotsTxOut type (new version)
--------------------------------------------------------------------------------
-- ChainIndexTxOut will be replaced by OgmiosTxOut. Note we already have an
-- OgmiosTxOut in JsonWsp.OgmiosTxOut but the one in this module includes extra
-- information. OPTION 1 follows ChainIndexTxOut closely although I don't know
-- if we can construct Validator and Datum (see below)
-- OPTION 2 is simpler although less informative and closer to JsonWsp.OgmiosTxOut
-- OPTION 3 is sticking to JsonWsp.OgmiosTxOut which I suspect is what we will
-- do in the short run. Changing the functions below to accept this data type
-- should be easy and part of the review.

-------------------------------- OPTION 1 --------------------------------------
-- If we can deserialise the below information from Ogmios, we should replace
-- the datatype JsonWsp.OgmiosTxOut.
-- 1) Follows ChainIndexTxOut more directly which would benefit users copying
-- code from Haskell including related lenses below. I believe these lenses are
-- often used for off-chain code.
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

-- | Converts an Ogmios Transaction output to a `ScriptOutput`.
toScriptOutput :: OgmiosTxOut -> Maybe ScriptOutput
toScriptOutput (ScriptOgmiosTxOut { validator, datum, value }) = do
  -- Recall: validator is a validator hash or validator
  vHash <- either Just validatorHash validator
  dHash <- either Just datumHash datum
  pure $ ScriptOutput
    { validatorHash: vHash
    , value
    , datumHash: dHash
    }
toScriptOutput _ = Nothing

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
    value /\ \val -> PublicKeyOgmiosTxOut rec { value = val }
  ScriptOgmiosTxOut rec@{ value } ->
    value /\ \val -> ScriptOgmiosTxOut rec { value = val }

_address :: Lens' OgmiosTxOut Address
_address = lens' case _ of
  PublicKeyOgmiosTxOut rec@{ address } ->
    address /\ \addr -> PublicKeyOgmiosTxOut rec { address = addr }
  ScriptOgmiosTxOut rec@{ address } ->
    address /\ \addr -> ScriptOgmiosTxOut rec { address = addr }

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

-------------------------------- OPTION 3 --------------------------------------
-- Stick to the current JsonWsp.OgmiosTxOut which seems easiest.
-- I wrote the functions below with Option 1 but it should be very simple to
-- convert to option 3.

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
            -- # elem mpsHash >>> Right >>> liftEither
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
  let txOutsMap = mapMaybe toScriptOutput txOutputs
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
  | CannotSatisfyAny
  | CannotConvertPOSIXTimeRange POSIXTimeRange

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
      case txOut of
        PublicKeyOgmiosTxOut { value } -> do
          -- POTENTIAL FIX ME: Plutus has Tx.TxIn and Tx.PubKeyTxIn -- TxIn
          -- keeps track TxOutRef and TxInType (the input type, whether
          -- consuming script, public key or simple script)
          _cpsToTxBody <<< _inputs %= (:) txo
          _valueSpentBalancesInputs <>= provide value
        _ -> liftEither $ throwError $ TxOutRefWrongType txo
    MustSpendScriptOutput txo red -> runExceptT do
      txOut <- ExceptT $ lookupTxOutRef txo
      case txOut of
        ScriptOgmiosTxOut { validator, datum, value } -> do
          -- Check in the `OgmiosTx` for the validator, then look for it in the
          -- `otherScripts` map.
          plutusScript <-
            ExceptT $ either lookupValidator (pure <<< Right) validator
              <#> map unwrap

          -- Note: Plutus uses `TxIn` to attach a redeemer and datum.
          -- Check in the `OgmiosTx` for the datum, then look for it in the
          -- `otherData` map.
          dataValue <- ExceptT $ either lookupDatum (pure <<< Right) datum
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
        liftEither $ maybe
          (throwError $ MintingPolicyHashNotCurrencySymbol mpsHash)
          Right
          (mpsSymbol mpsHash)
      let value = mkSingletonValue' cs tn
      -- If i is negative we are burning tokens. The tokens burned must
      -- be provided as an input. So we add the value burnt to
      -- 'valueSpentBalancesInputs'. If i is positive then new tokens are
      -- created which must be added to 'valueSpentBalancesOutputs'.
      mintVal <-
        if i < zero then do
          v <- liftEither
            $ maybe (throwError $ CannotMakeValue cs tn i) Right
            $ value (negate i)
          _valueSpentBalancesInputs <>= provide v
          liftEither $ Right v
        else do
          v <- liftEither
            $ maybe (throwError $ CannotMakeValue cs tn i) Right
            $ value i
          _valueSpentBalancesOutputs <>= provide v
          liftEither $ Right v
      ExceptT $ attachToCps attachPlutusScript plutusScript
      _cpsToTxBody <<< _mint <>= Just (wrap mintVal)
      _mintRedeemers <<< at mpsHash .= Just red
      _redeemers <<< at Mint <>= Just (Array.singleton red)
    MustPayToPubKeyAddress pkh _ mDatum amount -> runExceptT do
      -- If datum is presented, add it to 'datumWitnesses' and Array of datums.
      maybe (liftEither $ Right unit) (ExceptT <<< addDatums) mDatum
      networkId <- ExceptT getNetworkId
      let
        data_hash = mDatum >>= datumHash
        txOut = TransactionOutput
          { address: payPubKeyHashAddress networkId pkh, amount, data_hash }
      _cpsToTxBody <<< _outputs %= (:) txOut
      _valueSpentBalancesOutputs <>= provide amount
    MustPayToOtherScript vlh datum amount -> runExceptT do
      networkId <- ExceptT getNetworkId
      let
        data_hash = datumHash datum
        txOut = TransactionOutput
          { address: validatorHashAddress networkId vlh, amount, data_hash }
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
getNetworkId = runExceptT $ use (_cpsToTxBody <<< _networkId)
  >>= liftEither <<< maybe (throwError NetworkIdMissing) Right
