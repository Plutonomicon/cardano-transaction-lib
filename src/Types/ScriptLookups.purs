module Types.ScriptLookups
  ( OgmiosTxOut''
  , OgmiosTxOut'(..)
  , OgmiosTxOut(..)
  , PublicKeyOgmiosTxOut
  , ScriptLookups(..)
  , ScriptOgmiosTxOut
  , ValidatorDatum
  , _OgmiosTxOut'
  , _PublicKeyOgmiosTxOut
  , _PublicKeyOgmiosTxOut'
  , _ScriptOgmiosTxOut
  , _ScriptOgmiosTxOut'
  , _address
  , _datum
  , _datum'
  , _validator
  , _validator'
  , _validatorDatum
  , _value
  , appendFirstMaybe
  , datumHash
  , fromTxOut
  , fromTxOut'
  , mintingPolicy
  , mintingPolicyHash
  , otherData
  , otherScript
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , paymentPubKey
  , redeemerHash
  , stakeValidatorHash
  , toTxOut
  , toTxOut'
  , typedValidatorLookups
  , unspentOutputs
  , validatorHash
  ) where

import Prelude
import Data.Either (Either(Left, Right))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Lens (lens')
import Data.Lens.Iso (iso)
import Data.Lens.Prism (prism')
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Lens.Lens.Tuple (_1, _2)
import Data.Lens.Types (Iso', Lens', Prism', Traversal')
import Data.Map (Map, empty, singleton, union)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Maybe.First (First(First))
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Deserialization.WitnessSet (plutusScriptBytes)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Serialization.Address (Address, addressPaymentCred, withStakeCredential)
import Serialization.Hash (ScriptHash, scriptHashFromBytes)
import Serialization.WitnessSet (convertPlutusScript)
import Types.PlutusData (Datum, DatumHash, RedeemerHash)
import Types.Scripts
  ( MintingPolicy
  , MintingPolicyHash
  , PlutusScript
  , StakeValidator
  , StakeValidatorHash
  , TypedValidator(TypedValidator)
  , Validator
  , ValidatorHash(ValidatorHash)
  )
import Data.Symbol (SProxy(SProxy))
import Types.Transaction (Redeemer, TransactionOutput(TransactionOutput))
import Types.UnbalancedTransaction
  ( TxOutRef
  , PaymentPubKey
  , PaymentPubKeyHash
  , StakePubKeyHash
  , payPubKeyHash
  )
import Types.Value (Value)
import Undefined (undefined)

--------------------------------------------------------------------------------
-- ScriptHash helpers
--------------------------------------------------------------------------------
-- FIX ME (create issues), feels like this should be possible without Effect
-- and unsafePerformEffect feels bad. See comments below, I feel like we
-- shouldn't need Effect in the first place.
-- If there's no way around this, we'd have to lift a lot of this module into
-- Effect, not sure how problematic this will be in the longrun.
scriptHash :: PlutusScript -> ScriptHash
scriptHash = unsafePerformEffect <<< scriptHash'
  where
  -- Is there to make this pure? We'd need to go from internal PlutusScript to
  -- ByteArray instead of via convertPlutusScript. Again, I think this is actually
  -- safe because a valid PlutusScript should be hashable.
  scriptHash' :: PlutusScript -> Effect ScriptHash
  scriptHash' =
    map (unsafePartial fromJust <<< scriptHashFromBytes <<< plutusScriptBytes)
      <<< convertPlutusScript

-- FIX ME:
datumHash :: Datum -> DatumHash
datumHash = undefined

-- FIX ME:
redeemerHash :: Redeemer -> RedeemerHash
redeemerHash = undefined

validatorHash :: Validator -> ValidatorHash
validatorHash = plutusScriptHash

mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
mintingPolicyHash = plutusScriptHash

stakeValidatorHash :: StakeValidator -> StakeValidatorHash
stakeValidatorHash = plutusScriptHash

plutusScriptHash
  :: forall (m :: Type) (n :: Type)
   . Newtype m PlutusScript
  => Newtype n ScriptHash
  => m
  -> n
plutusScriptHash = wrap <<< scriptHash <<< unwrap

--------------------------------------------------------------------------------
-- OgmiotsTxOut type (new version)
--------------------------------------------------------------------------------
-- ChainIndexTxOut will be replaced by OgmiosTxOut. Note we already have an
-- OgmiosTxOut in JsonWsp.OgmiosTxOut - that may be not be sufficient so
-- consider the two options below. OPTION 1 follows ChainIndexTxOut closely
-- although I don't know if we can construct Validator and Datum (see below)
-- OPTION 2 is simpler although less informative.

-------------------------------- OPTION 1 --------------------------------------
-- If we can deserialise the below information from Ogmios, we should replace
-- the datatype JsonWsp.OgmiosTxOut.
-- 1) Follows ChainIndexTxOut more directly which would benefit users copying
-- code from Haskell including related lenses below.
-- 2) Can be simplified (see OPTION 2) to get rid of sum type although its
-- meaning may be obsecured.
-- 3) IMPORTANT: Can we coincide this with Ogmios query? In particular, can we
-- even get Validator and Datum directly or we just restricted to their hashes?
data OgmiosTxOut
  = PublicKeyOgmiosTxOut PublicKeyOgmiosTxOut
  | ScriptOgmiosTxOut ScriptOgmiosTxOut

derive instance Generic OgmiosTxOut _
derive instance Eq OgmiosTxOut

instance Show OgmiosTxOut where
  show = genericShow

type PublicKeyOgmiosTxOut =
  { address :: Address
  , value :: Value
  }

type PublicKeyOgmiosTxOut' = Address /\ Value

type ScriptOgmiosTxOut =
  { address :: Address
  , validator :: Either ValidatorHash Validator
  , datum :: Either DatumHash Datum
  , value :: Value
  }

type ScriptOgmiosTxOut' =
  Address /\ Either ValidatorHash Validator /\ Either DatumHash Datum /\ Value

--------------------------------------------------------------------------------
-- OgmiotsTxOut Option 1 helpers and lenses
--------------------------------------------------------------------------------
-- | Converts a transaction output from the Ogmios TxOut to the internal
-- | transaction input.
-- |
-- | Note that converting from `OgmiosTxOut` to `TxOutRef` and back to
-- | `OgmiosTxOut` loses precision (`Datum` and `Validator` are changed to
-- | `DatumHash` and `ValidatorHash` respectively)
toTxOut :: OgmiosTxOut -> TransactionOutput
toTxOut (PublicKeyOgmiosTxOut { address, value }) =
  TransactionOutput
    { address
    , amount: value
    , data_hash: Nothing
    }
toTxOut (ScriptOgmiosTxOut { address, datum: Left datHash, value }) =
  TransactionOutput
    { address
    , amount: value
    , data_hash: Just datHash
    }
toTxOut (ScriptOgmiosTxOut { address, datum: Right datum, value }) =
  TransactionOutput
    { address
    , amount: value
    , data_hash: Just $ datumHash datum
    }

-- | Converts a internal transaction output to the Ogmios transaction output.
fromTxOut :: TransactionOutput -> Maybe OgmiosTxOut
fromTxOut (TransactionOutput { address, amount: value, data_hash }) = do
  paymentCred <- addressPaymentCred address
  paymentCred # withStakeCredential
    { onKeyHash: const $ pure $ PublicKeyOgmiosTxOut { address, value }
    , onScriptHash: \sh -> data_hash >>= \dh ->
        pure $ ScriptOgmiosTxOut
          { address, validator: Left $ ValidatorHash sh, datum: Left dh, value }
    }

-- Lenses to replicate the Plutus API incase anyone uses these, we don't have
-- Template Purescript so they need to be created manually.
--
-- I'm using a seemingly Purescript convention to underscore lenses, which seems
-- to be "the opposite" of Haskell, where underscores are used in a record type
-- then TH derives lenses. This could be confusing with FFIs.
_value :: Lens' OgmiosTxOut Value
_value = lens' case _ of
  PublicKeyOgmiosTxOut { address, value } ->
    Tuple
      value
      \val -> PublicKeyOgmiosTxOut { address, value: val }
  ScriptOgmiosTxOut { address, validator, datum, value } ->
    Tuple
      value
      \val -> ScriptOgmiosTxOut { address, validator, datum, value: val }

_address :: Lens' OgmiosTxOut Address
_address = lens' case _ of
  PublicKeyOgmiosTxOut { address, value } ->
    Tuple
      address
      \addr -> PublicKeyOgmiosTxOut { address: addr, value }
  ScriptOgmiosTxOut { address, validator, datum, value } ->
    Tuple
      address
      \addr -> ScriptOgmiosTxOut { address: addr, validator, datum, value }

-- Can be AffineTraversal' also
_validator :: Traversal' OgmiosTxOut (Either ValidatorHash Validator)
_validator = _ScriptOgmiosTxOut' <<< _validatorField
  where
  _validatorField
    :: forall (a :: Type) (r :: Row Type). Lens' { validator :: a | r } a
  _validatorField = prop (SProxy :: SProxy "validator")

-- Can be AffineTraversal' also
_datum :: Traversal' OgmiosTxOut (Either DatumHash Datum)
_datum = _ScriptOgmiosTxOut' <<< _datumField
  where
  _datumField :: forall (a :: Type) (r :: Row Type). Lens' { datum :: a | r } a
  _datumField = prop (SProxy :: SProxy "datum")

_PublicKeyOgmiosTxOut' :: Prism' OgmiosTxOut PublicKeyOgmiosTxOut
_PublicKeyOgmiosTxOut' = prism' PublicKeyOgmiosTxOut case _ of
  PublicKeyOgmiosTxOut x -> Just x
  ScriptOgmiosTxOut _ -> Nothing

_PublicKeyOgmiosTxOut :: Prism' OgmiosTxOut PublicKeyOgmiosTxOut'
_PublicKeyOgmiosTxOut = _PublicKeyOgmiosTxOut' <<< _PubKeyIso

_ScriptOgmiosTxOut' :: Prism' OgmiosTxOut ScriptOgmiosTxOut
_ScriptOgmiosTxOut' = prism' ScriptOgmiosTxOut case _ of
  ScriptOgmiosTxOut x -> Just x
  PublicKeyOgmiosTxOut _ -> Nothing

_ScriptOgmiosTxOut :: Prism' OgmiosTxOut ScriptOgmiosTxOut'
_ScriptOgmiosTxOut = _ScriptOgmiosTxOut' <<< _ScriptIso

_PubKeyIso :: Iso' PublicKeyOgmiosTxOut PublicKeyOgmiosTxOut'
_PubKeyIso = iso recordToTuple tupleToRecord
  where
  recordToTuple :: PublicKeyOgmiosTxOut -> PublicKeyOgmiosTxOut'
  recordToTuple { address, value } = address /\ value

  tupleToRecord :: PublicKeyOgmiosTxOut' -> PublicKeyOgmiosTxOut
  tupleToRecord (address /\ value) = { address, value }

_ScriptIso :: Iso' ScriptOgmiosTxOut ScriptOgmiosTxOut'
_ScriptIso = iso recordToTuple tupleToRecord
  where
  recordToTuple :: ScriptOgmiosTxOut -> ScriptOgmiosTxOut'
  recordToTuple { address, validator, datum, value } =
    address /\ validator /\ datum /\ value

  tupleToRecord :: ScriptOgmiosTxOut' -> ScriptOgmiosTxOut
  tupleToRecord (address /\ validator /\ datum /\ value) =
    { address, validator, datum, value }

-------------------------------- OPTION 2 --------------------------------------
-- Isomorphic to OPTION 1 but is more succient but less expressive. In particular
-- Maybe failure and success provides a public key and script key address
-- respectively. The ' is just temporary so we can compile.
-- 1) Cleaner but the Maybe doesn't really explain what's going on. Feels like
-- a bad use of validating instead of parsing.
-- 2) Potentially less flexible in the long run, in case we want to use datum at
-- public keys for example (seems unlikely given the eutxo set up).
-- 3) More changes for Haskell code.
-- 4) Less lenses required.
data OgmiosTxOut' = OgmiosTxOut' OgmiosTxOut''

-- Can think of a better name:
type OgmiosTxOut'' =
  { address :: Address
  , validatorDatum :: Maybe ValidatorDatum -- Nothing = PublicKey, Just = Script
  , value :: Value
  }

--------------------------------------------------------------------------------
-- OgmiotsTxOut Option 2 helpers and lenses
--------------------------------------------------------------------------------
toTxOut' :: OgmiosTxOut' -> TransactionOutput
toTxOut' (OgmiosTxOut' { address, validatorDatum: Nothing, value }) =
  TransactionOutput
    { address
    , amount: value
    , data_hash: Nothing
    }
toTxOut' (OgmiosTxOut' { address, validatorDatum: Just (_ /\ Left dh), value }) =
  TransactionOutput
    { address
    , amount: value
    , data_hash: Just dh
    }
toTxOut' (OgmiosTxOut' { address, validatorDatum: Just (_ /\ Right d), value }) =
  TransactionOutput
    { address
    , amount: value
    , data_hash: Just $ datumHash d
    }

-- Less of a requirement for Lenses here but can still be useful for the user
_OgmiosTxOut' :: Lens' OgmiosTxOut' OgmiosTxOut''
_OgmiosTxOut' =
  lens' (\(OgmiosTxOut' record) -> record /\ (\rec -> OgmiosTxOut' rec))

-- Can be AffineTraversal' also
_validator' :: Traversal' OgmiosTxOut' (Either ValidatorHash Validator)
_validator' = _OgmiosTxOut' <<< _validatorDatum <<< _Just <<< _1

-- Can be AffineTraversal' also
_datum' :: Traversal' OgmiosTxOut' (Either DatumHash Datum)
_datum' = _OgmiosTxOut' <<< _validatorDatum <<< _Just <<< _2

_validatorDatum
  :: forall (a :: Type) (r :: Row Type). Lens' { validatorDatum :: a | r } a
_validatorDatum = prop (SProxy :: SProxy "validatorDatum")

fromTxOut' :: TransactionOutput -> Maybe OgmiosTxOut'
fromTxOut' (TransactionOutput { address, amount: value, data_hash }) = do
  paymentCred <- addressPaymentCred address
  paymentCred # withStakeCredential
    { onKeyHash: const
        $ pure
        $ OgmiosTxOut' { address, validatorDatum: Nothing, value }
    , onScriptHash: \sh -> data_hash >>= \dh ->
        pure $ OgmiosTxOut'
          { address
          , validatorDatum: Just (Left (ValidatorHash sh) /\ Left dh)
          , value
          }
    }

type ValidatorDatum =
  (Either ValidatorHash Validator) /\ (Either DatumHash Datum)

--------------------------------------------------------------------------------
-- ScriptLookups type
--------------------------------------------------------------------------------
newtype ScriptLookups (a :: Type) = ScriptLookups
  { mps :: Map MintingPolicyHash MintingPolicy -- Minting policies that the script interacts with
  , outputs :: Map TxOutRef OgmiosTxOut -- Unspent outputs that the script may want to spend
  , otherScripts :: Map ValidatorHash Validator -- Validators of scripts other than "our script"
  , otherData :: Map DatumHash Datum --  Datums that we might need
  , paymentPubKeyHashes :: Map PaymentPubKeyHash PaymentPubKey -- Public keys that we might need
  , typedValidator :: Maybe (TypedValidator a) -- The script instance with the typed validator hash & actual compiled program
  -- NOTE: we don't have Apply Code at the moment so maybe redundant.
  , ownPaymentPubKeyHash :: Maybe PaymentPubKeyHash -- The contract's payment public key hash, used for depositing tokens etc.
  , ownStakePubKeyHash :: Maybe StakePubKeyHash -- The contract's stake public key hash (optional)
  }

derive instance Generic (ScriptLookups a) _
derive instance Newtype (ScriptLookups a) _
derive newtype instance Eq (ScriptLookups a)

instance Show (ScriptLookups a) where
  show = genericShow

-- Perhaps move this and appendLastMaybe into helpers
appendFirstMaybe :: forall (a :: Type). Maybe a -> Maybe a -> Maybe a
appendFirstMaybe m m' = on (<>) First m m' # \(First m'') -> m''

infixr 5 appendFirstMaybe as <\>

-- Using `Data.Map.union`, we can reeplicate left-biased <> from Data.Map used
-- in Plutus (*not* Plutus' internal Map that uses something like unionWith (<>))
instance Semigroup (ScriptLookups a) where
  append (ScriptLookups l) (ScriptLookups r) =
    ScriptLookups
      { mps: l.mps `union` r.mps
      , outputs: l.outputs `union` r.outputs
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
    , outputs: empty
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
unspentOutputs mp = over ScriptLookups _ { outputs = mp } mempty

-- | A script lookups value with a minting policy script
mintingPolicy :: forall (a :: Type). MintingPolicy -> ScriptLookups a
mintingPolicy pl =
  let
    hsh = mintingPolicyHash pl
  in
    over ScriptLookups _ { mps = singleton hsh pl } mempty

-- | A script lookups value with a validator script
otherScript :: forall (a :: Type). Validator -> ScriptLookups a
otherScript vl =
  let
    vh = validatorHash vl
  in
    over ScriptLookups _ { otherScripts = singleton vh vl } mempty

-- | A script lookups value with a datum
otherData :: forall (a :: Type). Datum -> ScriptLookups a
otherData dt =
  let
    dh = datumHash dt
  in
    over ScriptLookups _ { otherData = singleton dh dt } mempty

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

data MkTxError
  -- = TypeCheckFailed ConnectionError
  = TxOutRefNotFound TxOutRef
  | TxOutRefWrongType TxOutRef
  | DatumNotFound DatumHash
  | MintingPolicyNotFound MintingPolicyHash
  | ValidatorHashNotFound ValidatorHash
  | OwnPubKeyMissing
  | TypedValidatorMissing
  | DatumWrongHash DatumHash Datum
  | CannotSatisfyAny
derive instance Generic MkTxError _
derive instance Eq MkTxError

instance Show MkTxError where
  show = genericShow