module CTL.Internal.Types.TxConstraints
  ( DatumPresence(DatumInline, DatumWitness)
  , InputConstraint(InputConstraint)
  , InputWithScriptRef(RefInput, SpendInput)
  , OutputConstraint(OutputConstraint)
  , TxConstraint
      ( MustIncludeDatum
      , MustValidateIn
      , MustBeSignedBy
      , MustSpendAtLeast
      , MustProduceAtLeast
      , MustSpendPubKeyOutput
      , MustSpendScriptOutput
      , MustSpendNativeScriptOutput
      , MustReferenceOutput
      , MustMintValue
      , MustPayToPubKeyAddress
      , MustPayToScript
      , MustPayToNativeScript
      , MustHashDatum
      , MustSatisfyAnyOf
      , MustNotBeValid
      )
  , TxConstraints(TxConstraints)
  , addTxIn
  , isSatisfiable
  , modifiesUtxoSet
  , mustBeSignedBy
  , mustHashDatum
  , mustIncludeDatum
  , mustMintCurrency
  , mustMintCurrencyUsingScriptRef
  , mustMintCurrencyWithRedeemer
  , mustMintCurrencyWithRedeemerUsingScriptRef
  , mustMintValue
  , mustMintValueWithRedeemer
  , mustPayToScript
  , mustPayToNativeScript
  , mustPayToScriptWithScriptRef
  , mustPayToPubKey
  , mustPayToPubKeyAddress
  , mustPayToPubKeyAddressWithDatum
  , mustPayToPubKeyAddressWithDatumAndScriptRef
  , mustPayToPubKeyAddressWithScriptRef
  , mustPayToPubKeyWithDatum
  , mustPayToPubKeyWithDatumAndScriptRef
  , mustPayToPubKeyWithScriptRef
  , mustProduceAtLeast
  , mustProduceAtLeastTotal
  , mustReferenceOutput
  , mustSatisfyAnyOf
  , mustSpendAtLeast
  , mustSpendAtLeastTotal
  , mustSpendNativeScriptOutput
  , mustSpendPubKeyOutput
  , mustSpendScriptOutput
  , mustSpendScriptOutputUsingScriptRef
  , mustValidateIn
  , mustNotBeValid
  , pubKeyPayments
  , requiredDatums
  , requiredMonetaryPolicies
  , requiredSignatories
  , singleton
  ) where

import Prelude hiding (join)

import CTL.Internal.Cardano.Types.NativeScript (NativeScript)
import CTL.Internal.Cardano.Types.ScriptRef (ScriptRef)
import CTL.Internal.NativeScripts (NativeScriptHash)
import CTL.Internal.Types.Datum (Datum)
import CTL.Internal.Types.Interval
  ( POSIXTimeRange
  , always
  , intersection
  , isEmpty
  )
import CTL.Internal.Types.PubKeyHash (PaymentPubKeyHash, StakePubKeyHash)
import CTL.Internal.Types.Redeemer (Redeemer, unitRedeemer)
import CTL.Internal.Types.Scripts (MintingPolicyHash, ValidatorHash)
import CTL.Internal.Types.TokenName (TokenName)
import CTL.Internal.Types.Transaction (DataHash, TransactionInput)
import CTL.Plutus.Types.CurrencySymbol
  ( CurrencySymbol
  , currencyMPSHash
  )
import CTL.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import CTL.Plutus.Types.Value (Value, flattenNonAdaAssets, isZero)
import Data.Array (concat, (:))
import Data.Array as Array
import Data.Bifunctor (class Bifunctor)
import Data.BigInt (BigInt)
import Data.Foldable (class Foldable, any, foldMap, foldl, foldr, null)
import Data.Generic.Rep (class Generic)
import Data.Lattice (join)
import Data.Map (fromFoldableWith, toUnfoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (guard)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.TypeError (class Warn, Text)

--------------------------------------------------------------------------------
-- TxConstraints Type and related
--------------------------------------------------------------------------------
-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/Ledger-Constraints.html
-- Plutus rev: cc72a56eafb02333c96f662581b57504f8f8992f via Plutus-apps (localhost): abe4785a4fc4a10ba0c4e6417f0ab9f1b4169b26

-- | Constraints on transactions that want to spend script outputs
data TxConstraint
  = MustIncludeDatum Datum
  | MustValidateIn POSIXTimeRange
  | MustBeSignedBy PaymentPubKeyHash
  | MustSpendAtLeast Value
  | MustProduceAtLeast Value
  | MustSpendPubKeyOutput TransactionInput
  | MustSpendNativeScriptOutput TransactionInput NativeScript
  | MustSpendScriptOutput TransactionInput Redeemer (Maybe InputWithScriptRef)
  | MustReferenceOutput TransactionInput
  | MustMintValue MintingPolicyHash Redeemer TokenName BigInt
      (Maybe InputWithScriptRef)
  | MustPayToPubKeyAddress PaymentPubKeyHash (Maybe StakePubKeyHash)
      (Maybe (Datum /\ DatumPresence))
      (Maybe ScriptRef)
      Value
  | MustPayToNativeScript NativeScriptHash Value
  | MustPayToScript ValidatorHash Datum DatumPresence (Maybe ScriptRef) Value
  | MustHashDatum DataHash Datum
  | MustSatisfyAnyOf (Array (Array TxConstraint))
  | MustNotBeValid

derive instance Eq TxConstraint
derive instance Generic TxConstraint _

instance Show TxConstraint where
  show x = genericShow x

data InputWithScriptRef
  = RefInput TransactionUnspentOutput
  | SpendInput TransactionUnspentOutput

derive instance Eq InputWithScriptRef
derive instance Generic InputWithScriptRef _

instance Show InputWithScriptRef where
  show = genericShow

-- | `DatumPresence` describes how datum should be stored in the transaction
-- | when paying to a script.
data DatumPresence
  -- | `DatumInline` asserts the datum should be stored in the
  -- | `TransactionOutput` as an inline datum.
  = DatumInline
  -- | `DatumWitness` asserts the datum's hash be stored in the
  -- | `TransactionOutput`, and the datum added to the transactions witnesses.
  | DatumWitness

derive instance Eq DatumPresence
derive instance Generic DatumPresence _

instance Show DatumPresence where
  show x = genericShow x

newtype InputConstraint (i :: Type) = InputConstraint
  { redeemer :: i
  , txOutRef :: TransactionInput
  }

derive instance Generic (InputConstraint i) _
derive instance Newtype (InputConstraint i) _
derive instance Functor InputConstraint
derive newtype instance Eq i => Eq (InputConstraint i)

instance Show i => Show (InputConstraint i) where
  show = genericShow

newtype OutputConstraint (o :: Type) = OutputConstraint
  { datum :: o
  , value :: Value
  }

derive instance Generic (OutputConstraint o) _
derive instance Newtype (OutputConstraint o) _
derive instance Functor OutputConstraint
derive newtype instance Eq o => Eq (OutputConstraint o)

instance Show o => Show (OutputConstraint o) where
  show = genericShow

-- | Restrictions placed on the allocation of funds to outputs of transactions.
newtype TxConstraints (i :: Type) (o :: Type) = TxConstraints
  { constraints :: Array TxConstraint
  , ownInputs :: Array (InputConstraint i)
  , ownOutputs :: Array (OutputConstraint o)
  }

derive instance Generic (TxConstraints i o) _
derive instance Newtype (TxConstraints i o) _
derive newtype instance (Eq i, Eq o) => Eq (TxConstraints i o)
-- Array concatenation allowing duplicates like Plutus
derive newtype instance Semigroup (TxConstraints i o)
derive newtype instance Monoid (TxConstraints i o)

instance (Show i, Show o) => Show (TxConstraints i o) where
  show = genericShow

instance Bifunctor TxConstraints where
  bimap f g (TxConstraints txc@{ ownInputs, ownOutputs }) =
    TxConstraints txc
      { ownInputs = map (map f) ownInputs
      , ownOutputs = map (map g) ownOutputs
      }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
-- | Adds a `TransactionInput` as an input constraint with an arbitrary
-- | redeemer.
addTxIn
  :: forall (i :: Type) (o :: Type)
   . TransactionInput
  -> i
  -> TxConstraints i o
  -> TxConstraints i o
addTxIn outRef red (TxConstraints txc@{ ownInputs }) =
  let
    ic = InputConstraint { redeemer: red, txOutRef: outRef }
  in
    TxConstraints txc { ownInputs = ic : ownInputs }

singleton
  :: forall (i :: Type) (o :: Type). TxConstraint -> TxConstraints i o
singleton a = over TxConstraints _ { constraints = Array.singleton a } mempty

-- | `mustValidateIn r` requires the transaction's time range to be contained
-- |  in `r`.
mustValidateIn
  :: forall (i :: Type) (o :: Type). POSIXTimeRange -> TxConstraints i o
mustValidateIn = singleton <<< MustValidateIn

-- | Require the transaction to be signed by the public key.
mustBeSignedBy
  :: forall (i :: Type) (o :: Type). PaymentPubKeyHash -> TxConstraints i o
mustBeSignedBy = singleton <<< MustBeSignedBy

-- | Require the transaction to include a datum.
mustIncludeDatum :: forall (i :: Type) (o :: Type). Datum -> TxConstraints i o
mustIncludeDatum = singleton <<< MustIncludeDatum

-- | Require the transaction to reference (not spend!) the given unspent
-- | transaction output.
mustReferenceOutput
  :: forall (i :: Type) (o :: Type). TransactionInput -> TxConstraints i o
mustReferenceOutput = singleton <<< MustReferenceOutput

-- | Lock the value with a payment public key hash and (optionally) a stake
-- | public key hash.
mustPayToPubKeyAddress
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> StakePubKeyHash
  -> Value
  -> TxConstraints i o
mustPayToPubKeyAddress pkh skh =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) Nothing Nothing

-- | Lock the value and datum with a payment public key hash and (optionally) a
-- | stake public key hash.
mustPayToPubKeyAddressWithDatum
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> StakePubKeyHash
  -> Datum
  -> DatumPresence
  -> Value
  -> TxConstraints i o
mustPayToPubKeyAddressWithDatum pkh skh datum dtp =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) (Just $ datum /\ dtp)
    Nothing

mustPayToPubKeyAddressWithScriptRef
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> StakePubKeyHash
  -> ScriptRef
  -> Value
  -> TxConstraints i o
mustPayToPubKeyAddressWithScriptRef pkh skh scriptRef =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) Nothing (Just scriptRef)

mustPayToPubKeyAddressWithDatumAndScriptRef
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> StakePubKeyHash
  -> Datum
  -> DatumPresence
  -> ScriptRef
  -> Value
  -> TxConstraints i o
mustPayToPubKeyAddressWithDatumAndScriptRef pkh skh datum dtp scriptRef =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) (Just $ datum /\ dtp)
    (Just scriptRef)

-- | Lock the value with a public key
mustPayToPubKey
  :: forall (i :: Type) (o :: Type)
   . Warn
       ( Text
           "Some wallets may not recognize addresses without a staking key component. Consider using mustPayToPubKeyAddress"
       )
  => PaymentPubKeyHash
  -> Value
  -> TxConstraints i o
mustPayToPubKey pkh =
  singleton <<< MustPayToPubKeyAddress pkh Nothing Nothing Nothing

-- | Lock the value and datum with a payment public key hash
mustPayToPubKeyWithDatum
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> Datum
  -> DatumPresence
  -> Value
  -> TxConstraints i o
mustPayToPubKeyWithDatum pkh datum dtp =
  singleton <<< MustPayToPubKeyAddress pkh Nothing (Just $ datum /\ dtp) Nothing

mustPayToPubKeyWithScriptRef
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> ScriptRef
  -> Value
  -> TxConstraints i o
mustPayToPubKeyWithScriptRef pkh scriptRef =
  singleton <<< MustPayToPubKeyAddress pkh Nothing Nothing (Just scriptRef)

mustPayToPubKeyWithDatumAndScriptRef
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> Datum
  -> DatumPresence
  -> ScriptRef
  -> Value
  -> TxConstraints i o
mustPayToPubKeyWithDatumAndScriptRef pkh datum dtp scriptRef =
  singleton <<< MustPayToPubKeyAddress pkh Nothing (Just $ datum /\ dtp)
    (Just scriptRef)

-- | Note that CTL does not have explicit equivalents of Plutus'
-- | `mustPayToTheScript` or `mustPayToOtherScript`, as we have no notion
-- | of a "current" script. Thus, we have the single constraint
-- | `mustPayToScript`, and all scripts must be explicitly provided to build
-- | the transaction.
mustPayToScript
  :: forall (i :: Type) (o :: Type)
   . ValidatorHash
  -> Datum
  -> DatumPresence
  -> Value
  -> TxConstraints i o
mustPayToScript vh dt dtp vl =
  singleton (MustPayToScript vh dt dtp Nothing vl)
    <> guard (dtp == DatumWitness) (singleton $ MustIncludeDatum dt)

mustPayToScriptWithScriptRef
  :: forall (i :: Type) (o :: Type)
   . ValidatorHash
  -> Datum
  -> DatumPresence
  -> ScriptRef
  -> Value
  -> TxConstraints i o
mustPayToScriptWithScriptRef vh dt dtp scriptRef vl =
  singleton (MustPayToScript vh dt dtp (Just scriptRef) vl)
    <> guard (dtp == DatumWitness) (singleton $ MustIncludeDatum dt)

mustPayToNativeScript
  :: forall (i :: Type) (o :: Type)
   . NativeScriptHash
  -> Value
  -> TxConstraints i o
mustPayToNativeScript nsHash vl =
  singleton (MustPayToNativeScript nsHash vl)

-- | Mint the given `Value`
mustMintValue :: forall (i :: Type) (o :: Type). Value -> TxConstraints i o
mustMintValue = mustMintValueWithRedeemer unitRedeemer

-- | Mint the given `Value` by accessing non-Ada assets.
mustMintValueWithRedeemer
  :: forall (i :: Type) (o :: Type)
   . Redeemer
  -> Value
  -> TxConstraints i o
mustMintValueWithRedeemer redeemer =
  Array.fold <<< map tokenConstraint <<< flattenNonAdaAssets
  where
  tokenConstraint
    :: CurrencySymbol /\ TokenName /\ BigInt -> TxConstraints i o
  tokenConstraint (cs /\ tn /\ amount) =
    let
      mintingPolicyHash = currencyMPSHash cs
    in
      mustMintCurrencyWithRedeemer mintingPolicyHash redeemer tn amount

-- | Create the given amount of the currency.
mustMintCurrency
  :: forall (i :: Type) (o :: Type)
   . MintingPolicyHash
  -> TokenName
  -> BigInt
  -> TxConstraints i o
mustMintCurrency mph =
  mustMintCurrencyWithRedeemer mph unitRedeemer

mustMintCurrencyUsingScriptRef
  :: forall (i :: Type) (o :: Type)
   . MintingPolicyHash
  -> TokenName
  -> BigInt
  -> InputWithScriptRef
  -> TxConstraints i o
mustMintCurrencyUsingScriptRef mph =
  mustMintCurrencyWithRedeemerUsingScriptRef mph unitRedeemer

-- | Create the given amount of the currency
mustMintCurrencyWithRedeemer
  :: forall (i :: Type) (o :: Type)
   . MintingPolicyHash
  -> Redeemer
  -> TokenName
  -> BigInt
  -> TxConstraints i o
mustMintCurrencyWithRedeemer mph red tn amount =
  singleton (MustMintValue mph red tn amount Nothing)

mustMintCurrencyWithRedeemerUsingScriptRef
  :: forall (i :: Type) (o :: Type)
   . MintingPolicyHash
  -> Redeemer
  -> TokenName
  -> BigInt
  -> InputWithScriptRef
  -> TxConstraints i o
mustMintCurrencyWithRedeemerUsingScriptRef mph red tn amount =
  singleton <<< MustMintValue mph red tn amount <<< Just

-- | Requirement to spend inputs with at least the given value
mustSpendAtLeast :: forall (i :: Type) (o :: Type). Value -> TxConstraints i o
mustSpendAtLeast = singleton <<< MustSpendAtLeast

-- | Requirement to produce outputs with at least the given value
mustProduceAtLeast :: forall (i :: Type) (o :: Type). Value -> TxConstraints i o
mustProduceAtLeast = singleton <<< MustProduceAtLeast

mustSpendPubKeyOutput
  :: forall (i :: Type) (o :: Type). TransactionInput -> TxConstraints i o
mustSpendPubKeyOutput = singleton <<< MustSpendPubKeyOutput

mustSpendScriptOutput
  :: forall (i :: Type) (o :: Type)
   . TransactionInput
  -> Redeemer
  -> TxConstraints i o
mustSpendScriptOutput txOutRef red =
  singleton (MustSpendScriptOutput txOutRef red Nothing)

mustSpendScriptOutputUsingScriptRef
  :: forall (i :: Type) (o :: Type)
   . TransactionInput
  -> Redeemer
  -> InputWithScriptRef
  -> TxConstraints i o
mustSpendScriptOutputUsingScriptRef txOutRef red =
  singleton <<< MustSpendScriptOutput txOutRef red <<< Just

mustSpendNativeScriptOutput
  :: forall (i :: Type) (o :: Type)
   . TransactionInput
  -> NativeScript
  -> TxConstraints i o
mustSpendNativeScriptOutput txOutRef = singleton <<< MustSpendNativeScriptOutput
  txOutRef

mustHashDatum
  :: forall (i :: Type) (o :: Type). DataHash -> Datum -> TxConstraints i o
mustHashDatum dhsh = singleton <<< MustHashDatum dhsh

mustSatisfyAnyOf
  :: forall (f :: Type -> Type) (i :: Type) (o :: Type)
   . Foldable f
  => f (TxConstraints i o)
  -> TxConstraints i o
mustSatisfyAnyOf =
  Array.fromFoldable
    >>> map (_.constraints <<< unwrap)
    >>> MustSatisfyAnyOf
    >>> singleton

-- | Marks the transaction as invalid, requiring at least one script execution
-- | to fail. Despite failure, the transaction can still be submitted into the
-- | chain and collateral will be lost.
mustNotBeValid :: forall (i :: Type) (o :: Type). TxConstraints i o
mustNotBeValid = singleton $ MustNotBeValid

-- | Are the constraints satisfiable given the time intervals?
isSatisfiable :: forall (i :: Type) (o :: Type). TxConstraints i o -> Boolean
isSatisfiable (TxConstraints { constraints }) =
  let
    intervals =
      Array.mapMaybe
        ( case _ of
            MustValidateIn i -> Just i
            _ -> Nothing
        )
        constraints
    itvl = foldl intersection always intervals
  in
    not (isEmpty itvl)

pubKeyPayments
  :: forall (i :: Type) (o :: Type)
   . TxConstraints i o
  -> Array (PaymentPubKeyHash /\ Value)
pubKeyPayments (TxConstraints { constraints }) =
  toUnfoldable
    $ fromFoldableWith (<>)
    $ constraints >>=
        case _ of
          MustPayToPubKeyAddress pkh _ _ _ vl -> Array.singleton (pkh /\ vl)
          _ -> []

-- | The minimum `Value` that satisfies all `MustSpendAtLeast` constraints
mustSpendAtLeastTotal
  :: forall (i :: Type) (o :: Type). TxConstraints i o -> Value
mustSpendAtLeastTotal =
  foldr (join <<< f) mempty <<< _.constraints <<< unwrap
  where
  f :: TxConstraint -> Value
  f (MustSpendAtLeast v) = v
  f _ = mempty

-- | The minimum `Value` that satisfies all `MustProduceAtLeast` constraints
mustProduceAtLeastTotal
  :: forall (i :: Type) (o :: Type). TxConstraints i o -> Value
mustProduceAtLeastTotal =
  foldr (join <<< f) mempty <<< _.constraints <<< unwrap
  where
  f :: TxConstraint -> Value
  f (MustProduceAtLeast v) = v
  f _ = mempty

requiredSignatories
  :: forall (i :: Type) (o :: Type)
   . TxConstraints i o
  -> Array PaymentPubKeyHash
requiredSignatories = foldMap f <<< _.constraints <<< unwrap
  where
  f :: TxConstraint -> Array PaymentPubKeyHash
  f (MustBeSignedBy pkh) = Array.singleton pkh
  f _ = []

requiredMonetaryPolicies
  :: forall (i :: Type) (o :: Type)
   . TxConstraints i o
  -> Array MintingPolicyHash
requiredMonetaryPolicies = foldMap f <<< _.constraints <<< unwrap
  where
  f :: TxConstraint -> Array MintingPolicyHash
  f (MustMintValue mph _ _ _ _) = Array.singleton mph
  f _ = []

requiredDatums
  :: forall (i :: Type) (o :: Type). TxConstraints i o -> Array Datum
requiredDatums = foldMap f <<< _.constraints <<< unwrap
  where
  f :: TxConstraint -> Array Datum
  f (MustIncludeDatum dt) = Array.singleton dt
  f _ = []

-- | Check whether every transaction that satisfies the constraints has to
-- | modify the UTXO set.
modifiesUtxoSet :: forall (i :: Type) (o :: Type). TxConstraints i o -> Boolean
modifiesUtxoSet (TxConstraints { constraints, ownInputs, ownOutputs }) =
  let
    requiresInputOutput :: TxConstraint -> Boolean
    requiresInputOutput = case _ of
      MustBeSignedBy _ -> false
      MustHashDatum _ _ -> false
      MustIncludeDatum _ -> false
      MustMintValue _ _ _ _ _ -> true
      MustNotBeValid -> false
      MustPayToNativeScript _ vl -> not (isZero vl)
      MustPayToPubKeyAddress _ _ _ _ vl -> not (isZero vl)
      MustPayToScript _ _ _ _ vl -> not (isZero vl)
      MustProduceAtLeast _ -> true
      MustReferenceOutput _ -> false
      MustSatisfyAnyOf xs -> any requiresInputOutput $ concat xs
      MustSpendAtLeast _ -> true
      MustSpendNativeScriptOutput _ _ -> true
      MustSpendPubKeyOutput _ -> true
      MustSpendScriptOutput _ _ _ -> true
      MustValidateIn _ -> false
  in
    any requiresInputOutput constraints
      || not (null ownInputs)
      || not (null ownOutputs)
