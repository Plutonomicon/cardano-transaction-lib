module Types.TxConstraints
  ( InputConstraint(..)
  , OutputConstraint(..)
  , TxConstraint(..)
  , TxConstraints(..)
  , addTxIn
  , isSatisfiable
  , modifiesUtxoSet
  , mustBeSignedBy
  , mustHashDatum
  , mustIncludeDatum
  , mustMintCurrency
  , mustMintCurrencyWithRedeemer
  , mustMintValue
  , mustMintValueWithRedeemer
  , mustPayToScript
  , mustPayToPubKey
  , mustPayToPubKeyAddress
  , mustPayWithDatumToPubKey
  , mustPayWithDatumToPubKeyAddress
  , mustProduceAtLeast
  , mustProduceAtLeastTotal
  , mustSatisfyAnyOf
  , mustSpendAtLeast
  , mustSpendAtLeastTotal
  , mustSpendPubKeyOutput
  , mustSpendScriptOutput
  , mustValidateIn
  , pubKeyPayments
  , requiredDatums
  , requiredMonetaryPolicies
  , requiredSignatories
  , singleton
  ) where

import Prelude hiding (join)

import Data.Array as Array
import Data.Array ((:), concat)
import Data.Bifunctor (class Bifunctor)
import Data.BigInt (BigInt)
import Data.Foldable (class Foldable, any, foldl, foldMap, foldr, null)
import Data.Generic.Rep (class Generic)
import Data.Lattice (join)
import Data.Map (fromFoldableWith, toUnfoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\), type (/\))
import Plutus.Types.CurrencySymbol (CurrencySymbol, currencyMPSHash)
import Plutus.Types.Value (Value, isZero, flattenNonAdaAssets)
import Types.Interval (POSIXTimeRange, always, intersection, isEmpty)
import Types.Redeemer (Redeemer, unitRedeemer)
import Types.PubKeyHash (PaymentPubKeyHash, StakePubKeyHash)
import Types.Scripts (MintingPolicyHash, ValidatorHash)
import Types.Datum (Datum)
import Types.Transaction (DataHash, TransactionInput)
import Types.TokenName (TokenName)

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
  | MustSpendScriptOutput TransactionInput Redeemer
  | MustMintValue MintingPolicyHash Redeemer TokenName BigInt
  | MustPayToPubKeyAddress PaymentPubKeyHash (Maybe StakePubKeyHash)
      (Maybe Datum)
      Value
  | MustPayToScript ValidatorHash Datum Value
  | MustHashDatum DataHash Datum
  | MustSatisfyAnyOf (Array (Array TxConstraint))

derive instance Eq TxConstraint
derive instance Generic TxConstraint _

instance Show TxConstraint where
  show x = genericShow x

newtype InputConstraint (i :: Type) = InputConstraint
  { redeemer :: i
  , txOutRef :: TransactionInput
  }

derive instance Generic (InputConstraint i) _
derive instance Newtype (InputConstraint i) _
derive instance Functor InputConstraint
derive newtype instance Eq i => Eq (InputConstraint i)

instance showInputConstraint :: Show i => Show (InputConstraint i) where
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

-- | Lock the value with a public key
mustPayToPubKey
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> Value
  -> TxConstraints i o
mustPayToPubKey pkh = singleton <<< MustPayToPubKeyAddress pkh Nothing Nothing

-- | Lock the value with a payment public key hash and (optionally) a stake
-- | public key hash.
mustPayToPubKeyAddress
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> StakePubKeyHash
  -> Value
  -> TxConstraints i o
mustPayToPubKeyAddress pkh skh =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) Nothing

-- | Lock the value and datum with a payment public key hash
mustPayWithDatumToPubKey
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> Datum
  -> Value
  -> TxConstraints i o
mustPayWithDatumToPubKey pkh datum =
  singleton <<< MustPayToPubKeyAddress pkh Nothing (Just datum)

-- | Lock the value and datum with a payment public key hash and (optionally) a
-- | stake public key hash.
mustPayWithDatumToPubKeyAddress
  :: forall i o
   . PaymentPubKeyHash
  -> StakePubKeyHash
  -> Datum
  -> Value
  -> TxConstraints i o
mustPayWithDatumToPubKeyAddress pkh skh datum =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) (Just datum)

-- | Note that CTL does not have explicit equivalents of Plutus'
-- | `mustPayToTheScript` or `mustPayToOtherScript`, as we have no notion
-- | of a "current" script. Thus, we have the single constraint
-- | `mustPayToScript`, and all scripts must be explicitly provided to build
-- | the transaction.
mustPayToScript
  :: forall (i :: Type) (o :: Type)
   . ValidatorHash
  -> Datum
  -> Value
  -> TxConstraints i o
mustPayToScript vh dt vl =
  singleton (MustPayToScript vh dt vl)
    <> singleton (MustIncludeDatum dt)

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
  Array.fold <<< Array.mapMaybe tokenConstraint <<< flattenNonAdaAssets
  where
  tokenConstraint
    :: CurrencySymbol /\ TokenName /\ BigInt -> Maybe (TxConstraints i o)
  tokenConstraint (cs /\ tn /\ amount) = do
    mintingPolicyHash <- currencyMPSHash cs
    pure $ mustMintCurrencyWithRedeemer mintingPolicyHash redeemer tn amount

-- | Create the given amount of the currency.
mustMintCurrency
  :: forall (i :: Type) (o :: Type)
   . MintingPolicyHash
  -> TokenName
  -> BigInt
  -> TxConstraints i o
mustMintCurrency mph = mustMintCurrencyWithRedeemer mph unitRedeemer

-- | Create the given amount of the currency
mustMintCurrencyWithRedeemer
  :: forall (i :: Type) (o :: Type)
   . MintingPolicyHash
  -> Redeemer
  -> TokenName
  -> BigInt
  -> TxConstraints i o
mustMintCurrencyWithRedeemer mph red tn = singleton <<< MustMintValue mph red tn

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
mustSpendScriptOutput txOutRef = singleton <<< MustSpendScriptOutput txOutRef

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
          MustPayToPubKeyAddress pkh _ _ vl -> Array.singleton (pkh /\ vl)
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
  f (MustMintValue mph _ _ _) = Array.singleton mph
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
      MustSpendAtLeast _ -> true
      MustProduceAtLeast _ -> true
      MustSpendPubKeyOutput _ -> true
      MustSpendScriptOutput _ _ -> true
      MustMintValue _ _ _ _ -> true
      MustPayToPubKeyAddress _ _ _ vl -> not (isZero vl)
      MustPayToScript _ _ vl -> not (isZero vl)
      MustSatisfyAnyOf xs -> any requiresInputOutput $ concat xs
      _ -> false
  in
    any requiresInputOutput constraints
      || not (null ownInputs)
      || not (null ownOutputs)
