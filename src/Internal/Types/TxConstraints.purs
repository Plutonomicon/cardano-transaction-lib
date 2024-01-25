module Ctl.Internal.Types.TxConstraints
  ( DatumPresence(DatumInline, DatumWitness)
  , InputConstraint(InputConstraint)
  , InputWithScriptRef(RefInput, SpendInput)
  , OutputConstraint(OutputConstraint)
  , TxConstraint
      ( MustBeSignedBy
      , MustDelegateStakePlutusScript
      , MustDelegateStakeNativeScript
      , MustDelegateStakePubKey
      , MustDeregisterStakePlutusScript
      , MustDeregisterStakeNativeScript
      , MustDeregisterStakePubKey
      , MustHashDatum
      , MustIncludeDatum
      , MustMintValue
      , MustMintValueUsingNativeScript
      , MustNotBeValid
      , MustPayToNativeScript
      , MustPayToPubKeyAddress
      , MustPayToScript
      , MustProduceAtLeast
      , MustReferenceOutput
      , MustRegisterPool
      , MustRegisterStakePubKey
      , MustRegisterStakeScript
      , MustRetirePool
      , MustSatisfyAnyOf
      , MustSpendAtLeast
      , MustSpendNativeScriptOutput
      , MustSpendPubKeyOutput
      , MustSpendScriptOutput
      , MustValidateIn
      , MustWithdrawStakePlutusScript
      , MustWithdrawStakeNativeScript
      , MustWithdrawStakePubKey
      )
  , TxConstraints(TxConstraints)
  , isSatisfiable
  , mustBeSignedBy
  , mustDelegateStakeNativeScript
  , mustDelegateStakePlutusScript
  , mustDelegateStakePubKey
  , mustDeregisterStakeNativeScript
  , mustDeregisterStakePlutusScript
  , mustDeregisterStakePubKey
  , mustHashDatum
  , mustIncludeDatum
  , mustMintCurrency
  , mustMintCurrencyUsingNativeScript
  , mustMintCurrencyUsingScriptRef
  , mustMintCurrencyWithRedeemer
  , mustMintCurrencyWithRedeemerUsingScriptRef
  , mustMintValue
  , mustMintValueWithRedeemer
  , mustNotBeValid
  , mustPayToNativeScript
  , mustPayToNativeScriptAddress
  , mustPayToPubKey
  , mustPayToPubKeyAddress
  , mustPayToPubKeyAddressWithDatum
  , mustPayToPubKeyAddressWithDatumAndScriptRef
  , mustPayToPubKeyAddressWithScriptRef
  , mustPayToPubKeyWithDatum
  , mustPayToPubKeyWithDatumAndScriptRef
  , mustPayToPubKeyWithScriptRef
  , mustPayToScript
  , mustPayToScriptAddress
  , mustPayToScriptAddressWithScriptRef
  , mustPayToScriptWithScriptRef
  , mustProduceAtLeast
  , mustProduceAtLeastTotal
  , mustReferenceOutput
  , mustRegisterPool
  , mustRegisterStakePubKey
  , mustRegisterStakeScript
  , mustRetirePool
  , mustSatisfyAnyOf
  , mustSpendAtLeast
  , mustSpendAtLeastTotal
  , mustSpendNativeScriptOutput
  , mustSpendPubKeyOutput
  , mustSpendScriptOutput
  , mustSpendScriptOutputUsingScriptRef
  , mustValidateIn
  , mustWithdrawStakeNativeScript
  , mustWithdrawStakePlutusScript
  , mustWithdrawStakePubKey
  , pubKeyPayments
  , requiredDatums
  , requiredMonetaryPolicies
  , requiredSignatories
  , singleton
  , utxoWithScriptRef
  ) where

import Prelude hiding (join)

import Ctl.Internal.Cardano.Types.NativeScript (NativeScript)
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef)
import Ctl.Internal.Cardano.Types.Transaction
  ( Epoch
  , PoolPubKeyHash
  , PoolRegistrationParams
  )
import Ctl.Internal.NativeScripts (NativeScriptHash)
import Ctl.Internal.Plutus.Types.Credential (Credential)
import Ctl.Internal.Plutus.Types.CurrencySymbol
  ( CurrencySymbol
  , currencyMPSHash
  )
import Ctl.Internal.Plutus.Types.Transaction (TransactionOutputWithRefScript)
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Ctl.Internal.Plutus.Types.Value (Value, flattenNonAdaAssets)
import Ctl.Internal.Types.Datum (Datum)
import Ctl.Internal.Types.Interval
  ( POSIXTimeRange
  , always
  , intersection
  , isEmpty
  )
import Ctl.Internal.Types.PlutusData (PlutusData)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash, StakePubKeyHash)
import Ctl.Internal.Types.Redeemer (Redeemer, unitRedeemer)
import Ctl.Internal.Types.Scripts
  ( MintingPolicyHash
  , NativeScriptStakeValidator
  , PlutusScriptStakeValidator
  , StakeValidatorHash
  , ValidatorHash
  )
import Ctl.Internal.Types.TokenName (TokenName)
import Ctl.Internal.Types.Transaction (DataHash, TransactionInput)
import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Lattice (join)
import Data.Map (Map, fromFoldableWith, toUnfoldable)
import Data.Map (singleton) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (guard)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import JS.BigInt (BigInt)
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
  | MustMintValueUsingNativeScript NativeScript TokenName BigInt
  | MustPayToPubKeyAddress PaymentPubKeyHash (Maybe StakePubKeyHash)
      (Maybe (Datum /\ DatumPresence))
      (Maybe ScriptRef)
      Value
  | MustPayToNativeScript NativeScriptHash (Maybe Credential) Value
  | MustPayToScript ValidatorHash (Maybe Credential) Datum DatumPresence
      (Maybe ScriptRef)
      Value
  | MustHashDatum DataHash Datum
  | MustRegisterStakePubKey StakePubKeyHash
  | MustDeregisterStakePubKey StakePubKeyHash
  | MustRegisterStakeScript StakeValidatorHash
  | MustDeregisterStakePlutusScript PlutusScriptStakeValidator Redeemer
  | MustDeregisterStakeNativeScript NativeScriptStakeValidator
  | MustRegisterPool PoolRegistrationParams
  | MustRetirePool PoolPubKeyHash Epoch
  | MustDelegateStakePubKey StakePubKeyHash PoolPubKeyHash
  | MustDelegateStakePlutusScript PlutusScriptStakeValidator Redeemer
      PoolPubKeyHash
  | MustDelegateStakeNativeScript NativeScriptStakeValidator PoolPubKeyHash
  | MustWithdrawStakePubKey StakePubKeyHash
  | MustWithdrawStakePlutusScript PlutusScriptStakeValidator Redeemer
  | MustWithdrawStakeNativeScript NativeScriptStakeValidator
  | MustSatisfyAnyOf (Array (Array TxConstraint))
  | MustNotBeValid

derive instance Eq TxConstraint
derive instance Generic TxConstraint _

instance Show TxConstraint where
  show x = genericShow x

-- | `InputWithScriptRef` specifies whether the underlying utxo with the
-- | reference script should be included in the transaction as a reference
-- | input.
data InputWithScriptRef
  -- | `RefInput` asserts the utxo containing the reference script should be
  -- | used as a reference input and therefore cannot be spent.
  = RefInput TransactionUnspentOutput
  -- | `SpendInput` asserts the utxo containing the reference script should be
  -- | used as a regular input and therefore can be spent.
  | SpendInput TransactionUnspentOutput

derive instance Eq InputWithScriptRef
derive instance Generic InputWithScriptRef _

instance Show InputWithScriptRef where
  show = genericShow

utxoWithScriptRef
  :: InputWithScriptRef -> Map TransactionInput TransactionOutputWithRefScript
utxoWithScriptRef inputWithRefScript = Map.singleton input output
  where
  TransactionUnspentOutput { input, output } =
    case inputWithRefScript of
      RefInput unspentOut -> unspentOut
      SpendInput unspentOut -> unspentOut

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

newtype InputConstraint = InputConstraint
  { redeemer :: PlutusData
  , txOutRef :: TransactionInput
  }

derive instance Generic InputConstraint _
derive instance Newtype InputConstraint _
derive newtype instance Eq InputConstraint

instance Show InputConstraint where
  show = genericShow

newtype OutputConstraint = OutputConstraint
  { datum :: PlutusData
  , value :: Value
  }

derive instance Generic OutputConstraint _
derive instance Newtype OutputConstraint _
derive newtype instance Eq OutputConstraint

instance Show OutputConstraint where
  show = genericShow

-- | Restrictions placed on the allocation of funds to outputs of transactions.
newtype TxConstraints = TxConstraints
  { constraints :: Array TxConstraint
  }

derive instance Generic TxConstraints _
derive instance Newtype TxConstraints _
derive newtype instance Eq TxConstraints
-- Array concatenation allowing duplicates like Plutus
derive newtype instance Semigroup TxConstraints
derive newtype instance Monoid TxConstraints

instance Show TxConstraints where
  show = genericShow

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

singleton
  :: TxConstraint -> TxConstraints
singleton a = over TxConstraints _ { constraints = Array.singleton a } mempty

-- | `mustValidateIn r` requires the transaction's time range to be contained
-- |  in `r`.
mustValidateIn
  :: POSIXTimeRange -> TxConstraints
mustValidateIn = singleton <<< MustValidateIn

-- | Require the transaction to be signed by the public key.
mustBeSignedBy
  :: PaymentPubKeyHash -> TxConstraints
mustBeSignedBy = singleton <<< MustBeSignedBy

-- | Require the transaction to include a datum.
mustIncludeDatum :: Datum -> TxConstraints
mustIncludeDatum = singleton <<< MustIncludeDatum

-- | Require the transaction to reference (not spend!) the given unspent
-- | transaction output.
mustReferenceOutput
  :: TransactionInput -> TxConstraints
mustReferenceOutput = singleton <<< MustReferenceOutput

-- | Lock the value with a public key address. (Base Address)
mustPayToPubKeyAddress
  :: PaymentPubKeyHash
  -> StakePubKeyHash
  -> Value
  -> TxConstraints
mustPayToPubKeyAddress pkh skh =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) Nothing Nothing

-- | Lock the value and datum with a public key address.
mustPayToPubKeyAddressWithDatum
  :: PaymentPubKeyHash
  -> StakePubKeyHash
  -> Datum
  -> DatumPresence
  -> Value
  -> TxConstraints
mustPayToPubKeyAddressWithDatum pkh skh datum dtp =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) (Just $ datum /\ dtp)
    Nothing

-- | Lock the value and reference script with a public key address.
mustPayToPubKeyAddressWithScriptRef
  :: PaymentPubKeyHash
  -> StakePubKeyHash
  -> ScriptRef
  -> Value
  -> TxConstraints
mustPayToPubKeyAddressWithScriptRef pkh skh scriptRef =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) Nothing (Just scriptRef)

-- | Lock the value, datum and reference script with a public key address.
mustPayToPubKeyAddressWithDatumAndScriptRef
  :: PaymentPubKeyHash
  -> StakePubKeyHash
  -> Datum
  -> DatumPresence
  -> ScriptRef
  -> Value
  -> TxConstraints
mustPayToPubKeyAddressWithDatumAndScriptRef pkh skh datum dtp scriptRef =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) (Just $ datum /\ dtp)
    (Just scriptRef)

-- | Lock the value with a public key. (Enterprise Address)
mustPayToPubKey
  :: Warn
       ( Text
           "Some wallets may not recognize addresses without a staking key component. Consider using mustPayToPubKeyAddress"
       )
  => PaymentPubKeyHash
  -> Value
  -> TxConstraints
mustPayToPubKey pkh =
  singleton <<< MustPayToPubKeyAddress pkh Nothing Nothing Nothing

-- | Lock the value and datum with a payment public key hash.
mustPayToPubKeyWithDatum
  :: PaymentPubKeyHash
  -> Datum
  -> DatumPresence
  -> Value
  -> TxConstraints
mustPayToPubKeyWithDatum pkh datum dtp =
  singleton <<< MustPayToPubKeyAddress pkh Nothing (Just $ datum /\ dtp) Nothing

-- | Lock the value and reference script with a payment public key hash.
mustPayToPubKeyWithScriptRef
  :: PaymentPubKeyHash
  -> ScriptRef
  -> Value
  -> TxConstraints
mustPayToPubKeyWithScriptRef pkh scriptRef =
  singleton <<< MustPayToPubKeyAddress pkh Nothing Nothing (Just scriptRef)

-- | Lock the value, datum and reference script with a payment public key hash.
mustPayToPubKeyWithDatumAndScriptRef
  :: PaymentPubKeyHash
  -> Datum
  -> DatumPresence
  -> ScriptRef
  -> Value
  -> TxConstraints
mustPayToPubKeyWithDatumAndScriptRef pkh datum dtp scriptRef =
  singleton <<< MustPayToPubKeyAddress pkh Nothing (Just $ datum /\ dtp)
    (Just scriptRef)

-- | Note that CTL does not have explicit equivalents of Plutus'
-- | `mustPayToTheScript` or `mustPayToOtherScript`, as we have no notion
-- | of a "current" script. Thus, we have the single constraint
-- | `mustPayToScript`, and all scripts must be explicitly provided to build
-- | the transaction.
mustPayToScript
  :: ValidatorHash
  -> Datum
  -> DatumPresence
  -> Value
  -> TxConstraints
mustPayToScript vh dt dtp vl =
  singleton (MustPayToScript vh Nothing dt dtp Nothing vl)
    <> guard (dtp == DatumWitness) (singleton $ MustIncludeDatum dt)

mustPayToScriptAddress
  :: ValidatorHash
  -> Credential
  -> Datum
  -> DatumPresence
  -> Value
  -> TxConstraints
mustPayToScriptAddress vh credential dt dtp vl =
  singleton (MustPayToScript vh (Just credential) dt dtp Nothing vl)
    <> guard (dtp == DatumWitness) (singleton $ MustIncludeDatum dt)

-- | Lock the value, datum and reference script with a script.
-- | Note that the provided reference script does *not* necessarily need to
-- | control the spending of the output, i.e. both scripts can be different.
mustPayToScriptWithScriptRef
  :: ValidatorHash
  -> Datum
  -> DatumPresence
  -> ScriptRef
  -> Value
  -> TxConstraints
mustPayToScriptWithScriptRef vh dt dtp scriptRef vl =
  singleton (MustPayToScript vh Nothing dt dtp (Just scriptRef) vl)
    <> guard (dtp == DatumWitness) (singleton $ MustIncludeDatum dt)

-- | Lock the value, datum and reference script with a script.
-- | Note that the provided reference script does *not* necessarily need to
-- | control the spending of the output, i.e. both scripts can be different.
mustPayToScriptAddressWithScriptRef
  :: forall (i :: Type) (o :: Type)
   . ValidatorHash
  -> Credential
  -> Datum
  -> DatumPresence
  -> ScriptRef
  -> Value
  -> TxConstraints
mustPayToScriptAddressWithScriptRef vh credential dt dtp scriptRef vl =
  singleton (MustPayToScript vh (Just credential) dt dtp (Just scriptRef) vl)
    <> guard (dtp == DatumWitness) (singleton $ MustIncludeDatum dt)

mustPayToNativeScript
  :: forall (i :: Type) (o :: Type)
   . NativeScriptHash
  -> Value
  -> TxConstraints
mustPayToNativeScript nsHash vl =
  singleton (MustPayToNativeScript nsHash Nothing vl)

mustPayToNativeScriptAddress
  :: forall (i :: Type) (o :: Type)
   . NativeScriptHash
  -> Credential
  -> Value
  -> TxConstraints
mustPayToNativeScriptAddress nsHash credential vl =
  singleton (MustPayToNativeScript nsHash (Just credential) vl)

-- | Mint the given `Value`
-- | The amount to mint must not be zero.
mustMintValue :: Value -> TxConstraints
mustMintValue = mustMintValueWithRedeemer unitRedeemer

-- | Mint the given `Value` by accessing non-Ada assets.
-- | The amount to mint must not be zero.
mustMintValueWithRedeemer
  :: forall (i :: Type) (o :: Type)
   . Redeemer
  -> Value
  -> TxConstraints
mustMintValueWithRedeemer redeemer =
  Array.fold <<< map tokenConstraint <<< flattenNonAdaAssets
  where
  tokenConstraint
    :: CurrencySymbol /\ TokenName /\ BigInt -> TxConstraints
  tokenConstraint (cs /\ tn /\ amount) =
    let
      mintingPolicyHash = currencyMPSHash cs
    in
      mustMintCurrencyWithRedeemer mintingPolicyHash redeemer tn amount

-- | Create the given amount of the currency.
-- | The amount to mint must not be zero.
mustMintCurrency
  :: forall (i :: Type) (o :: Type)
   . MintingPolicyHash
  -> TokenName
  -> BigInt
  -> TxConstraints
mustMintCurrency mph =
  mustMintCurrencyWithRedeemer mph unitRedeemer

mustMintCurrencyUsingNativeScript
  :: forall (i :: Type) (o :: Type)
   . NativeScript
  -> TokenName
  -> BigInt
  -> TxConstraints
mustMintCurrencyUsingNativeScript ns tk i = singleton
  (MustMintValueUsingNativeScript ns tk i)

-- | Create the given amount of the currency using a reference minting policy.
-- | The amount to mint must not be zero.
mustMintCurrencyUsingScriptRef
  :: forall (i :: Type) (o :: Type)
   . MintingPolicyHash
  -> TokenName
  -> BigInt
  -> InputWithScriptRef
  -> TxConstraints
mustMintCurrencyUsingScriptRef mph =
  mustMintCurrencyWithRedeemerUsingScriptRef mph unitRedeemer

-- | Create the given amount of the currency.
-- | The amount to mint must not be zero.
mustMintCurrencyWithRedeemer
  :: forall (i :: Type) (o :: Type)
   . MintingPolicyHash
  -> Redeemer
  -> TokenName
  -> BigInt
  -> TxConstraints
mustMintCurrencyWithRedeemer mph red tn amount =
  singleton (MustMintValue mph red tn amount Nothing)

-- | Create the given amount of the currency using a reference minting policy.
-- | The amount to mint must not be zero.
mustMintCurrencyWithRedeemerUsingScriptRef
  :: forall (i :: Type) (o :: Type)
   . MintingPolicyHash
  -> Redeemer
  -> TokenName
  -> BigInt
  -> InputWithScriptRef
  -> TxConstraints
mustMintCurrencyWithRedeemerUsingScriptRef mph red tn amount =
  singleton <<< MustMintValue mph red tn amount <<< Just

-- | Requirement to spend inputs with at least the given value
mustSpendAtLeast :: Value -> TxConstraints
mustSpendAtLeast = singleton <<< MustSpendAtLeast

-- | Requirement to produce outputs with at least the given value
mustProduceAtLeast :: Value -> TxConstraints
mustProduceAtLeast = singleton <<< MustProduceAtLeast

-- | Spend the given unspent transaction public key output.
mustSpendPubKeyOutput
  :: TransactionInput -> TxConstraints
mustSpendPubKeyOutput = singleton <<< MustSpendPubKeyOutput

-- | Spend the given unspent transaction script output.
mustSpendScriptOutput
  :: forall (i :: Type) (o :: Type)
   . TransactionInput
  -> Redeemer
  -> TxConstraints
mustSpendScriptOutput txOutRef red =
  singleton (MustSpendScriptOutput txOutRef red Nothing)

-- | Spend the given unspent transaction script output, using a reference script
-- | to satisfy the script witnessing requirement.
mustSpendScriptOutputUsingScriptRef
  :: forall (i :: Type) (o :: Type)
   . TransactionInput
  -> Redeemer
  -> InputWithScriptRef
  -> TxConstraints
mustSpendScriptOutputUsingScriptRef txOutRef red =
  singleton <<< MustSpendScriptOutput txOutRef red <<< Just

mustSpendNativeScriptOutput
  :: forall (i :: Type) (o :: Type)
   . TransactionInput
  -> NativeScript
  -> TxConstraints
mustSpendNativeScriptOutput txOutRef = singleton <<< MustSpendNativeScriptOutput
  txOutRef

mustHashDatum
  :: DataHash -> Datum -> TxConstraints
mustHashDatum dhsh = singleton <<< MustHashDatum dhsh

mustRegisterStakePubKey
  :: StakePubKeyHash -> TxConstraints
mustRegisterStakePubKey = singleton <<< MustRegisterStakePubKey

mustDeregisterStakePubKey
  :: StakePubKeyHash -> TxConstraints
mustDeregisterStakePubKey = singleton <<< MustDeregisterStakePubKey

mustRegisterStakeScript
  :: StakeValidatorHash -> TxConstraints
mustRegisterStakeScript = singleton <<< MustRegisterStakeScript

mustDeregisterStakePlutusScript
  :: forall (i :: Type) (o :: Type)
   . PlutusScriptStakeValidator
  -> Redeemer
  -> TxConstraints
mustDeregisterStakePlutusScript sv = singleton <<<
  MustDeregisterStakePlutusScript sv

mustDeregisterStakeNativeScript
  :: forall (i :: Type) (o :: Type)
   . NativeScriptStakeValidator
  -> TxConstraints
mustDeregisterStakeNativeScript = singleton <<< MustDeregisterStakeNativeScript

mustRegisterPool
  :: PoolRegistrationParams -> TxConstraints
mustRegisterPool = singleton <<< MustRegisterPool

mustRetirePool
  :: forall (i :: Type) (o :: Type)
   . PoolPubKeyHash
  -> Epoch
  -> TxConstraints
mustRetirePool poolPubKeyHash = singleton <<< MustRetirePool poolPubKeyHash

mustDelegateStakePubKey
  :: forall (i :: Type) (o :: Type)
   . StakePubKeyHash
  -> PoolPubKeyHash
  -> TxConstraints
mustDelegateStakePubKey spkh ppkh = singleton $ MustDelegateStakePubKey spkh
  ppkh

mustDelegateStakePlutusScript
  :: forall (i :: Type) (o :: Type)
   . PlutusScriptStakeValidator
  -> Redeemer
  -> PoolPubKeyHash
  -> TxConstraints
mustDelegateStakePlutusScript sv redeemer ppkh = singleton $
  MustDelegateStakePlutusScript sv redeemer ppkh

mustDelegateStakeNativeScript
  :: forall (i :: Type) (o :: Type)
   . NativeScriptStakeValidator
  -> PoolPubKeyHash
  -> TxConstraints
mustDelegateStakeNativeScript sv ppkh =
  singleton $ MustDelegateStakeNativeScript sv ppkh

mustWithdrawStakePubKey
  :: StakePubKeyHash -> TxConstraints
mustWithdrawStakePubKey spkh = singleton $ MustWithdrawStakePubKey spkh

mustWithdrawStakePlutusScript
  :: forall (i :: Type) (o :: Type)
   . PlutusScriptStakeValidator
  -> Redeemer
  -> TxConstraints
mustWithdrawStakePlutusScript validator redeemer =
  singleton $ MustWithdrawStakePlutusScript validator redeemer

mustWithdrawStakeNativeScript
  :: forall (i :: Type) (o :: Type)
   . NativeScriptStakeValidator
  -> TxConstraints
mustWithdrawStakeNativeScript =
  singleton <<< MustWithdrawStakeNativeScript

-- | Attempts to solve, in order, a sequence of constraints until the first
-- | successful try.
-- | `mustSatisfyaAnyOf` is just a way to define a chain of try-catch expressions
-- | in a declarative manner. It does not do any analysis of the constraints' semantics.
mustSatisfyAnyOf
  :: forall (f :: Type -> Type) (i :: Type) (o :: Type)
   . Foldable f
  => f (TxConstraints)
  -> TxConstraints
mustSatisfyAnyOf =
  Array.fromFoldable
    >>> map (_.constraints <<< unwrap)
    >>> MustSatisfyAnyOf
    >>> singleton

-- | Marks the transaction as invalid, requiring at least one script execution
-- | to fail. Despite failure, the transaction can still be submitted into the
-- | chain and collateral will be lost.
mustNotBeValid :: TxConstraints
mustNotBeValid = singleton $ MustNotBeValid

-- | Are the constraints satisfiable given the time intervals?
isSatisfiable :: TxConstraints -> Boolean
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
  :: TxConstraints
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
  :: TxConstraints -> Value
mustSpendAtLeastTotal =
  foldr (join <<< f) mempty <<< _.constraints <<< unwrap
  where
  f :: TxConstraint -> Value
  f (MustSpendAtLeast v) = v
  f _ = mempty

-- | The minimum `Value` that satisfies all `MustProduceAtLeast` constraints
mustProduceAtLeastTotal
  :: TxConstraints -> Value
mustProduceAtLeastTotal =
  foldr (join <<< f) mempty <<< _.constraints <<< unwrap
  where
  f :: TxConstraint -> Value
  f (MustProduceAtLeast v) = v
  f _ = mempty

requiredSignatories
  :: TxConstraints
  -> Array PaymentPubKeyHash
requiredSignatories = foldMap f <<< _.constraints <<< unwrap
  where
  f :: TxConstraint -> Array PaymentPubKeyHash
  f (MustBeSignedBy pkh) = Array.singleton pkh
  f _ = []

requiredMonetaryPolicies
  :: TxConstraints
  -> Array MintingPolicyHash
requiredMonetaryPolicies = foldMap f <<< _.constraints <<< unwrap
  where
  f :: TxConstraint -> Array MintingPolicyHash
  f (MustMintValue mph _ _ _ _) = Array.singleton mph
  f _ = []

requiredDatums
  :: TxConstraints -> Array Datum
requiredDatums = foldMap f <<< _.constraints <<< unwrap
  where
  f :: TxConstraint -> Array Datum
  f (MustIncludeDatum dt) = Array.singleton dt
  f _ = []
