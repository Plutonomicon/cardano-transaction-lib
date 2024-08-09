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
      , MustRegisterDrep
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
  , TxConstraints
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
  , mustReferenceOutput
  , mustRegisterDrep
  , mustRegisterPool
  , mustRegisterStakePubKey
  , mustRegisterStakeScript
  , mustRetirePool
  , mustSatisfyAnyOf
  , mustSpendAtLeast
  , mustSpendNativeScriptOutput
  , mustSpendPubKeyOutput
  , mustSpendScriptOutput
  , mustSpendScriptOutputUsingScriptRef
  , mustValidateIn
  , mustWithdrawStakeNativeScript
  , mustWithdrawStakePlutusScript
  , mustWithdrawStakePubKey
  , utxoWithScriptRef
  ) where

import Prelude hiding (join)

import Cardano.Types
  ( Anchor
  , AssetName
  , Credential
  , DataHash
  , Epoch
  , Mint
  , NativeScript
  , PaymentPubKeyHash
  , PlutusData
  , PlutusScript
  , PoolParams
  , PoolPubKeyHash
  , ScriptHash
  , ScriptRef
  , StakePubKeyHash
  , TransactionInput
  , TransactionOutput
  , TransactionUnspentOutput(TransactionUnspentOutput)
  , Value
  )
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.RedeemerDatum (RedeemerDatum)
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Ctl.Internal.Types.Interval (POSIXTimeRange)
import Data.Array (singleton)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (singleton) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype)
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
  = MustIncludeDatum PlutusData
  | MustValidateIn POSIXTimeRange
  | MustBeSignedBy PaymentPubKeyHash
  | MustSpendAtLeast Value
  | MustProduceAtLeast Value
  | MustSpendPubKeyOutput TransactionInput
  | MustSpendNativeScriptOutput TransactionInput NativeScript
  | MustSpendScriptOutput TransactionInput RedeemerDatum
      (Maybe InputWithScriptRef)
  | MustReferenceOutput TransactionInput
  | MustMintValue ScriptHash RedeemerDatum AssetName Int.Int
      (Maybe InputWithScriptRef)
  | MustMintValueUsingNativeScript NativeScript AssetName Int.Int
  | MustPayToPubKeyAddress PaymentPubKeyHash (Maybe StakePubKeyHash)
      (Maybe (PlutusData /\ DatumPresence))
      (Maybe ScriptRef)
      Value
  | MustPayToNativeScript ScriptHash (Maybe Credential) Value
  | MustPayToScript ScriptHash (Maybe Credential) PlutusData DatumPresence
      (Maybe ScriptRef)
      Value
  | MustHashDatum DataHash PlutusData
  | MustRegisterStakePubKey StakePubKeyHash
  | MustDeregisterStakePubKey StakePubKeyHash
  | MustRegisterStakeScript ScriptHash
  | MustDeregisterStakePlutusScript PlutusScript RedeemerDatum
  | MustDeregisterStakeNativeScript NativeScript
  | MustRegisterPool PoolParams
  | MustRetirePool PoolPubKeyHash Epoch
  | MustDelegateStakePubKey StakePubKeyHash PoolPubKeyHash
  | MustDelegateStakePlutusScript PlutusScript RedeemerDatum
      PoolPubKeyHash
  | MustDelegateStakeNativeScript NativeScript PoolPubKeyHash
  | MustWithdrawStakePubKey StakePubKeyHash
  | MustWithdrawStakePlutusScript PlutusScript RedeemerDatum
  | MustWithdrawStakeNativeScript NativeScript
  | MustSatisfyAnyOf (Array (Array TxConstraint))
  | MustNotBeValid
  | MustRegisterDrep Credential (Maybe Anchor)

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
  :: InputWithScriptRef -> Map TransactionInput TransactionOutput
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
type TxConstraints = Array TxConstraint

type TxConstraintsDeprecated = Text
  "Contract.TxConstraints is deprecated. Use `purescript-cardano-transaction-builder`"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | `mustValidateIn r` requires the transaction's time range to be contained
-- |  in `r`.
mustValidateIn
  :: Warn TxConstraintsDeprecated => POSIXTimeRange -> TxConstraints
mustValidateIn = singleton <<< MustValidateIn

-- | Require the transaction to be signed by the public key.
mustBeSignedBy
  :: Warn TxConstraintsDeprecated => PaymentPubKeyHash -> TxConstraints
mustBeSignedBy = singleton <<< MustBeSignedBy

-- | Require the transaction to include a datum.
mustIncludeDatum :: Warn TxConstraintsDeprecated => PlutusData -> TxConstraints
mustIncludeDatum = singleton <<< MustIncludeDatum

-- | Require the transaction to reference (not spend!) the given unspent
-- | transaction output.
mustReferenceOutput
  :: Warn TxConstraintsDeprecated => TransactionInput -> TxConstraints
mustReferenceOutput = singleton <<< MustReferenceOutput

-- | Lock the value with a public key address. (Base Address)
mustPayToPubKeyAddress
  :: Warn TxConstraintsDeprecated
  => PaymentPubKeyHash
  -> StakePubKeyHash
  -> Value
  -> TxConstraints
mustPayToPubKeyAddress pkh skh =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) Nothing Nothing

-- | Lock the value and datum with a public key address.
mustPayToPubKeyAddressWithDatum
  :: Warn TxConstraintsDeprecated
  => PaymentPubKeyHash
  -> StakePubKeyHash
  -> PlutusData
  -> DatumPresence
  -> Value
  -> TxConstraints
mustPayToPubKeyAddressWithDatum pkh skh datum dtp =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) (Just $ datum /\ dtp)
    Nothing

-- | Lock the value and reference script with a public key address.
mustPayToPubKeyAddressWithScriptRef
  :: Warn TxConstraintsDeprecated
  => PaymentPubKeyHash
  -> StakePubKeyHash
  -> ScriptRef
  -> Value
  -> TxConstraints
mustPayToPubKeyAddressWithScriptRef pkh skh scriptRef =
  singleton <<< MustPayToPubKeyAddress pkh (Just skh) Nothing (Just scriptRef)

-- | Lock the value, datum and reference script with a public key address.
mustPayToPubKeyAddressWithDatumAndScriptRef
  :: Warn TxConstraintsDeprecated
  => PaymentPubKeyHash
  -> StakePubKeyHash
  -> PlutusData
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
  => Warn TxConstraintsDeprecated
  => PaymentPubKeyHash
  -> Value
  -> TxConstraints
mustPayToPubKey pkh =
  singleton <<< MustPayToPubKeyAddress pkh Nothing Nothing Nothing

-- | Lock the value and datum with a payment public key hash.
mustPayToPubKeyWithDatum
  :: Warn TxConstraintsDeprecated
  => PaymentPubKeyHash
  -> PlutusData
  -> DatumPresence
  -> Value
  -> TxConstraints
mustPayToPubKeyWithDatum pkh datum dtp =
  singleton <<< MustPayToPubKeyAddress pkh Nothing (Just $ datum /\ dtp) Nothing

-- | Lock the value and reference script with a payment public key hash.
mustPayToPubKeyWithScriptRef
  :: Warn TxConstraintsDeprecated
  => PaymentPubKeyHash
  -> ScriptRef
  -> Value
  -> TxConstraints
mustPayToPubKeyWithScriptRef pkh scriptRef =
  singleton <<< MustPayToPubKeyAddress pkh Nothing Nothing (Just scriptRef)

-- | Lock the value, datum and reference script with a payment public key hash.
mustPayToPubKeyWithDatumAndScriptRef
  :: PaymentPubKeyHash
  -> PlutusData
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
  :: Warn TxConstraintsDeprecated
  => ScriptHash
  -> PlutusData
  -> DatumPresence
  -> Value
  -> TxConstraints
mustPayToScript vhash dat datp =
  singleton <<< MustPayToScript vhash Nothing dat datp Nothing

mustPayToScriptAddress
  :: Warn TxConstraintsDeprecated
  => ScriptHash
  -> Credential
  -> PlutusData
  -> DatumPresence
  -> Value
  -> TxConstraints
mustPayToScriptAddress vhash cred dat datp =
  singleton <<< MustPayToScript vhash (Just cred) dat datp Nothing

-- | Lock the value, datum and reference script with a script.
-- | Note that the provided reference script does *not* necessarily need to
-- | control the spending of the output, i.e. both scripts can be different.
mustPayToScriptWithScriptRef
  :: Warn TxConstraintsDeprecated
  => ScriptHash
  -> PlutusData
  -> DatumPresence
  -> ScriptRef
  -> Value
  -> TxConstraints
mustPayToScriptWithScriptRef vhash dat datp scriptRef =
  singleton <<< MustPayToScript vhash Nothing dat datp (Just scriptRef)

-- | Lock the value, datum and reference script with a script.
-- | Note that the provided reference script does *not* necessarily need to
-- | control the spending of the output, i.e. both scripts can be different.
mustPayToScriptAddressWithScriptRef
  :: Warn TxConstraintsDeprecated
  => ScriptHash
  -> Credential
  -> PlutusData
  -> DatumPresence
  -> ScriptRef
  -> Value
  -> TxConstraints
mustPayToScriptAddressWithScriptRef vhash cred dat datp scriptRef =
  singleton <<< MustPayToScript vhash (Just cred) dat datp (Just scriptRef)

mustPayToNativeScript
  :: Warn TxConstraintsDeprecated
  => ScriptHash
  -> Value
  -> TxConstraints
mustPayToNativeScript nsHash vl =
  singleton (MustPayToNativeScript nsHash Nothing vl)

mustPayToNativeScriptAddress
  :: Warn TxConstraintsDeprecated
  => ScriptHash
  -> Credential
  -> Value
  -> TxConstraints
mustPayToNativeScriptAddress nsHash credential vl =
  singleton (MustPayToNativeScript nsHash (Just credential) vl)

-- | Mint the given `Value`
-- | The amount to mint must not be zero.
mustMintValue :: Warn TxConstraintsDeprecated => Mint -> TxConstraints
mustMintValue = mustMintValueWithRedeemer RedeemerDatum.unit

-- | Mint the given `Value` by accessing non-Ada assets.
-- | The amount to mint must not be zero.
mustMintValueWithRedeemer
  :: Warn TxConstraintsDeprecated
  => RedeemerDatum
  -> Mint
  -> TxConstraints
mustMintValueWithRedeemer redeemer =
  Array.fold <<< map tokenConstraint <<< Mint.flatten
  where
  tokenConstraint
    :: ScriptHash /\ AssetName /\ Int.Int -> TxConstraints
  tokenConstraint (cs /\ tn /\ amount) =
    mustMintCurrencyWithRedeemer cs redeemer tn amount

-- | Create the given amount of the currency.
-- | The amount to mint must not be zero.
mustMintCurrency
  :: Warn TxConstraintsDeprecated
  => ScriptHash
  -> AssetName
  -> Int.Int
  -> TxConstraints
mustMintCurrency mph =
  mustMintCurrencyWithRedeemer mph RedeemerDatum.unit

mustMintCurrencyUsingNativeScript
  :: Warn TxConstraintsDeprecated
  => NativeScript
  -> AssetName
  -> Int.Int
  -> TxConstraints
mustMintCurrencyUsingNativeScript ns tk i = singleton
  (MustMintValueUsingNativeScript ns tk i)

-- | Create the given amount of the currency using a reference minting policy.
-- | The amount to mint must not be zero.
mustMintCurrencyUsingScriptRef
  :: Warn TxConstraintsDeprecated
  => ScriptHash
  -> AssetName
  -> Int.Int
  -> InputWithScriptRef
  -> TxConstraints
mustMintCurrencyUsingScriptRef mph =
  mustMintCurrencyWithRedeemerUsingScriptRef mph RedeemerDatum.unit

-- | Create the given amount of the currency.
-- | The amount to mint must not be zero.
mustMintCurrencyWithRedeemer
  :: Warn TxConstraintsDeprecated
  => ScriptHash
  -> RedeemerDatum
  -> AssetName
  -> Int.Int
  -> TxConstraints
mustMintCurrencyWithRedeemer mph red tn amount =
  singleton (MustMintValue mph red tn amount Nothing)

-- | Create the given amount of the currency using a reference minting policy.
-- | The amount to mint must not be zero.
mustMintCurrencyWithRedeemerUsingScriptRef
  :: Warn TxConstraintsDeprecated
  => ScriptHash
  -> RedeemerDatum
  -> AssetName
  -> Int.Int
  -> InputWithScriptRef
  -> TxConstraints
mustMintCurrencyWithRedeemerUsingScriptRef mph red tn amount =
  singleton <<< MustMintValue mph red tn amount <<< Just

-- | Requirement to spend inputs with at least the given value
mustSpendAtLeast :: Warn TxConstraintsDeprecated => Value -> TxConstraints
mustSpendAtLeast = singleton <<< MustSpendAtLeast

-- | Requirement to produce outputs with at least the given value
mustProduceAtLeast :: Warn TxConstraintsDeprecated => Value -> TxConstraints
mustProduceAtLeast = singleton <<< MustProduceAtLeast

-- | Spend the given unspent transaction public key output.
mustSpendPubKeyOutput
  :: Warn TxConstraintsDeprecated => TransactionInput -> TxConstraints
mustSpendPubKeyOutput = singleton <<< MustSpendPubKeyOutput

-- | Spend the given unspent transaction script output.
mustSpendScriptOutput
  :: Warn TxConstraintsDeprecated
  => TransactionInput
  -> RedeemerDatum
  -> TxConstraints
mustSpendScriptOutput txOutRef red =
  singleton (MustSpendScriptOutput txOutRef red Nothing)

-- | Spend the given unspent transaction script output, using a reference script
-- | to satisfy the script witnessing requirement.
mustSpendScriptOutputUsingScriptRef
  :: Warn TxConstraintsDeprecated
  => TransactionInput
  -> RedeemerDatum
  -> InputWithScriptRef
  -> TxConstraints
mustSpendScriptOutputUsingScriptRef txOutRef red =
  singleton <<< MustSpendScriptOutput txOutRef red <<< Just

mustSpendNativeScriptOutput
  :: Warn TxConstraintsDeprecated
  => TransactionInput
  -> NativeScript
  -> TxConstraints
mustSpendNativeScriptOutput txOutRef = singleton <<< MustSpendNativeScriptOutput
  txOutRef

mustHashDatum
  :: Warn TxConstraintsDeprecated => DataHash -> PlutusData -> TxConstraints
mustHashDatum dhsh = singleton <<< MustHashDatum dhsh

mustRegisterStakePubKey
  :: Warn TxConstraintsDeprecated => StakePubKeyHash -> TxConstraints
mustRegisterStakePubKey = singleton <<< MustRegisterStakePubKey

mustDeregisterStakePubKey
  :: Warn TxConstraintsDeprecated => StakePubKeyHash -> TxConstraints
mustDeregisterStakePubKey = singleton <<< MustDeregisterStakePubKey

mustRegisterStakeScript
  :: Warn TxConstraintsDeprecated => ScriptHash -> TxConstraints
mustRegisterStakeScript = singleton <<< MustRegisterStakeScript

mustDeregisterStakePlutusScript
  :: Warn TxConstraintsDeprecated
  => PlutusScript
  -> RedeemerDatum
  -> TxConstraints
mustDeregisterStakePlutusScript sv = singleton <<<
  MustDeregisterStakePlutusScript sv

mustDeregisterStakeNativeScript
  :: Warn TxConstraintsDeprecated
  => NativeScript
  -> TxConstraints
mustDeregisterStakeNativeScript = singleton <<< MustDeregisterStakeNativeScript

mustRegisterPool
  :: Warn TxConstraintsDeprecated => PoolParams -> TxConstraints
mustRegisterPool = singleton <<< MustRegisterPool

mustRetirePool
  :: Warn TxConstraintsDeprecated
  => PoolPubKeyHash
  -> Epoch
  -> TxConstraints
mustRetirePool poolPubKeyHash = singleton <<< MustRetirePool poolPubKeyHash

mustDelegateStakePubKey
  :: Warn TxConstraintsDeprecated
  => StakePubKeyHash
  -> PoolPubKeyHash
  -> TxConstraints
mustDelegateStakePubKey spkh ppkh = singleton $ MustDelegateStakePubKey spkh
  ppkh

mustDelegateStakePlutusScript
  :: Warn TxConstraintsDeprecated
  => PlutusScript
  -> RedeemerDatum
  -> PoolPubKeyHash
  -> TxConstraints
mustDelegateStakePlutusScript sv redeemer ppkh = singleton $
  MustDelegateStakePlutusScript sv redeemer ppkh

mustDelegateStakeNativeScript
  :: Warn TxConstraintsDeprecated
  => NativeScript
  -> PoolPubKeyHash
  -> TxConstraints
mustDelegateStakeNativeScript sv ppkh =
  singleton $ MustDelegateStakeNativeScript sv ppkh

mustWithdrawStakePubKey
  :: Warn TxConstraintsDeprecated => StakePubKeyHash -> TxConstraints
mustWithdrawStakePubKey spkh = singleton $ MustWithdrawStakePubKey spkh

mustWithdrawStakePlutusScript
  :: Warn TxConstraintsDeprecated
  => PlutusScript
  -> RedeemerDatum
  -> TxConstraints
mustWithdrawStakePlutusScript validator redeemer =
  singleton $ MustWithdrawStakePlutusScript validator redeemer

mustWithdrawStakeNativeScript
  :: Warn TxConstraintsDeprecated
  => NativeScript
  -> TxConstraints
mustWithdrawStakeNativeScript =
  singleton <<< MustWithdrawStakeNativeScript

-- | Attempts to solve, in order, a sequence of constraints until the first
-- | successful try.
-- | `mustSatisfyaAnyOf` is just a way to define a chain of try-catch expressions
-- | in a declarative manner. It does not do any analysis of the constraints' semantics.
mustSatisfyAnyOf
  :: forall (f :: Type -> Type)
   . Foldable f
  => Warn TxConstraintsDeprecated
  => f (TxConstraints)
  -> TxConstraints
mustSatisfyAnyOf =
  Array.fromFoldable
    >>> MustSatisfyAnyOf
    >>> singleton

-- | Marks the transaction as invalid, requiring at least one script execution
-- | to fail. Despite failure, the transaction can still be submitted into the
-- | chain and collateral will be lost.
mustNotBeValid :: Warn TxConstraintsDeprecated => TxConstraints
mustNotBeValid = singleton $ MustNotBeValid

mustRegisterDrep
  :: Warn TxConstraintsDeprecated
  => Credential
  -> Maybe Anchor
  -> TxConstraints
mustRegisterDrep drepCred = singleton <<< MustRegisterDrep drepCred
