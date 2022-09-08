module Contract.Test.Utils
  ( class ContractAssertions
  , ContractAssertionFailure
      ( CouldNotGetTxByHash
      , CouldNotGetUtxosAtAddress
      , CouldNotHashDatum
      , CouldNotParseMetadata
      , OutputHasNoDatumHash
      , TransactionHasNoMetadata
      , UnexpectedDatumHashInOutput
      , UnexpectedLovelaceDelta
      , UnexpectedMetadataValue
      )
  , ContractBasicAssertion
  , ContractWrapAssertion
  , ExpectedActual(ExpectedActual)
  , Label
  , Labeled(Labeled)
  , assertContract
  , assertContractExpectedActual
  , assertContractM
  , assertContractM'
  , assertGainAtAddress
  , assertGainAtAddress'
  , assertLossAtAddress
  , assertLossAtAddress'
  , assertLovelaceDeltaAtAddress
  , assertOutputHasDatum
  , assertOutputHasDatumHash
  , assertTxHasMetadata
  , checkBalanceDeltaAtAddress
  , checkOutputHasDatumHash
  , label
  , unlabel
  , valueAtAddress
  , withAssertions
  , wrapAndAssert
  ) where

import Prelude

import Contract.Address (Address)
import Contract.Hashing (datumHash)
import Contract.Monad (Contract, liftedM, liftContractM, throwContractError)
import Contract.PlutusData (Datum)
import Contract.Transaction
  ( DataHash
  , Transaction(Transaction)
  , TransactionHash
  , TransactionOutput
  , getTxByHash
  )
import Contract.Utxos (utxosAt)
import Contract.Value (Value, valueToCoin')
import Control.Monad.Error.Class (catchError)
import Data.BigInt (BigInt)
import Data.Foldable (foldMap)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid.Endo (Endo(Endo))
import Data.Newtype (ala, unwrap)
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Metadata.FromMetadata (fromMetadata)
import Metadata.MetadataType (class MetadataType, metadataLabel)
import Type.Proxy (Proxy(Proxy))

data ContractAssertionFailure
  = CouldNotGetTxByHash TransactionHash
  | CouldNotGetUtxosAtAddress (Labeled Address)
  | CouldNotHashDatum Datum
  | CouldNotParseMetadata Label
  | OutputHasNoDatumHash (Labeled TransactionOutput) DataHash
  | TransactionHasNoMetadata TransactionHash (Maybe Label)
  | UnexpectedDatumHashInOutput (Labeled TransactionOutput)
      (ExpectedActual DataHash)
  | UnexpectedLovelaceDelta (Labeled Address) (ExpectedActual BigInt)
  | UnexpectedMetadataValue Label (ExpectedActual String)

instance Show ContractAssertionFailure where
  show (CouldNotGetTxByHash txHash) =
    "Could not get tx by hash " <> show txHash

  show (CouldNotGetUtxosAtAddress addr) =
    "Could not get utxos at " <> show addr

  show (CouldNotHashDatum datum) =
    "Could not hash datum " <> show datum

  show (CouldNotParseMetadata mdLabel) =
    "Could not parse " <> show mdLabel <> " metadata"

  show (OutputHasNoDatumHash txOutput dataHash) =
    show txOutput <> " output does not have datum hash " <> show dataHash

  show (TransactionHasNoMetadata txHash mdLabel) =
    "Tx with " <> show txHash <> " does not hold "
      <> (maybe mempty (flip append " ") (show <$> mdLabel) <> "metadata")

  show (UnexpectedDatumHashInOutput txOutput expectedActual) =
    "Unexpected datum hash value in output "
      <> (show txOutput <> show expectedActual)

  show (UnexpectedLovelaceDelta addr expectedActual) =
    "Unexpected lovelace delta at address "
      <> (show addr <> show expectedActual)

  show (UnexpectedMetadataValue mdLabel expectedActual) =
    "Unexpected " <> show mdLabel <> " metadata value" <> show expectedActual

type Label = String

data Labeled (a :: Type) = Labeled a (Maybe Label)

label :: forall (a :: Type). a -> Label -> Labeled a
label x l = Labeled x (Just l)

unlabel :: forall (a :: Type). Labeled a -> a
unlabel (Labeled x _) = x

instance Show a => Show (Labeled a) where
  show (Labeled _ (Just l)) = l
  show (Labeled x Nothing) = show x

data ExpectedActual (a :: Type) = ExpectedActual a a

derive instance Functor ExpectedActual

instance Show a => Show (ExpectedActual a) where
  show (ExpectedActual expected actual) =
    " (Expected: " <> show expected <> ", Actual: " <> show actual <> ")"

--------------------------------------------------------------------------------
-- Different types of assertions, Assertion composition, Basic functions
--------------------------------------------------------------------------------

-- | An assertion that only needs the result of the contract.
type ContractBasicAssertion (r :: Row Type) (a :: Type) (b :: Type) =
  a -> Contract r b

-- | An assertion that can control when the contract is run. The
-- | assertion inhabiting this type should not call the contract more
-- | than once, as other assertions need to be able to make this
-- | assumption to succesfully compose.
type ContractWrapAssertion (r :: Row Type) (a :: Type) =
  Contract r a -> Contract r a

-- | Class to unify different methods of making assertions about a
-- | contract under a single interface. Note that the typechecker may
-- | need some help when using this class; try providing type
-- | annotations for your assertions using the type aliases for the
-- | instances of this class.
class ContractAssertions (f :: Type) (r :: Row Type) (a :: Type) where
  -- | Wrap a contract in an assertion. The wrapped contract itself
  -- | becomes a contract which can be wrapped, allowing for
  -- | compositionala of assertions.
  -- |
  -- | No guarantees are made about the order in which assertions are
  -- | made. Assertions with side effects should not be used.
  wrapAndAssert :: Contract r a -> f -> Contract r a

instance ContractAssertions (ContractWrapAssertion r a) r a where
  wrapAndAssert contract assertion = assertion contract
else instance ContractAssertions (ContractBasicAssertion r a b) r a where
  wrapAndAssert contract assertion = contract >>= \r -> assertion r *> pure r

instance ContractAssertions (Array (ContractWrapAssertion r a)) r a where
  wrapAndAssert contract assertions = ala Endo foldMap assertions contract
else instance
  ContractAssertions (Array (ContractBasicAssertion r a b)) r a where
  wrapAndAssert contract assertions =
    contract >>= \r -> traverse_ (_ $ r) assertions *> pure r

instance
  ( ContractAssertions f r a
  , ContractAssertions g r a
  ) =>
  ContractAssertions (f /\ g) r a where
  wrapAndAssert contract (assertionsX /\ assertionsY) =
    wrapAndAssert (wrapAndAssert contract assertionsX) assertionsY

assertContract
  :: forall (r :: Row Type) (e :: Type)
   . Show e
  => e
  -> Boolean
  -> Contract r Unit
assertContract msg cond =
  if cond then pure unit else throwContractError msg

assertContractExpectedActual
  :: forall (r :: Row Type) (e :: Type) (a :: Type)
   . Show e
  => Eq a
  => (ExpectedActual a -> e)
  -> a
  -> a
  -> Contract r Unit
assertContractExpectedActual mkAssertionFailure expected actual =
  assertContract (mkAssertionFailure $ ExpectedActual expected actual)
    (expected == actual)

assertContractM
  :: forall (r :: Row Type) (e :: Type) (a :: Type)
   . Show e
  => e
  -> Contract r (Maybe a)
  -> Contract r a
assertContractM msg = liftedM (show msg)

assertContractM'
  :: forall (r :: Row Type) (e :: Type) (a :: Type)
   . Show e
  => e
  -> Maybe a
  -> Contract r a
assertContractM' msg = liftContractM (show msg)

-- | `wrapAndAssert` flipped
withAssertions
  :: forall (r :: Row Type) (a :: Type) (assertions :: Type)
   . ContractAssertions assertions r a
  => assertions
  -> Contract r a
  -> Contract r a
withAssertions = flip wrapAndAssert

--------------------------------------------------------------------------------
-- Assertions and checks
--------------------------------------------------------------------------------

valueAtAddress
  :: forall (r :: Row Type). Labeled Address -> Contract r Value
valueAtAddress addr =
  assertContractM (CouldNotGetUtxosAtAddress addr) (utxosAt $ unlabel addr)
    <#> foldMap (_.amount <<< unwrap)

checkBalanceDeltaAtAddress
  :: forall (r :: Row Type) (a :: Type) (b :: Type)
   . Labeled Address
  -> (a -> Value -> Value -> Contract r b)
  -> Contract r a
  -> Contract r b
checkBalanceDeltaAtAddress addr check contract = do
  valueBefore <- valueAtAddress addr
  res <- contract
  valueAfter <- valueAtAddress addr
  check res valueBefore valueAfter

assertLovelaceDeltaAtAddress
  :: forall (r :: Row Type) (a :: Type)
   . Labeled Address
  -> (a -> Contract r BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> ContractWrapAssertion r a
assertLovelaceDeltaAtAddress addr getExpected comp =
  checkBalanceDeltaAtAddress addr
    \result valueBefore valueAfter -> do
      expected <- getExpected result
      let
        actual :: BigInt
        actual = valueToCoin' valueAfter - valueToCoin' valueBefore

        assertionFailure :: ContractAssertionFailure
        assertionFailure =
          UnexpectedLovelaceDelta addr (ExpectedActual expected actual)

      assertContract assertionFailure (comp actual expected)
      pure result

-- | Requires that the computed amount of lovelace was gained at the address 
-- | by calling the contract.
assertGainAtAddress
  :: forall (r :: Row Type) (a :: Type)
   . Labeled Address
  -> (a -> Contract r BigInt)
  -> ContractWrapAssertion r a
assertGainAtAddress addr getMinGain =
  assertLovelaceDeltaAtAddress addr getMinGain eq

-- | Requires that the passed amount of lovelace was gained at the address 
-- | by calling the contract.
assertGainAtAddress'
  :: forall (r :: Row Type) (a :: Type)
   . Labeled Address
  -> BigInt
  -> ContractWrapAssertion r a
assertGainAtAddress' addr minGain =
  assertGainAtAddress addr (const $ pure minGain)

-- | Requires that the computed amount of lovelace was lost at the address 
-- | by calling the contract.
assertLossAtAddress
  :: forall (r :: Row Type) (a :: Type)
   . Labeled Address
  -> (a -> Contract r BigInt)
  -> ContractWrapAssertion r a
assertLossAtAddress addr getMinLoss =
  assertLovelaceDeltaAtAddress addr (map negate <<< getMinLoss) eq

-- | Requires that the passed amount of lovelace was lost at the address 
-- | by calling the contract.
assertLossAtAddress'
  :: forall (r :: Row Type) (a :: Type)
   . Labeled Address
  -> BigInt
  -> ContractWrapAssertion r a
assertLossAtAddress' addr minLoss =
  assertLossAtAddress addr (const $ pure minLoss)

assertOutputHasDatumHash
  :: forall (r :: Row Type)
   . DataHash
  -> Labeled TransactionOutput
  -> Contract r Unit
assertOutputHasDatumHash expectedDatumHash txOutput = do
  let mDatumHash = _.dataHash $ unwrap (unlabel txOutput)
  datumHash <-
    assertContractM' (OutputHasNoDatumHash txOutput expectedDatumHash)
      mDatumHash
  assertContractExpectedActual (UnexpectedDatumHashInOutput txOutput)
    expectedDatumHash
    datumHash

assertOutputHasDatum
  :: forall (r :: Row Type)
   . Datum
  -> Labeled TransactionOutput
  -> Contract r Unit
assertOutputHasDatum expectedDatum txOutput = do
  expectedDatumHash <-
    assertContractM' (CouldNotHashDatum expectedDatum)
      (datumHash expectedDatum)
  assertOutputHasDatumHash expectedDatumHash txOutput

checkOutputHasDatumHash
  :: forall (r :: Row Type)
   . DataHash
  -> Labeled TransactionOutput
  -> Contract r Boolean
checkOutputHasDatumHash expected txOutput =
  (assertOutputHasDatumHash expected txOutput *> pure true)
    `catchError` (const $ pure false)

assertTxHasMetadata
  :: forall (r :: Row Type) (a :: Type)
   . MetadataType a
  => Eq a
  => Show a
  => Label
  -> TransactionHash
  -> a
  -> Contract r Unit
assertTxHasMetadata mdLabel txHash expectedMetadata = do
  Transaction { auxiliaryData } <-
    assertContractM (CouldNotGetTxByHash txHash) (getTxByHash txHash)

  generalMetadata <-
    assertContractM' (TransactionHasNoMetadata txHash Nothing)
      (map unwrap <<< _.metadata <<< unwrap =<< auxiliaryData)

  rawMetadata <-
    assertContractM' (TransactionHasNoMetadata txHash (Just mdLabel))
      (Map.lookup (metadataLabel (Proxy :: Proxy a)) generalMetadata)

  (metadata :: a) <-
    assertContractM' (CouldNotParseMetadata mdLabel)
      (fromMetadata rawMetadata)

  let expectedActual = show <$> ExpectedActual expectedMetadata metadata
  assertContract (UnexpectedMetadataValue mdLabel expectedActual)
    (metadata == expectedMetadata)

