module Contract.Test.Utils where

import Prelude

import Contract.Address (Address)
import Contract.Monad (Contract, throwContractError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT, except)
import Contract.Transaction (DataHash, TransactionOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (Value, valueToCoin')
import Control.Bind (bindFlipped)
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (class Foldable, foldl, foldMap, null)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested (type (/\), (/\))

type ContractTestM (r :: Row Type) (a :: Type) =
  ExceptT (Array ContractAssertionFailure) (Contract r) a

type Label = String

data ContractAssertionFailure
  = CouldNotGetUtxosAtAddress Label
  | OutputHasNoDatumHash Label DataHash
  | UnexpectedLovelaceDelta Label (Expected BigInt) (Actual BigInt)

instance Show ContractAssertionFailure where
  show (CouldNotGetUtxosAtAddress addrLabel) =
    "Could not get utxos at " <> addrLabel

  show (OutputHasNoDatumHash txOutputLabel dataHash) =
    txOutputLabel <> " output does not have datum hash " <> show dataHash

  show (UnexpectedLovelaceDelta addrLabel expected actual) =
    "Unexpected lovelace delta at address "
      <> (addrLabel <> show (ExpectedActual $ expected /\ actual))

newtype ContractAssertionFailures (f :: Type -> Type) =
  ContractAssertionFailures (f ContractAssertionFailure)

derive instance Newtype (ContractAssertionFailures f) _

instance Foldable f => Show (ContractAssertionFailures f) where
  show = foldl (\x y -> show x <> "\n" <> show y) mempty <<< unwrap

newtype Expected (a :: Type) = Expected a

derive instance Newtype (Expected a) _

instance Show a => Show (Expected a) where
  show = append "Expected: " <<< show <<< unwrap

newtype Actual (a :: Type) = Actual a

derive instance Newtype (Actual a) _

instance Show a => Show (Actual a) where
  show = append "Actual: " <<< show <<< unwrap

newtype ExpectedActual (a :: Type) (b :: Type) =
  ExpectedActual (Expected a /\ Actual b)

instance (Show a, Show b) => Show (ExpectedActual a b) where
  show (ExpectedActual (expected /\ actual)) =
    " (" <> show expected <> ", " <> show actual <> ")"

class ContractAssertions (f :: Type) (r :: Row Type) (a :: Type) where
  wrapAndAssert :: Contract r a -> f -> ContractTestM r Unit

type ContractBasicAssertion (r :: Row Type) (a :: Type) (b :: Type) =
  a -> ContractTestM r Unit

type ContractWrapAssertion (r :: Row Type) (a :: Type) =
  Contract r a -> ContractTestM r Unit

instance ContractAssertions (ContractWrapAssertion r a) r a where
  wrapAndAssert contract assertion = assertion contract
else instance ContractAssertions (ContractBasicAssertion r a b) r a where
  wrapAndAssert contract assertion = lift contract >>= assertion

combineAssertionResults
  :: forall (r :: Row Type)
   . ContractTestM r Unit
  -> ContractTestM r Unit
  -> ContractTestM r Unit
combineAssertionResults x y =
  ExceptT $ runExceptT x >>= \xResult -> runExceptT y >>= \yResult ->
    case xResult /\ yResult of
      Left xFailure /\ Left yFailure -> pure (Left $ xFailure <> yFailure)
      failure@(Left _) /\ _ -> pure failure
      _ /\ failure@(Left _) -> pure failure
      Right _ /\ Right _ -> pure (Right unit)

instance
  Foldable f =>
  ContractAssertions (f (ContractWrapAssertion r a)) r a where
  wrapAndAssert contract =
    foldl (\x y -> combineAssertionResults x (y contract)) (pure unit)
else instance
  Foldable f =>
  ContractAssertions (f (ContractBasicAssertion r a b)) r a where
  wrapAndAssert contract assertions = do
    result <- lift contract
    foldl (\x y -> combineAssertionResults x (y result)) (pure unit) assertions

withAssertions
  :: forall (r :: Row Type) (a :: Type) (assertions :: Type)
   . ContractAssertions assertions r a
  => assertions
  -> Contract r a
  -> Contract r Unit
withAssertions assertions contract =
  runExceptT (wrapAndAssert contract assertions)
    >>= either (throwContractError <<< ContractAssertionFailures) pure

assertionFailure
  :: forall (r :: Row Type) (a :: Type)
   . ContractAssertionFailure
  -> ContractTestM r a
assertionFailure = except <<< Left <<< Array.singleton

assertContract
  :: forall (r :: Row Type)
   . ContractAssertionFailure
  -> Boolean
  -> ContractTestM r Unit
assertContract failure cond =
  if cond then pure unit else assertionFailure failure

assertContractM
  :: forall (r :: Row Type) (a :: Type)
   . ContractAssertionFailure
  -> ContractTestM r (Maybe a)
  -> ContractTestM r a
assertContractM failure =
  bindFlipped (maybe (assertionFailure failure) pure)

--------------------------------------------------------------------------------

valueAtAddress
  :: forall (r :: Row Type). Label -> Address -> ContractTestM r Value
valueAtAddress addrLabel addr =
  assertContractM (CouldNotGetUtxosAtAddress addrLabel) (lift $ utxosAt addr)
    <#> foldMap (_.amount <<< unwrap)

checkBalanceDeltaAtAddress
  :: forall (r :: Row Type) (a :: Type) (b :: Type)
   . Label
  -> Address
  -> (a -> Value -> Value -> ContractTestM r b)
  -> Contract r a
  -> ContractTestM r b
checkBalanceDeltaAtAddress addrLabel addr check contract = do
  valueBefore <- valueAtAddress addrLabel addr
  res <- lift contract
  valueAfter <- valueAtAddress addrLabel addr
  check res valueBefore valueAfter

assertLovelaceDeltaAtAddress
  :: forall (r :: Row Type) (a :: Type)
   . Label
  -> Address
  -> (a -> Contract r BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> ContractWrapAssertion r a
assertLovelaceDeltaAtAddress addrLabel addr getExpected comp =
  checkBalanceDeltaAtAddress addrLabel addr
    \result valueBefore valueAfter -> do
      expected <- lift $ getExpected result
      let
        actual :: BigInt
        actual = valueToCoin' valueAfter - valueToCoin' valueBefore

        assertionFailure :: ContractAssertionFailure
        assertionFailure =
          UnexpectedLovelaceDelta addrLabel (Expected expected) (Actual actual)

      assertContract assertionFailure (comp actual expected)
