module Contract.Test.Utils where

import Prelude

import Contract.Address (Address)
import Contract.Monad (Contract, throwContractError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Contract.Transaction (DataHash, TransactionOutput)
import Contract.Value (Coin)
import Data.Foldable (class Foldable, foldl, null)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested (type (/\), (/\))

type ContractTestM (r :: Row Type) (a :: Type) =
  WriterT (Array ContractAssertionFailure) (Contract r) a

type Label = String

data ContractAssertionFailure
  = OutputHasNoDatumHash Label DataHash
  | UnexpectedLovelaceDelta Address (Expected Coin) (Actual Coin)

instance Show ContractAssertionFailure where
  show (OutputHasNoDatumHash txOutputLabel dataHash) =
    txOutputLabel <> " output does not have datum hash " <> show dataHash

  show (UnexpectedLovelaceDelta addr expected actual) =
    "Unexpected lovelace delta at address "
      <> (show addr <> show (expected /\ actual))

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
  ExpectedActual (Expected a /\ Expected b)

instance (Show a, Show b) => Show (ExpectedActual a b) where
  show (ExpectedActual (expected /\ actual)) =
    "(" <> show expected <> ", " <> show actual <> ")"

class ContractAssertions (f :: Type) (r :: Row Type) (a :: Type) where
  wrapAndAssert :: Contract r a -> f -> ContractTestM r a

type ContractBasicAssertion (r :: Row Type) (a :: Type) (b :: Type) =
  a -> ContractTestM r b

type ContractWrapAssertion (r :: Row Type) (a :: Type) =
  Contract r a -> ContractTestM r a

instance ContractAssertions (ContractWrapAssertion r a) r a where
  wrapAndAssert contract assertion = assertion contract
else instance ContractAssertions (ContractBasicAssertion r a b) r a where
  wrapAndAssert contract assertion =
    lift contract >>= \r -> assertion r *> pure r

withAssertions
  :: forall (r :: Row Type) (a :: Type) (assertions :: Type)
   . ContractAssertions assertions r a
  => assertions
  -> Contract r a
  -> Contract r a
withAssertions assertions contract = do
  result /\ failures <- runWriterT (wrapAndAssert contract assertions)
  if null failures then pure result else throwContractError failures

