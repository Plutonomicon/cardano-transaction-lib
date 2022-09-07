module Contract.Test.Utils where

import Prelude

import Contract.Address (Address)
import Contract.Monad (Contract, liftedM, throwContractError)
import Contract.Transaction (DataHash, TransactionOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (Value, valueToCoin')
import Data.BigInt (BigInt)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid.Endo (Endo(Endo))
import Data.Newtype (class Newtype, ala, unwrap)
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))

data ContractAssertionFailure
  = CouldNotGetUtxosAtAddress (Labeled Address)
  | OutputHasNoDatumHash (Labeled TransactionOutput) DataHash 
  | UnexpectedLovelaceDelta (Labeled Address) (Expected BigInt) (Actual BigInt)

instance Show ContractAssertionFailure where
  show (CouldNotGetUtxosAtAddress addr) =
    "Could not get utxos at " <> show addr

  show (OutputHasNoDatumHash txOutput dataHash) =
    show txOutput <> " output does not have datum hash " <> show dataHash

  show (UnexpectedLovelaceDelta addr expected actual) =
    "Unexpected lovelace delta at address "
      <> (show addr <> show (ExpectedActual $ expected /\ actual))

data Labeled (a :: Type) = Labeled a (Maybe String)

label :: forall (a :: Type). a -> String -> Labeled a 
label x l = Labeled x (Just l)

unlabel :: forall (a :: Type). Labeled a -> a 
unlabel (Labeled x _) = x 

instance Show a => Show (Labeled a) where 
  show (Labeled _ (Just l)) = l
  show (Labeled x Nothing) = show x 

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

assertContractM 
  :: forall (r :: Row Type) (e :: Type) (a :: Type) 
   . Show e 
  => e 
  -> Contract r (Maybe a)
  -> Contract r a
assertContractM msg = liftedM (show msg) 

-- | `wrapAndAssert` flipped
withAssertions 
  :: forall (r :: Row Type) (a :: Type) (assertions :: Type)
   . ContractAssertions assertions r a 
  => assertions 
  -> Contract r a 
  -> Contract r a
withAssertions = flip wrapAndAssert

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
          UnexpectedLovelaceDelta addr (Expected expected) (Actual actual)

      assertContract assertionFailure (comp actual expected)
      pure result
