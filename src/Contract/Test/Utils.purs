-- | This module provides an extensible interface for making various
-- | assertions about `Contract`s.
module Contract.Test.Utils
  ( class ContractAssertions
  , ContractAssertionFailure
      ( CouldNotGetTxByHash
      , CouldNotParseMetadata
      , CustomFailure
      , TransactionHasNoMetadata
      , UnexpectedDatumInOutput
      , UnexpectedLovelaceDelta
      , UnexpectedMetadataValue
      , UnexpectedRefScriptInOutput
      , UnexpectedTokenDelta
      )
  , ContractBasicAssertion
  , ContractTestM
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
  , assertOutputHasRefScript
  , assertTokenDeltaAtAddress
  , assertTokenGainAtAddress
  , assertTokenGainAtAddress'
  , assertTokenLossAtAddress
  , assertTokenLossAtAddress'
  , assertTxHasMetadata
  , checkBalanceDeltaAtAddress
  , checkNewUtxosAtAddress
  , checkOutputHasDatum
  , checkOutputHasRefScript
  , checkTxHasMetadata
  , label
  , exitCode
  , interruptOnSignal
  , runContractAssertionM
  , runContractAssertionM'
  , unlabel
  , utxosAtAddress
  , valueAtAddress
  , withAssertions
  , wrapAndAssert
  ) where

import Prelude

import Contract.Address (Address)
import Contract.Monad (Contract, launchAff_, throwContractError)
import Contract.PlutusData (OutputDatum)
import Contract.Prelude (Effect)
import Contract.Transaction
  ( ScriptRef
  , TransactionHash
  , TransactionOutputWithRefScript
  , getTxMetadata
  )
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, Value, valueOf, valueToCoin')
import Control.Monad.Except.Trans (ExceptT, except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Ctl.Internal.Metadata.FromMetadata (fromMetadata)
import Ctl.Internal.Metadata.MetadataType (class MetadataType, metadataLabel)
import Ctl.Internal.Plutus.Types.Transaction
  ( UtxoMap
  , _amount
  , _datum
  , _output
  , _scriptRef
  )
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Data.Array (fromFoldable, mapWithIndex) as Array
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), isRight)
import Data.Foldable (foldMap, null)
import Data.Lens.Getter (view, (^.))
import Data.Map (filterKeys, lookup, values) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid.Endo (Endo(Endo))
import Data.Newtype (class Newtype, ala, unwrap)
import Data.Posix.Signal (Signal)
import Data.Posix.Signal as Signal
import Data.Semigroup.Last (Last(Last))
import Data.String.Common (joinWith) as String
import Data.Traversable (traverse_)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Fiber, killFiber)
import Effect.Exception (error)
import Node.Process as Process
import Type.Proxy (Proxy(Proxy))

--------------------------------------------------------------------------------
-- `ContractTestM` and `ContractAssertionM` monads with related functions
--------------------------------------------------------------------------------

-- | Monad allowing for accumulation of assertion failures. Should be used in
-- | conjunction with `ContractAssertionM`.
type ContractTestM (a :: Type) =
  WriterT (Array ContractAssertionFailure) Contract a

-- | Represents computations which may fail with `ContractAssertionFailure`,
-- | with the capability of storing some intermediate result, usually the result
-- | of the contract under test.
-- |
-- | Particularly useful for assertions that can control when the contract is
-- | run (`ContractWrapAssertion`s). So in case of a failure after the contract
-- | has already been executed, we can return the result of the contract, thus
-- | preventing the failure of subsequent assertions.
type ContractAssertionM (w :: Type) (a :: Type) =
  -- ExceptT ContractAssertionFailure
  --   (Writer (Maybe (Last w)) (ContractTestM)) a
  ExceptT ContractAssertionFailure
    ( WriterT (Maybe (Last w))
        (WriterT (Array ContractAssertionFailure) (Contract))
    )
    a

runContractAssertionM
  :: forall (a :: Type)
   . ContractTestM a
  -> ContractAssertionM a a
  -> ContractTestM a
runContractAssertionM contract wrappedContract =
  runWriterT (runExceptT wrappedContract) >>= case _ of
    Right result /\ _ ->
      pure result
    Left failure /\ result ->
      tell [ failure ] *> maybe contract (pure <<< unwrap) result

runContractAssertionM'
  :: ContractAssertionM Unit Unit
  -> ContractTestM Unit
runContractAssertionM' = runContractAssertionM (pure unit)

liftContractTestM
  :: forall (w :: Type) (a :: Type)
   . ContractTestM a
  -> ContractAssertionM w a
liftContractTestM = lift <<< lift

--------------------------------------------------------------------------------
-- Data types and functions for building assertion failures
--------------------------------------------------------------------------------

data ContractAssertionFailure
  = CouldNotGetTxByHash TransactionHash
  | CouldNotParseMetadata Label
  | TransactionHasNoMetadata TransactionHash (Maybe Label)
  | UnexpectedDatumInOutput (Labeled TransactionOutputWithRefScript)
      (ExpectedActual OutputDatum)
  | UnexpectedLovelaceDelta (Labeled Address) (ExpectedActual BigInt)
  | UnexpectedMetadataValue Label (ExpectedActual String)
  | UnexpectedRefScriptInOutput (Labeled TransactionOutputWithRefScript)
      (ExpectedActual (Maybe ScriptRef))
  | UnexpectedTokenDelta (Labeled Address) TokenName (ExpectedActual BigInt)
  | CustomFailure String

newtype ContractAssertionFailures =
  ContractAssertionFailures (Array ContractAssertionFailure)

derive instance Newtype (ContractAssertionFailures) _

instance Show ContractAssertionFailures where
  show =
    append "The following `Contract` assertions failed: \n    "
      <<< String.joinWith "\n\n    "
      <<< Array.mapWithIndex (\ix elem -> show (ix + one) <> ". " <> show elem)
      <<< unwrap

instance Show ContractAssertionFailure where
  show (CouldNotGetTxByHash txHash) =
    "Could not get tx by hash " <> showTxHash txHash

  show (CouldNotParseMetadata mdLabel) =
    "Could not parse " <> show mdLabel <> " metadata"

  show (TransactionHasNoMetadata txHash mdLabel) =
    "Tx with id " <> showTxHash txHash <> " does not hold "
      <> (maybe mempty (flip append " ") (show <$> mdLabel) <> "metadata")

  show (UnexpectedDatumInOutput txOutput expectedActual) =
    "Unexpected datum in output " <> show txOutput <> show expectedActual

  show (UnexpectedLovelaceDelta addr expectedActual) =
    "Unexpected lovelace delta at address "
      <> (show addr <> show expectedActual)

  show (UnexpectedMetadataValue mdLabel expectedActual) =
    "Unexpected " <> show mdLabel <> " metadata value" <> show expectedActual

  show (UnexpectedRefScriptInOutput txOutput expectedActual) =
    "Unexpected reference script in output "
      <> (show txOutput <> show expectedActual)

  show (UnexpectedTokenDelta addr tn expectedActual) =
    "Unexpected token delta " <> show tn <> " at address "
      <> (show addr <> show expectedActual)

  show (CustomFailure msg) = msg

showTxHash :: TransactionHash -> String
showTxHash = byteArrayToHex <<< unwrap

type Label = String

data Labeled (a :: Type) = Labeled a (Maybe Label)

label :: forall (a :: Type). a -> Label -> Labeled a
label x l = Labeled x (Just l)

unlabel :: forall (a :: Type). Labeled a -> a
unlabel (Labeled x _) = x

noLabel :: forall (a :: Type). a -> Labeled a
noLabel = flip Labeled Nothing

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
type ContractBasicAssertion (a :: Type) (b :: Type) =
  a -> ContractTestM b

-- | An assertion that can control when the contract is run. The assertion
-- | inhabiting this type should not call the contract more than once, as other
-- | assertions need to be able to make this assumption to succesfully compose.
type ContractWrapAssertion (a :: Type) =
  ContractTestM a -> ContractTestM a

-- | Class to unify different methods of making assertions about a contract
-- | under a single interface. Note that the typechecker may need some help when
-- | using this class; try providing type annotations for your assertions using
-- | the type aliases for the instances of this class.
class ContractAssertions (f :: Type) (a :: Type) where
  -- | Wrap a contract in an assertion. The wrapped contract itself becomes a
  -- | contract which can be wrapped, allowing for composition of assertions.
  -- |
  -- | No guarantees are made about the order in which assertions are made.
  -- | Assertions with side effects should not be used.
  wrapAndAssert :: ContractTestM a -> f -> ContractTestM a

instance ContractAssertions (ContractWrapAssertion a) a where
  wrapAndAssert contract assertion = assertion contract
else instance ContractAssertions (ContractBasicAssertion a b) a where
  wrapAndAssert contract assertion = contract >>= \r -> assertion r *> pure r

instance ContractAssertions (Array (ContractWrapAssertion a)) a where
  wrapAndAssert contract assertions = ala Endo foldMap assertions contract
else instance
  ContractAssertions (Array (ContractBasicAssertion a b)) a where
  wrapAndAssert contract assertions =
    contract >>= \r -> traverse_ (_ $ r) assertions *> pure r

instance
  ( ContractAssertions f a
  , ContractAssertions g a
  ) =>
  ContractAssertions (f /\ g) a where
  wrapAndAssert contract (assertionsX /\ assertionsY) =
    wrapAndAssert (wrapAndAssert contract assertionsX) assertionsY

assertContract
  :: forall (w :: Type)
   . ContractAssertionFailure
  -> Boolean
  -> ContractAssertionM w Unit
assertContract failure cond
  | cond = pure unit
  | otherwise = except (Left failure)

assertContractM'
  :: forall (w :: Type) (a :: Type)
   . ContractAssertionFailure
  -> Maybe a
  -> ContractAssertionM w a
assertContractM' msg = maybe (except $ Left msg) pure

assertContractM
  :: forall (w :: Type) (a :: Type)
   . ContractAssertionFailure
  -> Contract (Maybe a)
  -> ContractAssertionM w a
assertContractM msg cm =
  liftContractTestM (lift cm) >>= assertContractM' msg

assertContractExpectedActual
  :: forall (w :: Type) (a :: Type)
   . Eq a
  => (ExpectedActual a -> ContractAssertionFailure)
  -> a
  -> a
  -> ContractAssertionM w Unit
assertContractExpectedActual mkAssertionFailure expected actual =
  assertContract (mkAssertionFailure $ ExpectedActual expected actual)
    (expected == actual)

withAssertions
  :: forall (a :: Type) (assertions :: Type)
   . ContractAssertions assertions a
  => assertions
  -> Contract a
  -> Contract a
withAssertions assertions contract = do
  result /\ failures <-
    runWriterT $ wrapAndAssert (lift contract) assertions
  if null failures then pure result
  else throwContractError (ContractAssertionFailures failures)

mkCheckFromAssertion
  :: forall (w :: Type) (a :: Type)
   . ContractAssertionM w a
  -> Contract Boolean
mkCheckFromAssertion =
  map (fst <<< fst) <<< runWriterT <<< runWriterT <<< map isRight <<< runExceptT

--------------------------------------------------------------------------------
-- Assertions and checks
--------------------------------------------------------------------------------

utxosAtAddress
  :: forall (w :: Type)
   . Labeled Address
  -> ContractAssertionM w UtxoMap
utxosAtAddress = liftContractTestM <<< lift <<< utxosAt <<< unlabel

valueAtAddress
  :: forall (w :: Type)
   . Labeled Address
  -> ContractAssertionM w Value
valueAtAddress =
  map (foldMap (view (_output <<< _amount))) <<< utxosAtAddress

checkBalanceDeltaAtAddress
  :: forall (w :: Type) (a :: Type)
   . Labeled Address
  -> ContractTestM w
  -> (w -> Value -> Value -> ContractAssertionM w a)
  -> ContractAssertionM w a
checkBalanceDeltaAtAddress addr contract check = do
  valueBefore <- valueAtAddress addr
  res <- liftContractTestM contract
  tell (Just $ Last res)
  valueAfter <- valueAtAddress addr
  check res valueBefore valueAfter

checkNewUtxosAtAddress
  :: forall (w :: Type) (a :: Type)
   . Labeled Address
  -> TransactionHash
  -> (Array TransactionOutputWithRefScript -> ContractAssertionM w a)
  -> ContractAssertionM w a
checkNewUtxosAtAddress addr txHash check =
  utxosAtAddress addr >>= \utxos ->
    check $ Array.fromFoldable $ Map.values $
      Map.filterKeys (\oref -> (unwrap oref).transactionId == txHash) utxos

assertLovelaceDeltaAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (a -> Contract BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> ContractWrapAssertion a
assertLovelaceDeltaAtAddress addr getExpected comp contract =
  runContractAssertionM contract $
    checkBalanceDeltaAtAddress addr contract
      \result valueBefore valueAfter -> do
        expected <- liftContractTestM $ lift $ getExpected result
        let
          actual :: BigInt
          actual = valueToCoin' valueAfter - valueToCoin' valueBefore

          unexpectedLovelaceDelta :: ContractAssertionFailure
          unexpectedLovelaceDelta =
            UnexpectedLovelaceDelta addr (ExpectedActual expected actual)

        assertContract unexpectedLovelaceDelta (comp actual expected)
        pure result

-- | Requires that the computed amount of lovelace was gained at the address
-- | by calling the contract.
assertGainAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (a -> Contract BigInt)
  -> ContractWrapAssertion a
assertGainAtAddress addr getMinGain =
  assertLovelaceDeltaAtAddress addr getMinGain eq

-- | Requires that the passed amount of lovelace was gained at the address
-- | by calling the contract.
assertGainAtAddress'
  :: forall (a :: Type)
   . Labeled Address
  -> BigInt
  -> ContractWrapAssertion a
assertGainAtAddress' addr minGain =
  assertGainAtAddress addr (const $ pure minGain)

-- | Requires that the computed amount of lovelace was lost at the address
-- | by calling the contract.
assertLossAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (a -> Contract BigInt)
  -> ContractWrapAssertion a
assertLossAtAddress addr getMinLoss =
  assertLovelaceDeltaAtAddress addr (map negate <<< getMinLoss) eq

-- | Requires that the passed amount of lovelace was lost at the address
-- | by calling the contract.
assertLossAtAddress'
  :: forall (a :: Type)
   . Labeled Address
  -> BigInt
  -> ContractWrapAssertion a
assertLossAtAddress' addr minLoss =
  assertLossAtAddress addr (const $ pure minLoss)

assertTokenDeltaAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (CurrencySymbol /\ TokenName)
  -> (a -> Contract BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> ContractWrapAssertion a
assertTokenDeltaAtAddress addr (cs /\ tn) getExpected comp contract =
  runContractAssertionM contract $
    checkBalanceDeltaAtAddress addr contract
      \result valueBefore valueAfter -> do
        expected <- liftContractTestM $ lift $ getExpected result
        let
          actual :: BigInt
          actual = valueOf valueAfter cs tn - valueOf valueBefore cs tn

          unexpectedTokenDelta :: ContractAssertionFailure
          unexpectedTokenDelta =
            UnexpectedTokenDelta addr tn (ExpectedActual expected actual)

        assertContract unexpectedTokenDelta (comp actual expected)
        pure result

-- | Requires that the computed number of tokens was gained at the address
-- | by calling the contract.
assertTokenGainAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (CurrencySymbol /\ TokenName)
  -> (a -> Contract BigInt)
  -> ContractWrapAssertion a
assertTokenGainAtAddress addr token getMinGain =
  assertTokenDeltaAtAddress addr token getMinGain eq

-- | Requires that the passed number of tokens was gained at the address
-- | by calling the contract.
assertTokenGainAtAddress'
  :: forall (a :: Type)
   . Labeled Address
  -> (CurrencySymbol /\ TokenName /\ BigInt)
  -> ContractWrapAssertion a
assertTokenGainAtAddress' addr (cs /\ tn /\ minGain) =
  assertTokenGainAtAddress addr (cs /\ tn) (const $ pure minGain)

-- | Requires that the computed number of tokens was lost at the address
-- | by calling the contract.
assertTokenLossAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (CurrencySymbol /\ TokenName)
  -> (a -> Contract BigInt)
  -> ContractWrapAssertion a
assertTokenLossAtAddress addr token getMinLoss =
  assertTokenDeltaAtAddress addr token (map negate <<< getMinLoss) eq

-- | Requires that the passed number of tokens was lost at the address
-- | by calling the contract.
assertTokenLossAtAddress'
  :: forall (a :: Type)
   . Labeled Address
  -> (CurrencySymbol /\ TokenName /\ BigInt)
  -> ContractWrapAssertion a
assertTokenLossAtAddress' addr (cs /\ tn /\ minLoss) =
  assertTokenLossAtAddress addr (cs /\ tn) (const $ pure minLoss)

assertOutputHasDatumImpl
  :: OutputDatum
  -> Labeled TransactionOutputWithRefScript
  -> ContractAssertionM Unit Unit
assertOutputHasDatumImpl expectedDatum txOutput = do
  let actualDatum = unlabel txOutput ^. _output <<< _datum
  assertContractExpectedActual (UnexpectedDatumInOutput txOutput)
    expectedDatum
    actualDatum

-- | Requires that the transaction output contains the specified datum or
-- | datum hash.
assertOutputHasDatum
  :: OutputDatum
  -> Labeled TransactionOutputWithRefScript
  -> ContractTestM Unit
assertOutputHasDatum expectedDatum =
  runContractAssertionM' <<< assertOutputHasDatumImpl expectedDatum

-- | Checks whether the transaction output contains the specified datum or
-- | datum hash.
checkOutputHasDatum
  :: OutputDatum
  -> TransactionOutputWithRefScript
  -> Contract Boolean
checkOutputHasDatum expectedDatum txOutput =
  mkCheckFromAssertion $
    assertOutputHasDatumImpl expectedDatum (noLabel txOutput)

assertOutputHasRefScriptImpl
  :: ScriptRef
  -> Labeled TransactionOutputWithRefScript
  -> ContractAssertionM Unit Unit
assertOutputHasRefScriptImpl expectedRefScript txOutput = do
  let actualRefScript = unlabel txOutput ^. _scriptRef
  assertContractExpectedActual (UnexpectedRefScriptInOutput txOutput)
    (Just expectedRefScript)
    actualRefScript

-- | Requires that the transaction output contains the specified reference
-- | script.
assertOutputHasRefScript
  :: ScriptRef
  -> Labeled TransactionOutputWithRefScript
  -> ContractTestM Unit
assertOutputHasRefScript expectedRefScript =
  runContractAssertionM' <<< assertOutputHasRefScriptImpl expectedRefScript

-- | Checks whether the transaction output contains the specified reference
-- | script.
checkOutputHasRefScript
  :: ScriptRef
  -> TransactionOutputWithRefScript
  -> Contract Boolean
checkOutputHasRefScript expectedRefScript txOutput =
  mkCheckFromAssertion $
    assertOutputHasRefScriptImpl expectedRefScript (noLabel txOutput)

assertTxHasMetadataImpl
  :: forall (a :: Type)
   . MetadataType a
  => Eq a
  => Show a
  => Label
  -> TransactionHash
  -> a
  -> ContractAssertionM Unit Unit
assertTxHasMetadataImpl mdLabel txHash expectedMetadata = do
  generalMetadata <-
    assertContractM (TransactionHasNoMetadata txHash Nothing)
      (getTxMetadata txHash)

  rawMetadata <-
    assertContractM' (TransactionHasNoMetadata txHash (Just mdLabel))
      (Map.lookup (metadataLabel (Proxy :: Proxy a)) (unwrap generalMetadata))

  (metadata :: a) <-
    assertContractM' (CouldNotParseMetadata mdLabel)
      (fromMetadata rawMetadata)

  let expectedActual = show <$> ExpectedActual expectedMetadata metadata
  assertContract (UnexpectedMetadataValue mdLabel expectedActual)
    (metadata == expectedMetadata)

-- | Requires that the transaction contains the specified metadata.
assertTxHasMetadata
  :: forall (a :: Type)
   . MetadataType a
  => Eq a
  => Show a
  => Label
  -> TransactionHash
  -> a
  -> ContractTestM Unit
assertTxHasMetadata mdLabel txHash =
  runContractAssertionM' <<< assertTxHasMetadataImpl mdLabel txHash

-- | Checks whether the transaction contains the specified metadata.
checkTxHasMetadata
  :: forall (a :: Type)
   . MetadataType a
  => Eq a
  => Show a
  => TransactionHash
  -> a
  -> Contract Boolean
checkTxHasMetadata txHash =
  mkCheckFromAssertion <<< assertTxHasMetadataImpl mempty txHash

--------------------------------------------------------------------------------
-- function to cancel aff fibers on signal
--------------------------------------------------------------------------------

foreign import exitCode :: Int -> Effect Unit

-- | attaches a custom handler on SIGINt to kill the fiber.
-- | see `doc/plutip-testing#custom-SIGINT-handlers`
interruptOnSignal :: forall a. Signal -> Fiber a -> Effect Unit
interruptOnSignal signal fiber = Process.onSignal signal do
  launchAff_ do
    killFiber (error $ Signal.toString signal) fiber
