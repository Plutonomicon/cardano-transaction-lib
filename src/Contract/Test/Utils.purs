-- | This module provides an extensible interface for making various 
-- | assertions about `Contract`s.
module Contract.Test.Utils
  ( class ContractAssertions
  , ContractAssertionFailure
      ( CouldNotGetTxByHash
      , CouldNotGetUtxosAtAddress
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
import Contract.Monad (Contract, throwContractError)
import Contract.PlutusData (OutputDatum)
import Contract.Transaction
  ( ScriptRef
  , Transaction(Transaction)
  , TransactionHash
  , TransactionOutputWithRefScript
  , getTxByHash
  )
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , valueOf
  , valueToCoin'
  )
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
import Data.Semigroup.Last (Last(Last))
import Data.String.Common (joinWith) as String
import Data.Traversable (traverse_)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Type.Proxy (Proxy(Proxy))

--------------------------------------------------------------------------------
-- `ContractTestM` and `ContractAssertionM` monads with related functions
--------------------------------------------------------------------------------

-- | Monad allowing for accumulation of assertion failures. Should be used in 
-- | conjunction with `ContractAssertionM`.
type ContractTestM (r :: Row Type) (a :: Type) =
  WriterT (Array ContractAssertionFailure) (Contract r) a

-- | Represents computations which may fail with `ContractAssertionFailure`, 
-- | with the capability of storing some intermediate result, usually the result 
-- | of the contract under test.
-- | 
-- | Particularly useful for assertions that can control when the contract is 
-- | run (`ContractWrapAssertion`s). So in case of a failure after the contract 
-- | has already been executed, we can return the result of the contract, thus 
-- | preventing the failure of subsequent assertions.
type ContractAssertionM (r :: Row Type) (w :: Type) (a :: Type) =
  -- ExceptT ContractAssertionFailure 
  --   (Writer (Maybe (Last w)) (ContractTestM r)) a
  ExceptT ContractAssertionFailure
    ( WriterT (Maybe (Last w))
        (WriterT (Array ContractAssertionFailure) (Contract r))
    )
    a

runContractAssertionM
  :: forall (r :: Row Type) (a :: Type)
   . ContractTestM r a
  -> ContractAssertionM r a a
  -> ContractTestM r a
runContractAssertionM contract wrappedContract =
  runWriterT (runExceptT wrappedContract) >>= case _ of
    Right result /\ _ ->
      pure result
    Left failure /\ result ->
      tell [ failure ] *> maybe contract (pure <<< unwrap) result

runContractAssertionM'
  :: forall (r :: Row Type)
   . ContractAssertionM r Unit Unit
  -> ContractTestM r Unit
runContractAssertionM' = runContractAssertionM (pure unit)

liftContractTestM
  :: forall (r :: Row Type) (w :: Type) (a :: Type)
   . ContractTestM r a
  -> ContractAssertionM r w a
liftContractTestM = lift <<< lift

--------------------------------------------------------------------------------
-- Data types and functions for building assertion failures
--------------------------------------------------------------------------------

data ContractAssertionFailure
  = CouldNotGetTxByHash TransactionHash
  | CouldNotGetUtxosAtAddress (Labeled Address)
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

  show (CouldNotGetUtxosAtAddress addr) =
    "Could not get utxos at " <> show addr

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
type ContractBasicAssertion (r :: Row Type) (a :: Type) (b :: Type) =
  a -> ContractTestM r b

-- | An assertion that can control when the contract is run. The assertion 
-- | inhabiting this type should not call the contract more than once, as other 
-- | assertions need to be able to make this assumption to succesfully compose.
type ContractWrapAssertion (r :: Row Type) (a :: Type) =
  ContractTestM r a -> ContractTestM r a

-- | Class to unify different methods of making assertions about a contract 
-- | under a single interface. Note that the typechecker may need some help when 
-- | using this class; try providing type annotations for your assertions using 
-- | the type aliases for the instances of this class.
class ContractAssertions (f :: Type) (r :: Row Type) (a :: Type) where
  -- | Wrap a contract in an assertion. The wrapped contract itself becomes a 
  -- | contract which can be wrapped, allowing for composition of assertions.
  -- |
  -- | No guarantees are made about the order in which assertions are made. 
  -- | Assertions with side effects should not be used.
  wrapAndAssert :: ContractTestM r a -> f -> ContractTestM r a

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
  :: forall (r :: Row Type) (w :: Type)
   . ContractAssertionFailure
  -> Boolean
  -> ContractAssertionM r w Unit
assertContract failure cond
  | cond = pure unit
  | otherwise = except (Left failure)

assertContractM'
  :: forall (r :: Row Type) (w :: Type) (a :: Type)
   . ContractAssertionFailure
  -> Maybe a
  -> ContractAssertionM r w a
assertContractM' msg = maybe (except $ Left msg) pure

assertContractM
  :: forall (r :: Row Type) (w :: Type) (a :: Type)
   . ContractAssertionFailure
  -> Contract r (Maybe a)
  -> ContractAssertionM r w a
assertContractM msg cm =
  liftContractTestM (lift cm) >>= assertContractM' msg

assertContractExpectedActual
  :: forall (r :: Row Type) (w :: Type) (a :: Type)
   . Eq a
  => (ExpectedActual a -> ContractAssertionFailure)
  -> a
  -> a
  -> ContractAssertionM r w Unit
assertContractExpectedActual mkAssertionFailure expected actual =
  assertContract (mkAssertionFailure $ ExpectedActual expected actual)
    (expected == actual)

withAssertions
  :: forall (r :: Row Type) (a :: Type) (assertions :: Type)
   . ContractAssertions assertions r a
  => assertions
  -> Contract r a
  -> Contract r a
withAssertions assertions contract = do
  result /\ failures <-
    runWriterT $ wrapAndAssert (lift contract) assertions
  if null failures then pure result
  else throwContractError (ContractAssertionFailures failures)

mkCheckFromAssertion
  :: forall (r :: Row Type) (w :: Type) (a :: Type)
   . ContractAssertionM r w a
  -> Contract r Boolean
mkCheckFromAssertion =
  map (fst <<< fst) <<< runWriterT <<< runWriterT <<< map isRight <<< runExceptT

--------------------------------------------------------------------------------
-- Assertions and checks
--------------------------------------------------------------------------------

utxosAtAddress
  :: forall (r :: Row Type) (w :: Type)
   . Labeled Address
  -> ContractAssertionM r w UtxoMap
utxosAtAddress addr =
  assertContractM (CouldNotGetUtxosAtAddress addr) (utxosAt $ unlabel addr)

valueAtAddress
  :: forall (r :: Row Type) (w :: Type)
   . Labeled Address
  -> ContractAssertionM r w Value
valueAtAddress =
  map (foldMap (view (_output <<< _amount))) <<< utxosAtAddress

checkBalanceDeltaAtAddress
  :: forall (r :: Row Type) (w :: Type) (a :: Type)
   . Labeled Address
  -> ContractTestM r w
  -> (w -> Value -> Value -> ContractAssertionM r w a)
  -> ContractAssertionM r w a
checkBalanceDeltaAtAddress addr contract check = do
  valueBefore <- valueAtAddress addr
  res <- liftContractTestM contract
  tell (Just $ Last res)
  valueAfter <- valueAtAddress addr
  check res valueBefore valueAfter

checkNewUtxosAtAddress
  :: forall (r :: Row Type) (w :: Type) (a :: Type)
   . Labeled Address
  -> TransactionHash
  -> (Array TransactionOutputWithRefScript -> ContractAssertionM r w a)
  -> ContractAssertionM r w a
checkNewUtxosAtAddress addr txHash check =
  utxosAtAddress addr >>= \utxos ->
    check $ Array.fromFoldable $ Map.values $
      Map.filterKeys (\oref -> (unwrap oref).transactionId == txHash) utxos

assertLovelaceDeltaAtAddress
  :: forall (r :: Row Type) (a :: Type)
   . Labeled Address
  -> (a -> Contract r BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> ContractWrapAssertion r a
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

assertTokenDeltaAtAddress
  :: forall (r :: Row Type) (a :: Type)
   . Labeled Address
  -> (CurrencySymbol /\ TokenName)
  -> (a -> Contract r BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> ContractWrapAssertion r a
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
  :: forall (r :: Row Type) (a :: Type)
   . Labeled Address
  -> (CurrencySymbol /\ TokenName)
  -> (a -> Contract r BigInt)
  -> ContractWrapAssertion r a
assertTokenGainAtAddress addr token getMinGain =
  assertTokenDeltaAtAddress addr token getMinGain eq

-- | Requires that the passed number of tokens was gained at the address 
-- | by calling the contract.
assertTokenGainAtAddress'
  :: forall (r :: Row Type) (a :: Type)
   . Labeled Address
  -> (CurrencySymbol /\ TokenName /\ BigInt)
  -> ContractWrapAssertion r a
assertTokenGainAtAddress' addr (cs /\ tn /\ minGain) =
  assertTokenGainAtAddress addr (cs /\ tn) (const $ pure minGain)

-- | Requires that the computed number of tokens was lost at the address 
-- | by calling the contract.
assertTokenLossAtAddress
  :: forall (r :: Row Type) (a :: Type)
   . Labeled Address
  -> (CurrencySymbol /\ TokenName)
  -> (a -> Contract r BigInt)
  -> ContractWrapAssertion r a
assertTokenLossAtAddress addr token getMinLoss =
  assertTokenDeltaAtAddress addr token (map negate <<< getMinLoss) eq

-- | Requires that the passed number of tokens was lost at the address 
-- | by calling the contract.
assertTokenLossAtAddress'
  :: forall (r :: Row Type) (a :: Type)
   . Labeled Address
  -> (CurrencySymbol /\ TokenName /\ BigInt)
  -> ContractWrapAssertion r a
assertTokenLossAtAddress' addr (cs /\ tn /\ minLoss) =
  assertTokenLossAtAddress addr (cs /\ tn) (const $ pure minLoss)

assertOutputHasDatumImpl
  :: forall (r :: Row Type)
   . OutputDatum
  -> Labeled TransactionOutputWithRefScript
  -> ContractAssertionM r Unit Unit
assertOutputHasDatumImpl expectedDatum txOutput = do
  let actualDatum = unlabel txOutput ^. _output <<< _datum
  assertContractExpectedActual (UnexpectedDatumInOutput txOutput)
    expectedDatum
    actualDatum

-- | Requires that the transaction output contains the specified datum or 
-- | datum hash.
assertOutputHasDatum
  :: forall (r :: Row Type)
   . OutputDatum
  -> Labeled TransactionOutputWithRefScript
  -> ContractTestM r Unit
assertOutputHasDatum expectedDatum =
  runContractAssertionM' <<< assertOutputHasDatumImpl expectedDatum

-- | Checks whether the transaction output contains the specified datum or 
-- | datum hash.
checkOutputHasDatum
  :: forall (r :: Row Type)
   . OutputDatum
  -> TransactionOutputWithRefScript
  -> Contract r Boolean
checkOutputHasDatum expectedDatum txOutput =
  mkCheckFromAssertion $
    assertOutputHasDatumImpl expectedDatum (noLabel txOutput)

assertOutputHasRefScriptImpl
  :: forall (r :: Row Type)
   . ScriptRef
  -> Labeled TransactionOutputWithRefScript
  -> ContractAssertionM r Unit Unit
assertOutputHasRefScriptImpl expectedRefScript txOutput = do
  let actualRefScript = unlabel txOutput ^. _scriptRef
  assertContractExpectedActual (UnexpectedRefScriptInOutput txOutput)
    (Just expectedRefScript)
    actualRefScript

-- | Requires that the transaction output contains the specified reference 
-- | script.
assertOutputHasRefScript
  :: forall (r :: Row Type)
   . ScriptRef
  -> Labeled TransactionOutputWithRefScript
  -> ContractTestM r Unit
assertOutputHasRefScript expectedRefScript =
  runContractAssertionM' <<< assertOutputHasRefScriptImpl expectedRefScript

-- | Checks whether the transaction output contains the specified reference 
-- | script.
checkOutputHasRefScript
  :: forall (r :: Row Type)
   . ScriptRef
  -> TransactionOutputWithRefScript
  -> Contract r Boolean
checkOutputHasRefScript expectedRefScript txOutput =
  mkCheckFromAssertion $
    assertOutputHasRefScriptImpl expectedRefScript (noLabel txOutput)

assertTxHasMetadataImpl
  :: forall (r :: Row Type) (a :: Type)
   . MetadataType a
  => Eq a
  => Show a
  => Label
  -> TransactionHash
  -> a
  -> ContractAssertionM r Unit Unit
assertTxHasMetadataImpl mdLabel txHash expectedMetadata = do
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

-- | Requires that the transaction contains the specified metadata.
assertTxHasMetadata
  :: forall (r :: Row Type) (a :: Type)
   . MetadataType a
  => Eq a
  => Show a
  => Label
  -> TransactionHash
  -> a
  -> ContractTestM r Unit
assertTxHasMetadata mdLabel txHash =
  runContractAssertionM' <<< assertTxHasMetadataImpl mdLabel txHash

-- | Checks whether the transaction contains the specified metadata.
checkTxHasMetadata
  :: forall (r :: Row Type) (a :: Type)
   . MetadataType a
  => Eq a
  => Show a
  => TransactionHash
  -> a
  -> Contract r Boolean
checkTxHasMetadata txHash =
  mkCheckFromAssertion <<< assertTxHasMetadataImpl mempty txHash
