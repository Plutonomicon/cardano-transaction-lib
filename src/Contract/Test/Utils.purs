-- | This module provides an extensible interface for making various
-- | assertions about `Contract`s.
module Contract.Test.Utils
  ( ContractAssertionFailure
      ( CouldNotGetTxByHash
      , CouldNotParseMetadata
      , CustomFailure
      , MaxExUnitsExceeded
      , TransactionHasNoMetadata
      , UnexpectedDatumInOutput
      , UnexpectedLovelaceDelta
      , UnexpectedMetadataValue
      , UnexpectedRefScriptInOutput
      , UnexpectedTokenDelta
      )
  , ContractTestM
  , ContractWrapAssertion
  , ExpectedActual(ExpectedActual)
  , Label
  , Labeled(Labeled)
  , assertContractExpectedActual
  , assertContractTestMaybe
  , assertContractTestM
  , assertExUnitsNotExceed
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
  , assertValueDeltaAtAddress
  , checkNewUtxosAtAddress
  -- -- , checkOutputHasDatum
  -- -- , checkOutputHasRefScript
  -- -- , checkTxHasMetadata
  , label
  , exitCode
  , interruptOnSignal
  , unlabel
  , utxosAtAddress
  , valueAtAddress
  , withAssertions
  , mkSimpleAssertion
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
import Control.Monad.Error.Class (throwError)
import Control.Monad.Error.Class as E
import Control.Monad.Reader (ReaderT, ask, local, mapReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Cardano.Types.Transaction
  ( ExUnits
  , Transaction
  , _redeemers
  , _witnessSet
  )
import Ctl.Internal.Contract.Monad (ContractEnv)
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
import Data.Array (foldr)
import Data.Array (fromFoldable, mapWithIndex) as Array
import Data.BigInt (BigInt)
import Data.Either (Either(Right, Left))
import Data.Foldable (foldMap, null, sum)
import Data.Lens (non, to, traversed, view, (%~), (^.), (^..))
import Data.Lens.Record (prop)
import Data.List (List(Cons, Nil))
import Data.Map (filterKeys, lookup, values) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Posix.Signal (Signal)
import Data.Posix.Signal as Signal
import Data.String.Common (joinWith) as String
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Fiber, killFiber)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw, try)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.Process as Process
import Type.Proxy (Proxy(Proxy))

--------------------------------------------------------------------------------
-- `ContractTestM` and `ContractAssertionM` monads with related functions
--------------------------------------------------------------------------------

-- | Monad allowing for accumulation of assertion failures. Should be used in
-- | conjunction with `ContractAssertionM`.
type ContractTestM (a :: Type) =
  ReaderT (Ref (List ContractAssertionFailure)) Contract a

-- -- | Represents computations which may fail with `ContractAssertionFailure`,
-- -- | with the capability of storing some intermediate result, usually the result
-- -- | of the contract under test.
-- -- |
-- -- | Particularly useful for assertions that can control when the contract is
-- -- | run (`ContractWrapAssertion`s). So in case of a failure after the contract
-- -- | has already been executed, we can return the result of the contract, thus
-- -- | preventing the failure of subsequent assertions.
-- type ContractAssertionM (w :: Type) (a :: Type) =
--   -- ExceptT ContractAssertionFailure
--   --   (Writer (Maybe (Last w)) (ContractTestM)) a
--   ExceptT ContractAssertionFailure
--     ( WriterT (Maybe (Last w))
--         (WriterT (Array ContractAssertionFailure) (Contract))
--     )
--     a

-- runContractAssertionM
--   :: forall (a :: Type)
--    . ContractTestM a
--   -> ContractAssertionM a a
--   -> ContractTestM a
-- runContractAssertionM contract wrappedContract =
--   runWriterT (runExceptT wrappedContract) >>= case _ of
--     Right result /\ _ ->
--       pure result
--     Left failure /\ result ->
--       tell [ failure ] *> maybe contract (pure <<< unwrap) result

-- runContractAssertionM'
--   :: ContractAssertionM Unit Unit
--   -> ContractTestM Unit
-- runContractAssertionM' = runContractAssertionM (pure unit)

-- liftContractTestM
--   :: forall (w :: Type) (a :: Type)
--    . ContractTestM a
--   -> ContractAssertionM w a
-- liftContractTestM = lift <<< lift

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
  | MaxExUnitsExceeded ExUnits ExUnits
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

  show (MaxExUnitsExceeded maxExUnits exUnits) =
    "ExUnits limit exceeded: spent " <> show exUnits
      <> ", but the limit is "
      <> show maxExUnits

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

-- | An assertion that can control when the contract is run.
type ContractWrapAssertion a =
  ContractTestM a -> ContractTestM (ContractTestM Unit /\ ContractTestM a)

-- | Create an assertion that simply checks a `Contract` result.
-- | If a given `Contract` throws an exception, the assertion is never checked.
mkSimpleAssertion
  :: forall (a :: Type). (a -> ContractTestM Unit) -> ContractWrapAssertion a
mkSimpleAssertion f contract = do
  ref <- liftEffect $ Ref.new Nothing
  let
    run = do
      res <- contract
      liftEffect $ Ref.write (Just res) ref
      pure res
    finalize = do
      liftEffect (Ref.read ref) >>= traverse_ f
  pure $ finalize /\ run

assertContractTestM
  :: forall (w :: Type)
   . ContractAssertionFailure
  -> Boolean
  -> ContractTestM Unit
assertContractTestM failure cond
  | cond = pure unit
  | otherwise = tellFailure failure

assertContractTestMaybe
  :: forall (w :: Type) (a :: Type)
   . ContractAssertionFailure
  -> Maybe a
  -> ContractTestM a
assertContractTestMaybe msg =
  maybe (liftEffect $ throw $ show msg) pure

assertContractExpectedActual
  :: forall (a :: Type)
   . Eq a
  => (ExpectedActual a -> ContractAssertionFailure)
  -> a
  -> a
  -> ContractTestM Unit
assertContractExpectedActual mkAssertionFailure expected actual =
  assertContractTestM (mkAssertionFailure $ ExpectedActual expected actual)
    (expected == actual)

withAssertions
  :: forall (a :: Type) (assertions :: Type)
   . Array (ContractWrapAssertion a)
  -> Contract a
  -> Contract a
withAssertions assertions contract = do
  ref <- liftEffect $ Ref.new Nil
  result <-
    flip runReaderT ref go
  failures <- liftEffect $ Ref.read ref
  if null failures then pure result
  else throwContractError
    (ContractAssertionFailures $ Array.fromFoldable failures)
  where
  go :: ContractTestM a
  go = foldr
    ( \assertion acc -> do
        finalize /\ res <- assertion acc
        E.try res >>= case _ of
          Left failure -> do
            finalize
            throwError failure
          Right success -> do
            finalize
            pure (success :: a)
    )
    (lift contract :: ContractTestM a)
    assertions

-- mkCheckFromAssertion
--   :: forall (w :: Type) (a :: Type)
--    . ContractAssertionM w a
--   -> Contract Boolean
-- mkCheckFromAssertion =
--   map (fst <<< fst) <<< runWriterT <<< runWriterT <<< map isRight <<< runExceptT

--------------------------------------------------------------------------------
-- Assertions and checks
--------------------------------------------------------------------------------

utxosAtAddress
  :: forall (w :: Type)
   . Labeled Address
  -> ContractTestM UtxoMap
utxosAtAddress = lift <<< utxosAt <<< unlabel

valueAtAddress
  :: forall (w :: Type)
   . Labeled Address
  -> ContractTestM Value
valueAtAddress =
  map (foldMap (view (_output <<< _amount))) <<< utxosAtAddress

tellFailure
  :: ContractAssertionFailure -> ContractTestM Unit
tellFailure failure = do
  ask >>= liftEffect <<< Ref.modify_ (Cons failure)

checkNewUtxosAtAddress
  :: forall (w :: Type) (a :: Type)
   . Labeled Address
  -> TransactionHash
  -> (Array TransactionOutputWithRefScript -> ContractTestM a)
  -> ContractTestM a
checkNewUtxosAtAddress addr txHash check =
  utxosAtAddress addr >>= \utxos ->
    check $ Array.fromFoldable $ Map.values $
      Map.filterKeys (\oref -> (unwrap oref).transactionId == txHash) utxos

-- | Sets a limit on `ExUnits` budget. All ExUnits values of all submitted transactions are combined. Transactions that are constructed, but not submitted, are not considered.
-- | The execution of the `Contract` will not be interrupted in case the `ExUnits` limit is reached.
assertExUnitsNotExceed
  :: forall (a :: Type)
   . ExUnits
  -> ContractWrapAssertion a
assertExUnitsNotExceed maxExUnits contract = do
  (ref :: Ref ExUnits) <- liftEffect $ Ref.new { mem: zero, steps: zero }
  let
    submitHook :: Transaction -> Effect Unit
    submitHook tx = do
      let
        (newExUnits :: ExUnits) = sum $ tx ^..
          _witnessSet
            <<< _redeemers
            <<< non []
            <<< traversed
            <<< to (unwrap >>> _.exUnits)
      Ref.modify_ (add newExUnits) ref

    setSubmitHook :: ContractEnv -> ContractEnv
    setSubmitHook =
      prop (Proxy :: Proxy "hooks") <<< prop (Proxy :: Proxy "onSubmit")
        -- Extend a hook action if it exists, or set it to `Just submitHook`
        %~ maybe (Just submitHook)
          \oldHook -> Just \tx -> do
            -- ignore possible exception from the old hook
            void $ try $ oldHook tx
            submitHook tx

    finalize :: ContractTestM Unit
    finalize = do
      exUnits <- liftEffect $ Ref.read ref
      assertContractTestM (MaxExUnitsExceeded maxExUnits exUnits)
        (maxExUnits >= exUnits)

  pure (finalize /\ mapReaderT (local setSubmitHook) contract)

valueAtAddress'
  :: forall (w :: Type)
   . Labeled Address
  -> ContractTestM Value
valueAtAddress' = map (foldMap (view (_output <<< _amount))) <<< lift
  <<< utxosAt
  <<< unlabel

-- | Arguments are:
-- |
-- | - a labeled address
-- | - a callback that implements the assertion, accepting `Contract` execution
-- |   result, and values (before and after)
assertValueDeltaAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (a -> Contract BigInt)
  -> (a -> Value -> Value -> ContractTestM Unit)
  -> ContractWrapAssertion a
assertValueDeltaAtAddress addr getExpected check contract = do
  valueBefore <- valueAtAddress' addr
  ref <- liftEffect $ Ref.new Nothing
  let
    finalize = do
      valueAfter <- valueAtAddress' addr
      liftEffect (Ref.read ref) >>= case _ of
        Nothing -> pure unit -- tellFailure $ CustomFailure "Contract did not run"
        Just res -> check res valueBefore valueAfter
    contract' = do
      res <- contract
      liftEffect $ Ref.write (Just res) ref
      pure res
  pure (finalize /\ contract')

assertLovelaceDeltaAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (a -> Contract BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> ContractWrapAssertion a
assertLovelaceDeltaAtAddress addr getExpected comp contract = do
  assertValueDeltaAtAddress addr getExpected check contract
  -- valueBefore <- valueAtAddress' addr
  -- ref <- liftEffect $ Ref.new Nothing
  -- let
  --   finalize = do
  --     valueAfter <- valueAtAddress' addr
  --     liftEffect (Ref.read ref) >>= case _ of
  --       Nothing -> pure unit -- tellFailure $ CustomFailure "Contract did not run"
  --       Just res -> check res valueBefore valueAfter
  --   contract' = do
  --     res <- contract
  --     liftEffect $ Ref.write (Just res) ref
  --     pure res
  -- pure (finalize /\ contract')
  where
  check :: a -> Value -> Value -> ContractTestM Unit
  check result valueBefore valueAfter = do
    expected <- lift $ getExpected result
    let
      actual :: BigInt
      actual = valueToCoin' valueAfter - valueToCoin' valueBefore

      unexpectedLovelaceDelta :: ContractAssertionFailure
      unexpectedLovelaceDelta =
        UnexpectedLovelaceDelta addr (ExpectedActual expected actual)

    assertContractTestM unexpectedLovelaceDelta (comp actual expected)
    pure unit

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
  assertValueDeltaAtAddress addr getExpected check contract
  where
  check :: a -> Value -> Value -> ContractTestM Unit
  check result valueBefore valueAfter = do
    expected <- lift $ getExpected result
    let
      actual :: BigInt
      actual = valueOf valueAfter cs tn - valueOf valueBefore cs tn

      unexpectedTokenDelta :: ContractAssertionFailure
      unexpectedTokenDelta =
        UnexpectedTokenDelta addr tn (ExpectedActual expected actual)

    assertContractTestM unexpectedTokenDelta (comp actual expected)

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
  -> ContractTestM Unit
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
assertOutputHasDatum expectedDatum = -- TODO: simplify

  assertOutputHasDatumImpl expectedDatum

-- -- | Checks whether the transaction output contains the specified datum or
-- -- | datum hash.
-- checkOutputHasDatum
--   :: OutputDatum
--   -> TransactionOutputWithRefScript
--   -> Contract Boolean
-- checkOutputHasDatum expectedDatum txOutput =
--   mkCheckFromAssertion $
--     assertOutputHasDatumImpl expectedDatum (noLabel txOutput)

-- assertOutputHasRefScriptImpl
--   :: ScriptRef
--   -> Labeled TransactionOutputWithRefScript
--   -> ContractAssertionM Unit Unit
-- assertOutputHasRefScriptImpl expectedRefScript txOutput = do
--   let actualRefScript = unlabel txOutput ^. _scriptRef
--   assertContractExpectedActual (UnexpectedRefScriptInOutput txOutput)
--     (Just expectedRefScript)
--     actualRefScript

-- | Requires that the transaction output contains the specified reference
-- | script.
assertOutputHasRefScript
  :: ScriptRef
  -> Labeled TransactionOutputWithRefScript
  -> ContractTestM Unit
assertOutputHasRefScript expectedRefScript txOutput = do
  let actualRefScript = unlabel txOutput ^. _scriptRef
  assertContractExpectedActual (UnexpectedRefScriptInOutput txOutput)
    (Just expectedRefScript)
    actualRefScript

-- -- | Checks whether the transaction output contains the specified reference
-- -- | script.
-- checkOutputHasRefScript
--   :: ScriptRef
--   -> TransactionOutputWithRefScript
--   -> Contract Boolean
-- checkOutputHasRefScript expectedRefScript txOutput =
--   mkCheckFromAssertion $
--     assertOutputHasRefScriptImpl expectedRefScript (noLabel txOutput)

assertTxHasMetadata
  :: forall (a :: Type)
   . MetadataType a
  => Eq a
  => Show a
  => Label
  -> TransactionHash
  -> a
  -> ContractWrapAssertion a
assertTxHasMetadata mdLabel txHash expectedMetadata contract = do
  pure (finalize /\ contract)
  where
  finalize = do
    generalMetadata <-
      assertContractTestMaybe (TransactionHasNoMetadata txHash Nothing)
        =<< lift (getTxMetadata txHash)

    rawMetadata <-
      assertContractTestMaybe (TransactionHasNoMetadata txHash (Just mdLabel))
        (Map.lookup (metadataLabel (Proxy :: Proxy a)) (unwrap generalMetadata))

    (metadata :: a) <-
      assertContractTestMaybe (CouldNotParseMetadata mdLabel)
        (fromMetadata rawMetadata)

    let expectedActual = show <$> ExpectedActual expectedMetadata metadata
    assertContractTestM (UnexpectedMetadataValue mdLabel expectedActual)
      (metadata == expectedMetadata)

-- -- | Checks whether the transaction contains the specified metadata.
-- checkTxHasMetadata
--   :: forall (a :: Type)
--    . MetadataType a
--   => Eq a
--   => Show a
--   => TransactionHash
--   -> a
--   -> Contract Boolean
-- checkTxHasMetadata txHash =
--   mkCheckFromAssertion <<< assertTxHasMetadataImpl mempty txHash

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
