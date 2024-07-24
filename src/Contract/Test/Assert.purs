module Contract.Test.Assert
  ( ContractAssertionFailure
      ( CouldNotGetTxByHash
      , CouldNotParseMetadata
      , CustomFailure
      , SkippedTest
      , MaxExUnitsExceeded
      , TransactionHasNoMetadata
      , UnexpectedDatumInOutput
      , UnexpectedLovelaceDelta
      , UnexpectedMetadataValue
      , UnexpectedRefScriptInOutput
      , UnexpectedTokenDelta
      , FailedToGetExpectedValue
      )
  , ContractAssertion
  , ContractCheck
  , ExpectedActual(ExpectedActual)
  , Label
  , Labeled(Labeled)
  , assertContract
  , assertContractEqual
  , assertContractExpectedActual
  , assertContractMaybe
  , assertNewUtxosAtAddress
  , assertNewUtxosInWallet
  , assertOutputHasDatum
  , assertOutputHasRefScript
  , assertTxHasMetadata
  , assertionToCheck
  , checkExUnitsNotExceed
  , checkGainAtAddress
  , checkGainAtAddress'
  , checkGainInWallet
  , checkGainInWallet'
  , checkLossAtAddress
  , checkLossAtAddress'
  , checkLossInWallet
  , checkLossInWallet'
  , checkLovelaceDeltaAtAddress
  , checkLovelaceDeltaInWallet
  , checkTokenDeltaAtAddress
  , checkTokenDeltaInWallet
  , checkTokenGainAtAddress
  , checkTokenGainAtAddress'
  , checkTokenGainInWallet
  , checkTokenGainInWallet'
  , checkTokenLossAtAddress
  , checkTokenLossAtAddress'
  , checkTokenLossInWallet
  , checkTokenLossInWallet'
  , checkValueDeltaAtAddress
  , checkValueDeltaInWallet
  , collectAssertionFailures
  , label
  , noLabel
  , printContractAssertionFailure
  , printContractAssertionFailures
  , printExpectedActual
  , printLabeled
  , runChecks
  , tellFailure
  , unlabel
  ) where

import Prelude

import Cardano.FromMetadata (fromMetadata)
import Cardano.Serialization.Lib (toBytes)
import Cardano.Types
  ( Address
  , Asset(Asset)
  , AssetName
  , ExUnits(ExUnits)
  , OutputDatum
  , ScriptHash
  , ScriptRef
  , Transaction
  , TransactionHash
  , TransactionOutput
  , Value
  , _amount
  , _datum
  , _redeemers
  , _scriptRef
  , _witnessSet
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Value as Value
import Contract.Monad (Contract)
import Contract.Prelude (Effect)
import Contract.Transaction (getTxAuxiliaryData)
import Contract.Utxos (utxosAt)
import Contract.Wallet (getWalletBalance, getWalletUtxos)
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Error.Class as E
import Control.Monad.Reader (ReaderT, ask, local, mapReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Contract.Monad (ContractEnv)
import Ctl.Internal.Metadata.MetadataType (class MetadataType, metadataLabel)
import Data.Array (foldr)
import Data.Array (fromFoldable, length, mapWithIndex, partition) as Array
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either, either, hush)
import Data.Foldable (fold, foldMap, null)
import Data.Generic.Rep (class Generic)
import Data.Lens (to, view, (%~), (^.))
import Data.Lens.Record (prop)
import Data.List (List(Cons, Nil))
import Data.Map (empty, filterKeys, lookup, values) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String (trim) as String
import Data.String.Common (joinWith) as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message, throw, try)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import JS.BigInt (BigInt)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(Proxy))

-- | Monad allowing for accumulation of assertion failures.
type ContractAssertion (a :: Type) =
  ReaderT (Ref (List ContractAssertionFailure)) Contract a

--------------------------------------------------------------------------------
-- Data types and functions for building assertion failures
--------------------------------------------------------------------------------

data ContractAssertionFailure
  = CouldNotGetTxByHash TransactionHash
  | CouldNotParseMetadata Label
  | TransactionHasNoMetadata TransactionHash (Maybe Label)
  | UnexpectedDatumInOutput (Labeled TransactionOutput)
      (ExpectedActual (Maybe OutputDatum))
  | UnexpectedLovelaceDelta (Maybe (Labeled Address)) (ExpectedActual BigInt)
  | UnexpectedMetadataValue Label (ExpectedActual String)
  | UnexpectedRefScriptInOutput (Labeled TransactionOutput)
      (ExpectedActual (Maybe ScriptRef))
  | UnexpectedTokenDelta (Maybe (Labeled Address)) AssetName
      (ExpectedActual BigInt)
  | FailedToGetExpectedValue String
  | MaxExUnitsExceeded (ExpectedActual ExUnits)
  | CustomFailure String
  | SkippedTest String

derive instance Eq ContractAssertionFailure
derive instance Generic ContractAssertionFailure _

instance Show ContractAssertionFailure where
  show = genericShow

-- | A pretty-printing function that produces a human-readable report on failures.
printContractAssertionFailures :: Array ContractAssertionFailure -> String
printContractAssertionFailures failures =
  String.trim $ errorText <> warningText
  where
  isWarning :: ContractAssertionFailure -> Boolean
  isWarning = case _ of
    SkippedTest _ -> true
    _ -> false

  { yes: warnings, no: errors } = Array.partition isWarning failures

  listFailures = String.joinWith "\n\n    "
    <<< Array.mapWithIndex
      ( \ix elem -> show (ix + one) <> ". " <> printContractAssertionFailure
          elem
      )
  errorText =
    if Array.length errors > 0 then
      "In addition to the error above, the following `Contract` assertions"
        <> " have failed:\n\n    "
        <> listFailures errors
        <> "\n\n"
    else ""
  warningText =
    if Array.length warnings > 0 then
      "The following `Contract` checks have been skipped due to an exception: \n\n    "
        <>
          listFailures warnings
    else ""

-- | Pretty printing function that produces a human readable report for a
-- | single `ContractAssertionFailure`
printContractAssertionFailure :: ContractAssertionFailure -> String
printContractAssertionFailure = case _ of
  CouldNotGetTxByHash txHash ->
    "Could not get tx by hash " <> showTxHash txHash

  CouldNotParseMetadata mdLabel ->
    "Could not parse " <> mdLabel <> " metadata"

  TransactionHasNoMetadata txHash mdLabel ->
    "Tx with id " <> showTxHash txHash <> " does not hold "
      <> (maybe "" (_ <> " ") mdLabel <> "metadata")

  UnexpectedDatumInOutput txOutput expectedActual ->
    "Unexpected datum in output " <> printLabeled txOutput <> " " <>
      printExpectedActual expectedActual

  UnexpectedLovelaceDelta (Just addr) expectedActual ->
    "Unexpected lovelace delta at address "
      <> printLabeled addr
      <> printExpectedActual expectedActual
  UnexpectedLovelaceDelta Nothing expectedActual ->
    "Unexpected lovelace delta in wallet: "
      <> printExpectedActual expectedActual

  UnexpectedMetadataValue mdLabel expectedActual ->
    "Unexpected " <> mdLabel <> " metadata value" <> printExpectedActual
      expectedActual

  UnexpectedRefScriptInOutput txOutput expectedActual ->
    "Unexpected reference script in output "
      <> (printLabeled txOutput <> printExpectedActual expectedActual)

  UnexpectedTokenDelta (Just addr) tn expectedActual ->
    "Unexpected token delta " <> show tn <> " at address "
      <> printLabeled addr
      <> printExpectedActual expectedActual

  UnexpectedTokenDelta Nothing tn expectedActual ->
    "Unexpected token delta " <> show tn <> " in wallet: "
      <> printExpectedActual expectedActual

  FailedToGetExpectedValue err ->
    "Error while trying to get expected value: " <> err

  MaxExUnitsExceeded expectedActual ->
    "ExUnits limit exceeded: " <> printExpectedActualWith printExUnits
      expectedActual

  CustomFailure msg -> msg
  SkippedTest msg -> msg

showTxHash :: TransactionHash -> String
showTxHash = byteArrayToHex <<< toBytes <<< unwrap

printExUnits :: ExUnits -> String
printExUnits (ExUnits { mem, steps }) =
  "{ mem: " <> BigNum.toString mem
    <> ", steps: "
    <> BigNum.toString steps
    <> " }"

type Label = String

data Labeled (a :: Type) = Labeled a (Maybe Label)

derive instance Eq a => Eq (Labeled a)
derive instance Ord a => Ord (Labeled a)
derive instance Generic (Labeled a) _

instance Show a => Show (Labeled a) where
  show = genericShow

label :: forall (a :: Type). a -> Label -> Labeled a
label x l = Labeled x (Just l)

unlabel :: forall (a :: Type). Labeled a -> a
unlabel (Labeled x _) = x

noLabel :: forall (a :: Type). a -> Labeled a
noLabel = flip Labeled Nothing

printLabeled :: forall (a :: Type). Show a => Labeled a -> String
printLabeled (Labeled _ (Just l)) = l
printLabeled (Labeled x Nothing) = show x

data ExpectedActual (a :: Type) = ExpectedActual a a

derive instance Eq a => Eq (ExpectedActual a)
derive instance Ord a => Ord (ExpectedActual a)
derive instance Generic (ExpectedActual a) _

instance Show a => Show (ExpectedActual a) where
  show = genericShow

derive instance Functor ExpectedActual

printExpectedActual :: forall (a :: Type). Show a => ExpectedActual a -> String
printExpectedActual = printExpectedActualWith show

printExpectedActualWith
  :: forall (a :: Type)
   . (a -> String)
  -> ExpectedActual a
  -> String
printExpectedActualWith format (ExpectedActual expected actual) =
  " Expected: " <> format expected <> ", Actual: " <> format actual <> " "

--------------------------------------------------------------------------------
-- Different types of assertions, Assertion composition, Basic functions
--------------------------------------------------------------------------------

-- | A check that can run some initialization code before the `Contract` is run
-- | and check the results afterwards. It is used to implement assertions that
-- | require state monitoring, e.g. checking gains at address.
type ContractCheck a =
  ContractAssertion a
  -> ContractAssertion (ContractAssertion a /\ ContractAssertion Unit)

-- | Create a check that simply asserts something about a `Contract` result.
-- |
-- | If a `Contract` throws an exception, the assertion is never checked,
-- | because the result is never computed. In this case, a warning will be
-- | printed, containing the given description.
assertionToCheck
  :: forall (a :: Type)
   . String
  -> (a -> ContractAssertion Unit)
  -> ContractCheck a
assertionToCheck description f contract = do
  putRef /\ getRef <- tieRef
  let
    run = do
      res <- contract
      putRef res
    finalize = do
      getRef >>= case _ of
        Nothing -> tellFailure $ SkippedTest description
        Just res -> f res
  pure $ run /\ finalize

assertContract
  :: ContractAssertionFailure
  -> Boolean
  -> ContractAssertion Unit
assertContract failure cond = unless cond $ tellFailure failure

assertContractMaybe
  :: forall (a :: Type)
   . ContractAssertionFailure
  -> Maybe a
  -> ContractAssertion a
assertContractMaybe msg =
  maybe (liftEffect $ throw $ printContractAssertionFailure msg) pure

assertContractExpectedActual
  :: forall (a :: Type)
   . Eq a
  => (ExpectedActual a -> ContractAssertionFailure)
  -> a
  -> a
  -> ContractAssertion Unit
assertContractExpectedActual mkAssertionFailure expected actual =
  assertContract (mkAssertionFailure $ ExpectedActual expected actual)
    (expected == actual)

assertContractEqual
  :: forall (a :: Type)
   . Eq a
  => Show a
  => a
  -> a
  -> ContractAssertion Unit
assertContractEqual = assertContractExpectedActual
  \(ExpectedActual expected actual) -> CustomFailure $
    "assertContractEqual: failure, expected:\n" <> show expected
      <> "\ngot:\n"
      <> show actual

-- | Like `runChecks`, but does not throw a user-readable report, collecting
-- | the exceptions instead.
collectAssertionFailures
  :: forall (a :: Type)
   . Array (ContractCheck a)
  -> ContractAssertion a
  -> Contract (Either Error a /\ Array ContractAssertionFailure)
collectAssertionFailures assertions contract = do
  ref <- liftEffect $ Ref.new Nil
  eiResult <- E.try $ flip runReaderT ref wrappedContract
  failures <- liftEffect $ Ref.read ref
  pure (eiResult /\ Array.fromFoldable failures)
  where
  wrapAssertion :: ContractCheck a -> ContractAssertion a -> ContractAssertion a
  wrapAssertion assertion acc = do
    run /\ finalize <- assertion acc
    E.try run >>= \res -> finalize *> liftEither res

  wrappedContract :: ContractAssertion a
  wrappedContract = foldr wrapAssertion contract assertions

-- | Accepts an array of checks and interprets them into a `Contract`.
runChecks
  :: forall (a :: Type)
   . Array (ContractCheck a)
  -> ContractAssertion a
  -> Contract a
runChecks assertions contract = do
  eiResult /\ failures <- collectAssertionFailures assertions contract
  if null failures then either
    (liftEffect <<< throwError <<< error <<< reportException)
    pure
    eiResult
  else do
    let
      errorStr = either reportException (const "") eiResult
      errorReport =
        String.trim
          ( errorStr <> "\n\n" <>
              printContractAssertionFailures (Array.fromFoldable failures)
          ) <> "\n"
    -- error trace from the exception itself will be appended here
    liftEffect $ throwError $ error errorReport
  where
  reportException :: Error -> String
  reportException error = "\n\nAn exception has been thrown: \n\n" <> message
    error

----------------------- UTxOs ---------------------------------------------------

-- | Assert some property of address UTxOs created by a Transaction.
assertNewUtxosAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> TransactionHash
  -> (Array TransactionOutput -> ContractAssertion a)
  -> ContractAssertion a
assertNewUtxosAtAddress addr txHash check =
  lift (utxosAt $ unlabel addr) >>= \utxos ->
    check $ Array.fromFoldable $ Map.values $
      Map.filterKeys (\oref -> (unwrap oref).transactionId == txHash) utxos

-- | Assert some property of wallet UTxOs created by a Transaction.
assertNewUtxosInWallet
  :: forall (a :: Type)
   . TransactionHash
  -> (Array TransactionOutput -> ContractAssertion a)
  -> ContractAssertion a
assertNewUtxosInWallet txHash check =
  lift getWalletUtxos >>= fromMaybe Map.empty >>> \utxos ->
    check $ Array.fromFoldable $ Map.values $
      Map.filterKeys (\oref -> (unwrap oref).transactionId == txHash) utxos

------------------------ Execution units ----------------------------------------

-- | Sets a limit on `ExUnits` budget. All ExUnits values of all submitted
-- | transactions are combined. Transactions that are constructed, but not
-- | submitted, are not considered.
-- | The execution of the `Contract` will not be interrupted in case the
-- | `ExUnits` limit is reached.
checkExUnitsNotExceed
  :: forall (a :: Type)
   . ExUnits
  -> ContractCheck a
checkExUnitsNotExceed maxExUnits contract = do
  (ref :: Ref ExUnits) <- liftEffect $ Ref.new $ ExUnits
    { mem: BigNum.zero, steps: BigNum.zero }
  let
    submitHook :: Transaction -> Effect Unit
    submitHook tx = do
      let
        (newExUnits :: ExUnits) =
          unsafePartial
            $ foldr append (ExUnits { mem: BigNum.zero, steps: BigNum.zero })
            $ tx
                ^.
                  _witnessSet
                    <<< _redeemers
                    <<< to (map (unwrap >>> _.exUnits))
      Ref.modify_ (unsafePartial $ append newExUnits) ref

    setSubmitHook :: ContractEnv -> ContractEnv
    setSubmitHook =
      prop (Proxy :: Proxy "hooks") <<< prop (Proxy :: Proxy "onSubmit")
        -- Extend a hook action if it exists, or set it to `Just submitHook`
        %~ maybe (Just submitHook)
          \oldHook -> Just \tx -> do
            -- ignore possible exception from the old hook
            void $ try $ oldHook tx
            submitHook tx

    finalize :: ContractAssertion Unit
    finalize = do
      exUnits <- liftEffect $ Ref.read ref
      assertContract (MaxExUnitsExceeded (ExpectedActual maxExUnits exUnits))
        ( (unwrap maxExUnits).mem >= (unwrap exUnits).mem &&
            (unwrap maxExUnits).steps >= (unwrap exUnits).steps
        )

  pure (mapReaderT (local setSubmitHook) contract /\ finalize)

-------------- Values at address ------------------------------------------------

-- | Arguments are:
-- |
-- | - a labeled address
-- | - a callback that implements the assertion, accepting `Contract` execution
-- |   result, and values (before and after). The value may not be computed due
-- |   to an exception, hence it's wrapped in `Maybe`.
checkValueDeltaAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (Maybe a -> Value -> Value -> ContractAssertion Unit)
  -> ContractCheck a
checkValueDeltaAtAddress addr check contract = do
  valueBefore <- getValueAtAddress addr
  ref <- liftEffect $ Ref.new Nothing
  let
    finalize = do
      valueAfter <- getValueAtAddress addr
      liftEffect (Ref.read ref) >>= \res -> check res valueBefore valueAfter
    run = do
      res <- contract
      liftEffect $ Ref.write (Just res) ref
      pure res
  pure (run /\ finalize)

checkLovelaceDeltaAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (Maybe a -> Contract BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> ContractCheck a
checkLovelaceDeltaAtAddress addr getExpected comp contract = do
  checkValueDeltaAtAddress addr check contract
  where
  check :: Maybe a -> Value -> Value -> ContractAssertion Unit
  check result valueBefore valueAfter = do
    lift (E.try $ getExpected result) >>= either
      (tellFailure <<< FailedToGetExpectedValue <<< message)
      \expected -> do
        let
          actual :: BigInt
          actual = BigNum.toBigInt (unwrap (Value.getCoin valueAfter)) -
            BigNum.toBigInt (unwrap $ Value.getCoin valueBefore)

          unexpectedLovelaceDelta :: ContractAssertionFailure
          unexpectedLovelaceDelta =
            UnexpectedLovelaceDelta (Just addr) (ExpectedActual expected actual)

        assertContract unexpectedLovelaceDelta (comp actual expected)

-- | Requires that the computed amount of lovelace was gained at the address
-- | by calling the contract.
checkGainAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (Maybe a -> Contract BigInt)
  -> ContractCheck a
checkGainAtAddress addr getMinGain =
  checkLovelaceDeltaAtAddress addr getMinGain eq

-- | Requires that the passed amount of lovelace was gained at the address
-- | by calling the contract.
checkGainAtAddress'
  :: forall (a :: Type)
   . Labeled Address
  -> BigInt
  -> ContractCheck a
checkGainAtAddress' addr minGain =
  checkGainAtAddress addr (const $ pure minGain)

-- | Requires that the computed amount of lovelace was lost at the address
-- | by calling the contract.
checkLossAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (Maybe a -> Contract BigInt)
  -> ContractCheck a
checkLossAtAddress addr getMinLoss =
  checkLovelaceDeltaAtAddress addr (map negate <<< getMinLoss) eq

-- | Requires that the passed amount of lovelace was lost at the address
-- | by calling the contract.
checkLossAtAddress'
  :: forall (a :: Type)
   . Labeled Address
  -> BigInt
  -> ContractCheck a
checkLossAtAddress' addr minLoss =
  checkLossAtAddress addr (const $ pure minLoss)

checkTokenDeltaAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (ScriptHash /\ AssetName)
  -> (Maybe a -> Contract BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> ContractCheck a
checkTokenDeltaAtAddress addr (cs /\ tn) getExpected comp contract =
  checkValueDeltaAtAddress addr check contract
  where
  check :: Maybe a -> Value -> Value -> ContractAssertion Unit
  check result valueBefore valueAfter = do
    lift (E.try $ getExpected result) >>= either
      (tellFailure <<< FailedToGetExpectedValue <<< message)
      \expected -> do
        let
          actual :: BigInt
          actual =
            (BigNum.toBigInt $ Value.valueOf (Asset cs tn) valueAfter) -
              (BigNum.toBigInt $ Value.valueOf (Asset cs tn) valueBefore)

          unexpectedTokenDelta :: ContractAssertionFailure
          unexpectedTokenDelta =
            UnexpectedTokenDelta (Just addr) tn (ExpectedActual expected actual)

        assertContract unexpectedTokenDelta (comp actual expected)

-- | Requires that the computed number of tokens was gained at the address
-- | by calling the contract.
checkTokenGainAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (ScriptHash /\ AssetName)
  -> (Maybe a -> Contract BigInt)
  -> ContractCheck a
checkTokenGainAtAddress addr token getMinGain =
  checkTokenDeltaAtAddress addr token getMinGain eq

-- | Requires that the passed number of tokens was gained at the address
-- | by calling the contract.
checkTokenGainAtAddress'
  :: forall (a :: Type)
   . Labeled Address
  -> (ScriptHash /\ AssetName /\ BigInt)
  -> ContractCheck a
checkTokenGainAtAddress' addr (cs /\ tn /\ minGain) =
  checkTokenGainAtAddress addr (cs /\ tn) (const $ pure minGain)

-- | Requires that the computed number of tokens was lost at the address
-- | by calling the contract.
checkTokenLossAtAddress
  :: forall (a :: Type)
   . Labeled Address
  -> (ScriptHash /\ AssetName)
  -> (Maybe a -> Contract BigInt)
  -> ContractCheck a
checkTokenLossAtAddress addr token getMinLoss =
  checkTokenDeltaAtAddress addr token (map negate <<< getMinLoss) eq

-- | Requires that the passed number of tokens was lost at the address
-- | by calling the contract.
checkTokenLossAtAddress'
  :: forall (a :: Type)
   . Labeled Address
  -> (ScriptHash /\ AssetName /\ BigInt)
  -> ContractCheck a
checkTokenLossAtAddress' addr (cs /\ tn /\ minLoss) =
  checkTokenLossAtAddress addr (cs /\ tn) (const $ pure minLoss)

------------------- Values in wallet --------------------------------------------

-- | Accepts a callback that implements the assetion. The result value of type
-- | `a` may not be computed due to an exception, hence it's wrapped in `Maybe`.
checkValueDeltaInWallet
  :: forall (a :: Type)
   . (Maybe a -> Value -> Value -> ContractAssertion Unit)
  -> ContractCheck a
checkValueDeltaInWallet check contract = do
  valueBefore <- lift $ unsafePartial $ getWalletBalance <#> fold
  ref <- liftEffect $ Ref.new Nothing
  let
    finalize = do
      valueAfter <- unsafePartial $ lift $ getWalletBalance <#> fold
      liftEffect (Ref.read ref) >>= \res -> check res valueBefore valueAfter
    run = do
      res <- contract
      liftEffect $ Ref.write (Just res) ref
      pure res
  pure (run /\ finalize)

checkLovelaceDeltaInWallet
  :: forall (a :: Type)
   . (Maybe a -> Contract BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> ContractCheck a
checkLovelaceDeltaInWallet getExpected comp contract = do
  checkValueDeltaInWallet check contract
  where
  check :: Maybe a -> Value -> Value -> ContractAssertion Unit
  check result valueBefore valueAfter = do
    lift (E.try $ getExpected result) >>= either
      (tellFailure <<< FailedToGetExpectedValue <<< message)
      \expected -> do
        let
          actual :: BigInt
          actual = BigNum.toBigInt (unwrap (Value.getCoin valueAfter)) -
            BigNum.toBigInt (unwrap (Value.getCoin valueBefore))

          unexpectedLovelaceDelta :: ContractAssertionFailure
          unexpectedLovelaceDelta =
            UnexpectedLovelaceDelta Nothing (ExpectedActual expected actual)

        assertContract unexpectedLovelaceDelta (comp actual expected)

-- | Requires that the computed amount of lovelace was gained in the wallet
-- | by calling the contract.
checkGainInWallet
  :: forall (a :: Type)
   . (Maybe a -> Contract BigInt)
  -> ContractCheck a
checkGainInWallet getMinGain =
  checkLovelaceDeltaInWallet getMinGain eq

-- | Requires that the passed amount of lovelace was gained in the wallet
-- | by calling the contract.
checkGainInWallet'
  :: forall (a :: Type)
   . BigInt
  -> ContractCheck a
checkGainInWallet' minGain =
  checkGainInWallet (const $ pure minGain)

-- | Requires that the computed amount of lovelace was lost in the wallet
-- | by calling the contract.
checkLossInWallet
  :: forall (a :: Type)
   . (Maybe a -> Contract BigInt)
  -> ContractCheck a
checkLossInWallet getMinLoss =
  checkLovelaceDeltaInWallet (map negate <<< getMinLoss) eq

-- | Requires that the passed amount of lovelace was lost in the wallet
-- | by calling the contract.
checkLossInWallet'
  :: forall (a :: Type)
   . BigInt
  -> ContractCheck a
checkLossInWallet' minLoss =
  checkLossInWallet (const $ pure minLoss)

checkTokenDeltaInWallet
  :: forall (a :: Type)
   . (ScriptHash /\ AssetName)
  -> (Maybe a -> Contract BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> ContractCheck a
checkTokenDeltaInWallet (cs /\ tn) getExpected comp contract =
  checkValueDeltaInWallet check contract
  where
  check :: Maybe a -> Value -> Value -> ContractAssertion Unit
  check result valueBefore valueAfter = do
    lift (E.try $ getExpected result) >>= either
      (tellFailure <<< FailedToGetExpectedValue <<< message)
      \expected -> do
        let
          actual :: BigInt
          actual = BigNum.toBigInt (Value.valueOf (Asset cs tn) valueAfter) -
            BigNum.toBigInt (Value.valueOf (Asset cs tn) valueBefore)

          unexpectedTokenDelta :: ContractAssertionFailure
          unexpectedTokenDelta =
            UnexpectedTokenDelta Nothing tn (ExpectedActual expected actual)

        assertContract unexpectedTokenDelta (comp actual expected)

-- | Requires that the computed number of tokens was gained in the wallet
-- | by calling the contract.
checkTokenGainInWallet
  :: forall (a :: Type)
   . (ScriptHash /\ AssetName)
  -> (Maybe a -> Contract BigInt)
  -> ContractCheck a
checkTokenGainInWallet token getMinGain =
  checkTokenDeltaInWallet token getMinGain eq

-- | Requires that the passed number of tokens was gained in the wallet
-- | by calling the contract.
checkTokenGainInWallet'
  :: forall (a :: Type)
   . (ScriptHash /\ AssetName /\ BigInt)
  -> ContractCheck a
checkTokenGainInWallet' (cs /\ tn /\ minGain) =
  checkTokenGainInWallet (cs /\ tn) (const $ pure minGain)

-- | Requires that the computed number of tokens was lost in the wallet
-- | by calling the contract.
checkTokenLossInWallet
  :: forall (a :: Type)
   . (ScriptHash /\ AssetName)
  -> (Maybe a -> Contract BigInt)
  -> ContractCheck a
checkTokenLossInWallet token getMinLoss =
  checkTokenDeltaInWallet token (map negate <<< getMinLoss) eq

-- | Requires that the passed number of tokens was lost in the wallet
-- | by calling the contract.
checkTokenLossInWallet'
  :: forall (a :: Type)
   . (ScriptHash /\ AssetName /\ BigInt)
  -> ContractCheck a
checkTokenLossInWallet' (cs /\ tn /\ minLoss) =
  checkTokenLossInWallet (cs /\ tn) (const $ pure minLoss)

--------------------- Datums and scripts ----------------------------------------

-- | Requires that the transaction output contains the specified datum,
-- | datum hash or does not contain anything.
assertOutputHasDatum
  :: Maybe OutputDatum
  -> Labeled TransactionOutput
  -> ContractAssertion Unit
assertOutputHasDatum expectedDatum txOutput = do
  let actualDatum = unlabel txOutput ^. _datum
  assertContractExpectedActual (UnexpectedDatumInOutput txOutput)
    expectedDatum
    actualDatum

-- | Requires that the transaction output contains the specified reference
-- | script.
assertOutputHasRefScript
  :: ScriptRef
  -> Labeled TransactionOutput
  -> ContractAssertion Unit
assertOutputHasRefScript expectedRefScript txOutput = do
  let actualRefScript = unlabel txOutput ^. _scriptRef
  assertContractExpectedActual (UnexpectedRefScriptInOutput txOutput)
    (Just expectedRefScript)
    actualRefScript

------------------------- Metadata ---------------------------------------------

assertTxHasMetadata
  :: forall (metadata :: Type) (a :: Type)
   . MetadataType metadata
  => Eq metadata
  => Show metadata
  => Label
  -> TransactionHash
  -> metadata
  -> ContractAssertion Unit
assertTxHasMetadata mdLabel txHash expectedMetadata = do
  generalMetadata <-
    assertContractMaybe (TransactionHasNoMetadata txHash Nothing)
      =<< lift
        ( map ((=<<) (_.metadata <<< unwrap)) $ hush <$> getTxAuxiliaryData
            txHash
        )

  rawMetadata <-
    assertContractMaybe (TransactionHasNoMetadata txHash (Just mdLabel))
      ( Map.lookup (unwrap $ metadataLabel (Proxy :: Proxy metadata))
          (unwrap generalMetadata)
      )

  (metadata :: metadata) <-
    assertContractMaybe (CouldNotParseMetadata mdLabel)
      (fromMetadata rawMetadata)

  let expectedActual = show <$> ExpectedActual expectedMetadata metadata
  assertContract (UnexpectedMetadataValue mdLabel expectedActual)
    (metadata == expectedMetadata)

-------------------------- Utils -----------------------------------------------

getValueAtAddress
  :: Labeled Address
  -> ContractAssertion Value
getValueAtAddress = map (unsafePartial $ foldMap (view (_amount))) <<< lift
  <<< utxosAt
  <<< unlabel

tellFailure
  :: ContractAssertionFailure -> ContractAssertion Unit
tellFailure failure = do
  ask >>= liftEffect <<< Ref.modify_ (Cons failure)

tieRef
  :: forall (a :: Type)
   . ContractAssertion
       ((a -> ContractAssertion a) /\ ContractAssertion (Maybe a))
tieRef = do
  ref <- liftEffect $ Ref.new Nothing
  let
    putResult result = do
      liftEffect $ Ref.write (Just result) ref
      pure result
    getResult = liftEffect (Ref.read ref)
  pure (putResult /\ getResult)
