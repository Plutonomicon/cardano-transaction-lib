module BalanceTx
  ( Actual(Actual)
  , AddSignatoriesFailure(AddSignatoriesFailure)
  , AddSignatoriesFailureReason(SigningAddressNotFound)
  , AddTxCollateralsFailure(AddTxCollateralsFailure)
  , AddTxCollateralsFailureReason(CollateralUtxosUnavailable)
  , BalanceNonAdaOutsFailure(BalanceNonAdaOutsFailure)
  , BalanceNonAdaOutsFailureReason(InputsCannotBalanceNonAdaTokens)
  , BalanceTxFailure
      ( AddSignatoriesFailure'
      , AddTxCollateralsFailure'
      , BalanceNonAdaOutsFailure'
      , BalanceTxInsFailure'
      , BalanceTxMFailure'
      , BuildTxRawFailure'
      , CalculateMinFeeFailure'
      , ReturnAdaChangeFailure'
      , ToEitherTransactionInputFailure'
      , UtxosAtFailure'
      )
  , BalanceTxInsFailure(BalanceTxInsFailure)
  , BalanceTxInsFailureReason(InsufficientTxInputs)
  , BalanceTxMFailure(BalanceTxMFailure)
  , BalanceTxMFailureReason(UnknownRequiredSigners)
  , BuildTxRawFailure(BuildTxRawFailure)
  , BuildTxRawFailureReason(CannotBuildTxRaw)
  , CalculateMinFeeFailure(CalculateMinFeeFailure)
  , CalculateMinFeeFailureReason(CannotCalculateMinFee)
  , Expected(Expected)
  , Impossible(Impossible)
  , ReturnAdaChangeFailure(ReturnAdaChangeFailure)
  , ReturnAdaChangeFailureReason
      ( CouldNotModifyUtxo
      , CouldNotModifyUtxoHead
      , InputAdaDoesNotCoverSingleAdaOutput
      , NotEnoughAdaInputAfterPrebalance
      , ReturnAdaChangeBuildTxRaw
      , ReturnAdaChangeCalculateMinFee
      )
  , ToEitherTransactionInputFailure(ToEitherTransactionInputFailure)
  , ToEitherTransactionInputFailureReason(CannotConvertScriptOutputToTxInput)
  , UnbalancedTransaction(UnbalancedTransaction)
  , UtxosAtFailure(UtxosAtFailure)
  , UtxosAtFailureReason(CouldNotGetUtxos)
  , balanceTxM -- Transaction balancer function
  ) where

import Prelude
import Data.Array ((\\), findIndex, modifyAt)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt, quot)
import Data.Either (Either(Left, Right), hush, note)
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.List ((:), List(Nil), partition)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe, Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Undefined (undefined)

import Ogmios (QueryM, utxosAt')
import ProtocolParametersAlonzo
  ( coinSize
  , lovelacePerUTxOWord
  , pidSize
  , protocolParamUTxOCostPerWord
  , utxoEntrySizeWithoutVal
  )
import Types.Ada (fromValue, getLovelace, lovelaceValueOf)
import Types.ByteArray (byteLength)
import Types.Transaction
  ( Address
  , DataHash
  , PaymentCredential(PaymentCredentialKey, PaymentCredentialScript)
  , RequiredSigner
  , Transaction(Transaction)
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  , Utxo
  , UtxoM
  )
import Types.Value
  ( allTokenNames
  , flattenValue
  , geq
  , isAdaOnly
  , isPos
  , isZero
  , minus
  , numCurrencySymbols
  , numTokenNames
  , TokenName
  , Value(Value)
  )

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs

--------------------------------------------------------------------------------
-- Errors for Balancing functions
--------------------------------------------------------------------------------
-- These derivations may need tweaking when testing to make sure they are easy
-- to read, especially with generic show vs newtype show derivations.
data BalanceTxFailure
  = UtxosAtFailure' UtxosAtFailure
  | BalanceTxMFailure' BalanceTxMFailure
  | ReturnAdaChangeFailure' ReturnAdaChangeFailure
  | AddTxCollateralsFailure' AddTxCollateralsFailure
  | ToEitherTransactionInputFailure' ToEitherTransactionInputFailure
  | BalanceTxInsFailure' BalanceTxInsFailure
  | BalanceNonAdaOutsFailure' BalanceNonAdaOutsFailure
  | AddSignatoriesFailure' AddSignatoriesFailure
  | CalculateMinFeeFailure' CalculateMinFeeFailure
  | BuildTxRawFailure' BuildTxRawFailure

derive instance genericBalanceTxFailure :: Generic BalanceTxFailure _

instance showBalanceTxFailure :: Show BalanceTxFailure where
  show = genericShow

newtype UtxosAtFailure = UtxosAtFailure UtxosAtFailureReason

derive instance newtypeUtxosAtFailure :: Newtype UtxosAtFailure _
derive newtype instance showUtxosAtFailure :: Show UtxosAtFailure

data UtxosAtFailureReason = CouldNotGetUtxos

instance showUtxosAtFailureReason :: Show UtxosAtFailureReason where
  show CouldNotGetUtxos = "Couldn't get utxos at address."

newtype BalanceTxMFailure = BalanceTxMFailure BalanceTxMFailureReason

derive instance newtypeBalanceTxMFailure :: Newtype BalanceTxMFailure _
derive newtype instance showBalanceTxMFailure :: Show BalanceTxMFailure

data BalanceTxMFailureReason = UnknownRequiredSigners

instance showBalanceTxMFailureReason :: Show BalanceTxMFailureReason where
  show UnknownRequiredSigners = "Unknown required signers."

newtype ReturnAdaChangeFailure = ReturnAdaChangeFailure ReturnAdaChangeFailureReason

derive instance newtypeReturnAdaChangeFailure :: Newtype ReturnAdaChangeFailure _

derive newtype instance showReturnAdaChangeFailure :: Show ReturnAdaChangeFailure

data ReturnAdaChangeFailureReason
  = NotEnoughAdaInputAfterPrebalance Impossible
  | CouldNotModifyUtxo
  | CouldNotModifyUtxoHead Impossible
  | InputAdaDoesNotCoverSingleAdaOutput
  | ReturnAdaChangeCalculateMinFee CalculateMinFeeFailureReason
  | ReturnAdaChangeBuildTxRaw BuildTxRawFailureReason

instance showReturnAdaChangeFailureReason :: Show ReturnAdaChangeFailureReason where
  show (NotEnoughAdaInputAfterPrebalance Impossible) =
    "Not enough Input Ada to cover output and fees after prebalance - this \
    \should be **IMPOSSIBLE** if called after loop."
  show CouldNotModifyUtxo =
    "Couldn't modify utxo to return change."
  show (CouldNotModifyUtxoHead Impossible) =
    "Couldn't modify head utxo to add Ada - this should be **IMPOSSIBLE**."
  show InputAdaDoesNotCoverSingleAdaOutput =
    "ReturnAda' does not cover min. utxo requirement for single Ada-only \
    \output."
  show (ReturnAdaChangeCalculateMinFee reason) = show reason
  show (ReturnAdaChangeBuildTxRaw reason) = show reason

newtype AddTxCollateralsFailure = AddTxCollateralsFailure AddTxCollateralsFailureReason

derive instance newtypeAddTxCollateralsFailure :: Newtype AddTxCollateralsFailure _

derive newtype instance showAddTxCollateralsFailure :: Show AddTxCollateralsFailure

data AddTxCollateralsFailureReason = CollateralUtxosUnavailable

instance showAddTxCollateralsFailureReason :: Show AddTxCollateralsFailureReason where
  show CollateralUtxosUnavailable =
    "There are no utxos to be used as collateral."

newtype ToEitherTransactionInputFailure = ToEitherTransactionInputFailure ToEitherTransactionInputFailureReason

derive instance newtypeToEitherTransactionInputFailure :: Newtype ToEitherTransactionInputFailure _

derive newtype instance showToEitherTransactionInputFailure :: Show ToEitherTransactionInputFailure

data ToEitherTransactionInputFailureReason = CannotConvertScriptOutputToTxInput

instance showToEitherTransactionInputFailureReason :: Show ToEitherTransactionInputFailureReason where
  show CannotConvertScriptOutputToTxInput =
    "Cannot convert a script output to TransactionInput"

newtype BalanceTxInsFailure = BalanceTxInsFailure BalanceTxInsFailureReason

derive instance newtypeBalanceTxInsFailure :: Newtype BalanceTxInsFailure _
derive newtype instance showBalanceTxInsFailure :: Show BalanceTxInsFailure

data BalanceTxInsFailureReason = InsufficientTxInputs Expected Actual

instance showBalanceTxInsFailureReason :: Show BalanceTxInsFailureReason where
  show (InsufficientTxInputs expected actual) =
    "Insufficient tx inputs, needed: "
      <> show expected
      <> ", got: "
      <> show actual

newtype CollectTxInsFailure = CollectTxInsFailure CollectTxInsFailureReason

derive instance newtypeCollectTxInsFailure :: Newtype CollectTxInsFailure _
derive newtype instance showCollectTxInsFailure :: Show CollectTxInsFailure

data CollectTxInsFailureReason = CollectTxInsInsufficientTxInputs BalanceTxInsFailureReason

instance showCollectTxInsFailureReason :: Show CollectTxInsFailureReason where
  show (CollectTxInsInsufficientTxInputs reason) = show reason

newtype Expected = Expected Value

derive instance newtypeExpected :: Newtype Expected _

instance showExpected :: Show Expected where
  show = show <<< flattenValue <<< unwrap

newtype Actual = Actual Value

derive instance newtypeActual :: Newtype Actual _

instance showActual :: Show Actual where
  show = show <<< flattenValue <<< unwrap

newtype BalanceNonAdaOutsFailure = BalanceNonAdaOutsFailure BalanceNonAdaOutsFailureReason

derive instance newtypeBalanceNonAdaOutsFailure :: Newtype BalanceNonAdaOutsFailure _

derive newtype instance showBalanceNonAdaOutsFailure :: Show BalanceNonAdaOutsFailure

data BalanceNonAdaOutsFailureReason = InputsCannotBalanceNonAdaTokens

instance showBalanceNonAdaOutsFailureReason :: Show BalanceNonAdaOutsFailureReason where
  show InputsCannotBalanceNonAdaTokens = "Not enough inputs to balance tokens."

newtype AddSignatoriesFailure = AddSignatoriesFailure AddSignatoriesFailureReason

derive instance newtypeAddSignatoriesFailure :: Newtype AddSignatoriesFailure _
derive newtype instance showAddSignatoriesFailure :: Show AddSignatoriesFailure

data AddSignatoriesFailureReason = SigningAddressNotFound Address

instance showAddSignatoriesFailureReason :: Show AddSignatoriesFailureReason where
  show (SigningAddressNotFound address) =
    "Signing address not found for: " <> show address

-- | Represents that an error reason should be impossible
data Impossible = Impossible

newtype BuildTxRawFailure = BuildTxRawFailure BuildTxRawFailureReason

derive instance newtypeBuildTxRawFailure :: Newtype BuildTxRawFailure _
derive newtype instance showBuildTxRawFailure :: Show BuildTxRawFailure

data BuildTxRawFailureReason = CannotBuildTxRaw

instance showBuildTxRawFailureReason :: Show BuildTxRawFailureReason where
  show CannotBuildTxRaw = "Cannot build raw transaction."

newtype CalculateMinFeeFailure = CalculateMinFeeFailure CalculateMinFeeFailureReason

derive instance newtypeCalculateMinFeeFailure :: Newtype CalculateMinFeeFailure _

derive newtype instance showCalculateMinFeeFailure :: Show CalculateMinFeeFailure

data CalculateMinFeeFailureReason = CannotCalculateMinFee

instance showCalculateMinFeeFailureReason :: Show CalculateMinFeeFailureReason where
  show CannotCalculateMinFee = "Cannot calculate minimum fee."

--------------------------------------------------------------------------------
-- Type aliases, temporary placeholder types and functions
--------------------------------------------------------------------------------
-- Output utxos with the amount of lovelaces required.
type MinUtxos = Array (TransactionOutput /\ BigInt)

newtype PubKeyHash = PubKeyHash String
newtype PrivateKey = PrivateKey String

-- getPkhFromAddress :: Address -> PubKeyHash
-- getPkhFromAddress = undefined

-- TO DO: find a way to get private keys associated to each address/pkh for
-- signing purposes. This can be replaced by Nami signing functionality.
getPrivKeys :: QueryM (Map.Map Address PrivateKey)
getPrivKeys = undefined

-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/Ledger-Constraints-OffChain.html#t:UnbalancedTx
-- to demonstrate what is required. This is temporary to make the compiler happy.
-- I haven't copied it exactly, instead just a minimal version given our
-- incomplete Transaction.purs types. Also requiredSignatures is already in
-- TxBody. I've ignored validity interval too.
newtype UnbalancedTransaction = UnbalancedTransaction
  { unbalancedTx :: Transaction
  , utxoIndex :: UtxoM
  -- utxoIndex will include utxos for scripts as well presumably.
  }

-- Some functionality that builds a Transaction from a TxBody without balancing
-- https://github.com/input-output-hk/cardano-node/blob/b6ca519f97a0e795611a63174687e6bb70c9f752/cardano-cli/src/Cardano/CLI/Shelley/Run/Transaction.hs#L312
-- Is this relevant? Can probably ignore this.
buildTxRaw :: TxBody -> Either BuildTxRawFailure Transaction
buildTxRaw = undefined

-- There is an estimate function here:
-- https://input-output-hk.github.io/cardano-node/cardano-api/src/Cardano.Api.Fees.html#estimateTransactionFee
-- Otherwise, we'd need a Haskell server to run the "exact" version, since we
-- can't run the Plutus interpreter via Purescript.
calculateMinFee :: Transaction -> Either CalculateMinFeeFailure BigInt
calculateMinFee = undefined

-- TO DO: We need proper signing functionality. Do we need it to sign a TxBody
-- or Transaction?
signBy :: TxBody -> PrivateKey -> TxBody
signBy = undefined

--------------------------------------------------------------------------------
-- Balancing functions and helpers
--------------------------------------------------------------------------------
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L54
-- | Balances an unbalanced transaction
balanceTxM
  :: Address
  -> UnbalancedTransaction
  -> QueryM (Either BalanceTxFailure Transaction)

balanceTxM ownAddr (UnbalancedTransaction { unbalancedTx, utxoIndex }) = do
  utxos' :: Either BalanceTxFailure Utxo <-
    utxosAt' ownAddr <#>
      note (UtxosAtFailure' $ wrap CouldNotGetUtxos) >>> map unwrap
  case utxos' of
    Left err -> pure $ Left err
    Right utxos -> do
      privKeys :: Map.Map Address PrivateKey <- getPrivKeys
      let -- Combines utxos at the user address and those from any scripts involved
          -- with the contract.
          utxoIndex' :: Utxo
          utxoIndex' = utxos `Map.union` unwrap utxoIndex

      case (unwrap (unwrap unbalancedTx).body).required_signers of
        Nothing -> pure $ Left $ BalanceTxMFailure' $ wrap UnknownRequiredSigners
        Just requiredSigners -> do
          prebalancedTx' :: Either BalanceTxFailure Transaction <-
            loop utxoIndex' ownAddr privKeys requiredSigners [] unbalancedTx
          pure do
            prebalancedTx :: Transaction <- prebalancedTx'
            lmap ReturnAdaChangeFailure' $
              returnAdaChange ownAddr utxoIndex' prebalancedTx
    where
    loop ::
      Utxo ->
      Address ->
      Map.Map Address PrivateKey ->
      Array RequiredSigner ->
      MinUtxos ->
      Transaction ->
      QueryM (Either BalanceTxFailure Transaction)
    loop
      utxoIndex'
      ownAddr'
      privKeys'
      requiredSigners'
      prevMinUtxos'
      (Transaction tx'@{ body: txBody'@(TxBody txB) }) = do
      let nextMinUtxos' :: MinUtxos
          nextMinUtxos' =
            calculateMinUtxos $ txB.outputs \\ map fst prevMinUtxos'

          minUtxos' :: MinUtxos
          minUtxos' = prevMinUtxos' <> nextMinUtxos'

          balancedTxBody' :: Either BalanceTxFailure TxBody
          balancedTxBody' =
            chainedBalancer
              minUtxos'
              utxoIndex'
              ownAddr'
              privKeys'
              requiredSigners'
              txBody'

      case balancedTxBody' of
        Left err -> pure $ Left err
        Right balancedTxBody'' ->
          if txBody' == balancedTxBody''
           then pure $ Right $ wrap tx' { body = balancedTxBody'' }
           else
            loop
              utxoIndex'
              ownAddr'
              privKeys'
              requiredSigners'
              minUtxos'
              $ wrap tx' { body = balancedTxBody'' }

    chainedBalancer
      :: MinUtxos
      -> Utxo
      -> Address
      -> Map.Map Address PrivateKey
      -> Array RequiredSigner
      -> TxBody
      -> Either BalanceTxFailure TxBody
    chainedBalancer
      minUtxos' utxoIndex' ownAddr' privKeys' requiredSigners' txBody' = do
        txBodyWithoutFees' :: TxBody <-
          preBalanceTxBody
            minUtxos'
            zero
            utxoIndex'
            ownAddr'
            privKeys'
            requiredSigners'
            txBody'
        tx' :: Transaction <-
          lmap BuildTxRawFailure' $ buildTxRaw txBodyWithoutFees'
        fees' :: BigInt <- lmap CalculateMinFeeFailure' $ calculateMinFee tx' -- FIX ME: use txBodyWithoutFees replaced in original tx.
        preBalanceTxBody
          minUtxos'
          utxoIndex'
          ownAddr'
          privKeys'
          requiredSigners'
          txBody'

    case balancedTxBody' of
      Left err -> pure $ Left err
      Right balancedTxBody'' ->
        if txBody' == balancedTxBody'' then pure $ Right $ wrap tx' { body = balancedTxBody'' }
        else
          loop
            utxoIndex'
            ownAddr'
            privKeys'
            requiredSigners'
            minUtxos'
            $ wrap tx' { body = balancedTxBody'' }

  chainedBalancer
    :: MinUtxos
    -> Utxo
    -> Address
    -> Map.Map Address PrivateKey
    -> Array RequiredSigner
    -> TxBody
    -> Either BalanceTxFailure TxBody
  chainedBalancer
    minUtxos'
    utxoIndex'
    ownAddr'
    privKeys'
    requiredSigners'
    txBody' = do
    txBodyWithoutFees' :: TxBody <-
      preBalanceTxBody
        minUtxos'
        zero
        utxoIndex'
        ownAddr'
        privKeys'
        requiredSigners'
        txBody'
    tx' :: Transaction <-
      lmap BuildTxRawFailure' $ buildTxRaw txBodyWithoutFees'
    fees' :: BigInt <- lmap CalculateMinFeeFailure' $ calculateMinFee tx' -- FIX ME: use txBodyWithoutFees replaced in original tx.
    preBalanceTxBody
      minUtxos'
      (fees' + fromInt 500000) -- FIX ME: Add 0.5 Ada to ensure enough input for later on in final balancing.
      utxoIndex'
      ownAddr'
      privKeys'
      requiredSigners'
      txBody'

-- Transaction should be prebalanced at this point with all excess with Ada
-- where the Ada value of inputs is greater or equal to value of outputs.
-- Also add fees to txBody. This should be called with a Tx with min
-- Ada in each output utxo, namely, after "loop".
returnAdaChange
  :: Address
  -> Utxo
  -> Transaction
  -> Either ReturnAdaChangeFailure Transaction
returnAdaChange changeAddr utxos (Transaction tx@{ body: TxBody txBody }) = do
  fees :: BigInt <-
    lmap
      ( \(CalculateMinFeeFailure err) -> wrap $ ReturnAdaChangeCalculateMinFee err
      )
      $ calculateMinFee
      $ wrap tx
  let
    txOutputs :: Array TransactionOutput
    txOutputs = txBody.outputs

    inputValue :: Value
    inputValue = getInputValue utxos (wrap txBody)

    inputAda :: BigInt
    inputAda = getLovelace $ fromValue inputValue

    -- FIX ME, ignore mint value?
    outputValue :: Value
    outputValue = Array.foldMap getAmount txOutputs

    outputAda :: BigInt
    outputAda = getLovelace $ fromValue outputValue

    returnAda :: BigInt
    returnAda = inputAda - outputAda - fees
  case compare returnAda zero of
    LT -> Left $ wrap $ NotEnoughAdaInputAfterPrebalance Impossible
    EQ -> pure $ wrap tx { body = wrap txBody { fee = wrap fees } }
    GT -> do
      -- Short circuits and adds Ada to any output utxo of the owner. This saves
      -- on fees but does not create a separate utxo. Do we want this behaviour?
      -- I expect if there are any output utxos to the user, they are either Ada
      -- only or non-Ada with minimum Ada value. Either way, we can just add the
      -- the value and it shouldn't incur extra fees.
      -- If we do require a new utxo, then we must add fees, under the assumption
      -- we have enough Ada in the input at this stage, otherwise we fail because
      -- we don't want to loop again over the addition of one output utxo.
      let
        changeIndex :: Maybe Int
        changeIndex =
          findIndex ((==) changeAddr <<< _.address <<< unwrap) txOutputs

      case changeIndex of
        Just idx -> do
          -- Add the Ada value to the first output utxo of the owner to not
          -- concur fees. This should be Ada only or non-Ada which has min Ada.
          newOutputs :: Array TransactionOutput <-
            note (wrap CouldNotModifyUtxo) $
              modifyAt
                idx
                ( \(TransactionOutput o@{ amount }) -> TransactionOutput
                    o { amount = amount <> lovelaceValueOf returnAda }
                )
                txOutputs
          -- Fees unchanged because we aren't adding a new utxo.
          pure $
            wrap
              tx { body = wrap txBody { outputs = newOutputs, fee = wrap fees } }
        Nothing -> do
          -- Create a txBody with the extra output utxo then recalculate fees,
          -- then adjust as necessary if we have sufficient Ada in the input.
          let
            utxoCost :: BigInt
            utxoCost = getLovelace protocolParamUTxOCostPerWord

            -- An ada-only UTxO entry is 29 words. More details about min utxo
            -- calculation can be found here:
            -- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0028#rationale-for-parameter-choices
            changeMinUtxo :: BigInt
            changeMinUtxo = (fromInt 29) * utxoCost

            txBody' :: TxBody
            txBody' =
              wrap
                txBody
                  { outputs =
                      wrap
                        { address: changeAddr
                        , amount: lovelaceValueOf returnAda
                        , data_hash: Nothing
                        }
                        `Array.cons` txBody.outputs
                  }
          tx' :: Transaction <-
            lmap
              ( \(BuildTxRawFailure err) -> wrap $ ReturnAdaChangeBuildTxRaw err
              )
              $ buildTxRaw txBody'
          fees' :: BigInt <-
            lmap
              ( \(CalculateMinFeeFailure err) -> wrap $ ReturnAdaChangeCalculateMinFee err
              )
              $ calculateMinFee tx' -- fees should increase.
          -- New return Ada amount should decrease:
          let
            returnAda' :: BigInt
            returnAda' = returnAda + fees - fees'

          if returnAda' >= changeMinUtxo then do
            newOutputs :: Array TransactionOutput <-
              note (wrap $ CouldNotModifyUtxoHead Impossible)
                $ modifyAt
                    0
                    ( \(TransactionOutput o) -> TransactionOutput
                        o { amount = lovelaceValueOf returnAda }
                    )
                $ _.outputs <<< unwrap
                $ txBody'
            pure $
              wrap
                tx { body = wrap txBody { outputs = newOutputs, fee = wrap fees } }
          else
            Left $ wrap InputAdaDoesNotCoverSingleAdaOutput

calculateMinUtxos :: Array TransactionOutput -> MinUtxos
calculateMinUtxos = map (\a -> a /\ calculateMinUtxo a)

-- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
-- https://github.com/input-output-hk/cardano-ledger/blob/master/doc/explanations/min-utxo-alonzo.rst
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0028#rationale-for-parameter-choices
-- | Given an array of transaction outputs, return the paired amount of lovelaces
-- | required by each utxo.
calculateMinUtxo :: TransactionOutput -> BigInt
calculateMinUtxo txOut = unwrap lovelacePerUTxOWord * utxoEntrySize txOut
  where
  -- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
  -- https://github.com/input-output-hk/cardano-ledger/blob/master/doc/explanations/min-utxo-alonzo.rst
  utxoEntrySize :: TransactionOutput -> BigInt
  utxoEntrySize (TransactionOutput txOut') =
    let
      outputValue :: Value
      outputValue = txOut'.amount
    in
      if isAdaOnly outputValue then utxoEntrySizeWithoutVal + coinSize -- 29 in Alonzo
      else utxoEntrySizeWithoutVal
        + size outputValue
        + dataHashSize txOut'.data_hash

-- https://github.com/input-output-hk/cardano-ledger/blob/master/doc/explanations/min-utxo-alonzo.rst
-- | Calculates how many words are needed depending on whether the datum is
-- | hashed or not. 10 words for a hashed datum and 0 for no hash. The argument
-- | to the function is the datum hash found in TransactionOutput.
dataHashSize :: Maybe DataHash -> BigInt -- Should we add type safety?
dataHashSize Nothing = zero
dataHashSize (Just _) = fromInt 10

-- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
-- See "size"
size :: Value -> BigInt
size v = fromInt 6 + roundupBytesToWords b
  where
  b :: BigInt
  b = numTokenNames v * fromInt 12
    + sumTokenNameLengths v
    + numCurrencySymbols v * pidSize

  -- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
  -- Converts bytes to 8-byte long words, rounding up
  roundupBytesToWords :: BigInt -> BigInt
  roundupBytesToWords b' = quot (b' + (fromInt 7)) $ fromInt 8

  -- https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html
  -- The formula is actually based on the length of the  bytestring
  --  representation - test this.
  -- | Sum of the length of the strings of distinct token names.
  sumTokenNameLengths :: Value -> BigInt
  sumTokenNameLengths = Foldable.foldl lenAdd zero <<< allTokenNames
    where
    lenAdd :: BigInt -> TokenName -> BigInt
    lenAdd = \c a -> c + (fromInt <<< byteLength <<< unwrap $ a)

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L116
preBalanceTxBody
  :: MinUtxos
  -> BigInt
  -> Utxo
  -> Address
  -> Map.Map Address PrivateKey
  -> Array RequiredSigner
  -> TxBody
  -> Either BalanceTxFailure TxBody
preBalanceTxBody minUtxos fees utxos ownAddr privKeys requiredSigners txBody =
  -- Take a single Ada only utxo collateral
  addTxCollaterals utxos txBody
    >>= balanceTxIns utxos fees -- Add input fees for the Ada only collateral
    >>= balanceNonAdaOuts ownAddr utxos
    >>= pure <<< addLovelaces minUtxos
    >>= balanceTxIns utxos fees -- Adding more inputs if required
    >>= balanceNonAdaOuts ownAddr utxos
    >>= addSignatories ownAddr privKeys requiredSigners

addTxCollaterals :: Utxo -> TxBody -> Either BalanceTxFailure TxBody
addTxCollaterals utxo txBody =
  addTxCollaterals' utxo txBody # lmap AddTxCollateralsFailure'

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L211
-- | Pick a collateral from the utxo map and add it to the unbalanced transaction
-- | (suboptimally we just pick a random utxo from the tx inputs). TO DO: upgrade
-- | to a better coin selection algorithm.
addTxCollaterals' :: Utxo -> TxBody -> Either AddTxCollateralsFailure TxBody
addTxCollaterals' utxos (TxBody txBody) = do
  let
    txIns :: Array TransactionInput
    txIns = utxosToTransactionInput $ filterAdaOnly utxos
  txIn :: TransactionInput <- findPubKeyTxIn txIns
  pure $ wrap txBody { collateral = Just (Array.singleton txIn) }
  where
  filterAdaOnly :: Utxo -> Utxo
  filterAdaOnly = Map.filter (isAdaOnly <<< getAmount)

  -- Take head for collateral - can use better coin selection here.
  findPubKeyTxIn
    :: Array TransactionInput
    -> Either AddTxCollateralsFailure TransactionInput
  findPubKeyTxIn =
    note (wrap CollateralUtxosUnavailable) <<< Array.head

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- | Get TransactionInput such that it is associated to PaymentCredentialKey
-- | and not PaymentCredentialScript, i.e. we want wallets only
getPublicKeyTransactionInput
  :: TransactionInput /\ TransactionOutput
  -> Either ToEitherTransactionInputFailure TransactionInput
getPublicKeyTransactionInput (txOutRef /\ txOut) =
  case txOutPaymentCredentials txOut of
    -- TEST ME: using PaymentCredentialKey to determine whether wallet or script
    PaymentCredentialKey _ ->
      pure txOutRef
    PaymentCredentialScript _ ->
      Left $ wrap CannotConvertScriptOutputToTxInput

addressPaymentCredentials :: Address -> PaymentCredential
addressPaymentCredentials = _.payment <<< unwrap <<< _."AddrType" <<< unwrap

txOutPaymentCredentials :: TransactionOutput -> PaymentCredential
txOutPaymentCredentials = addressPaymentCredentials <<< _.address <<< unwrap

balanceTxIns :: Utxo -> BigInt -> TxBody -> Either BalanceTxFailure TxBody
balanceTxIns utxos fees txbody =
  balanceTxIns' utxos fees txbody # lmap BalanceTxInsFailure'

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- Notice we aren't using protocol parameters for utxo cost per word.
balanceTxIns' :: Utxo -> BigInt -> TxBody -> Either BalanceTxInsFailure TxBody
balanceTxIns' utxos fees (TxBody txBody) = do
  let
    utxoCost :: BigInt
    utxoCost = getLovelace protocolParamUTxOCostPerWord

    -- An ada-only UTxO entry is 29 words. More details about min utxo
    -- calculation can be found here:
    -- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0028#rationale-for-parameter-choices
    changeMinUtxo :: BigInt
    changeMinUtxo = (fromInt 29) * utxoCost

    txOutputs :: Array TransactionOutput
    txOutputs = txBody.outputs

    nonMintedValue :: Value
    nonMintedValue =
      Array.foldMap getAmount txOutputs
        `minus` maybe mempty unwrap txBody.mint

    minSpending :: Value
    minSpending = lovelaceValueOf (fees + changeMinUtxo) <> nonMintedValue

  txIns :: Array TransactionInput <-
    lmap
      ( \( CollectTxInsFailure
             (CollectTxInsInsufficientTxInputs insufficientTxInputs)
         ) ->
          wrap insufficientTxInputs
      )
      $ collectTxIns txBody.inputs utxos minSpending
  -- Original code uses Set append which is union. Array unions behave
  -- a little differently as it removes duplicates in the second argument.
  -- but all inputs should be unique anyway so I think this is fine.
  pure $ wrap
    txBody
      { inputs = Array.union txIns txBody.inputs
      }

--https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- | Getting the necessary input utxos to cover the fees for the transaction
collectTxIns
  :: Array TransactionInput
  -> Utxo
  -> Value
  -> Either CollectTxInsFailure (Array TransactionInput)
collectTxIns originalTxIns utxos value =
  if isSufficient updatedInputs then pure updatedInputs
  else
    Left $ wrap $ CollectTxInsInsufficientTxInputs $
      InsufficientTxInputs (Expected value) (Actual $ txInsValue updatedInputs)
  where
  updatedInputs :: Array TransactionInput
  updatedInputs =
    Foldable.foldl
      ( \newTxIns txIn ->
          if isSufficient newTxIns then newTxIns
          else
            -- set insertion in original code.
            if txIn `Array.elem` newTxIns then newTxIns
          else txIn `Array.cons` newTxIns
      )
      originalTxIns
      $ utxosToTransactionInput utxos

  isSufficient :: Array TransactionInput -> Boolean
  isSufficient txIns' =
    not (Array.null txIns') && (txInsValue txIns') `geq` value

  txInsValue :: Array TransactionInput -> Value
  txInsValue =
    Array.foldMap getAmount <<< Array.mapMaybe (flip Map.lookup utxos)

utxosToTransactionInput :: Utxo -> Array TransactionInput
utxosToTransactionInput =
  Array.mapMaybe (hush <<< getPublicKeyTransactionInput) <<< Map.toUnfoldable

balanceNonAdaOuts
  :: Address
  -> Utxo
  -> TxBody
  -> Either BalanceTxFailure TxBody
balanceNonAdaOuts changeAddr utxos txBody =
  balanceNonAdaOuts' changeAddr utxos txBody # lmap BalanceNonAdaOutsFailure'

-- FIX ME: (payment credential) address for change substitute for pkh (Address)
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L225
-- | We need to balance non ada values as part of the prebalancer before returning
-- | excess Ada to the owner.
balanceNonAdaOuts'
  :: Address
  -> Utxo
  -> TxBody
  -> Either BalanceNonAdaOutsFailure TxBody
balanceNonAdaOuts' changeAddr utxos txBody'@(TxBody txBody) =
  let -- FIX ME: Similar to Address issue, need pkh.
    -- payCredentials :: PaymentCredential
    -- payCredentials = addressPaymentCredentials changeAddr

    -- FIX ME: once both BaseAddresses are merged into one.
    -- pkh :: PubKeyHash
    -- pkh = addressPubKeyHash (unwrap changeAddr)."AddrType"

    txOutputs :: Array TransactionOutput
    txOutputs = txBody.outputs

    inputValue :: Value
    inputValue = getInputValue utxos txBody'

    outputValue :: Value
    outputValue = Array.foldMap getAmount txOutputs

    nonMintedOutputValue :: Value
    nonMintedOutputValue =
      outputValue `minus` maybe mempty unwrap txBody.mint

    nonAdaChange :: Value
    nonAdaChange =
      filterNonAda inputValue `minus` filterNonAda nonMintedOutputValue

    outputs :: Array TransactionOutput
    outputs =
      Array.fromFoldable $
        case
          partition
            ((==) changeAddr <<< _.address <<< unwrap) --  <<< txOutPaymentCredentials)

            $ Array.toUnfoldable txOutputs
          of
          { no: txOuts, yes: Nil } ->
            TransactionOutput
              { address: changeAddr
              , amount: nonAdaChange
              , data_hash: Nothing
              } : txOuts
          { no: txOuts'
          , yes: TransactionOutput txOut@{ amount: v } : txOuts
          } ->
            TransactionOutput
              txOut { amount = v <> nonAdaChange } : txOuts <> txOuts'

  -- Original code uses "isNat" because there is a guard against zero, see
  -- isPos for more detail.
  in
    if isPos nonAdaChange then pure $ wrap txBody { outputs = outputs }
    else if isZero nonAdaChange then pure $ wrap txBody
    else Left $ wrap InputsCannotBalanceNonAdaTokens

getAmount :: TransactionOutput -> Value
getAmount = _.amount <<< unwrap

-- | Add min lovelaces to each tx output
addLovelaces :: MinUtxos -> TxBody -> TxBody
addLovelaces minLovelaces (TxBody txBody) =
  let
    lovelacesAdded :: Array TransactionOutput
    lovelacesAdded =
      map
        ( \txOut' ->
            let
              txOut = unwrap txOut'

              outValue :: Value
              outValue = txOut.amount

              lovelaces :: BigInt
              lovelaces = getLovelace $ fromValue outValue

              minUtxo :: BigInt
              minUtxo = fromMaybe zero $ Foldable.lookup txOut' minLovelaces
            in
              wrap
                txOut
                  { amount =
                      outValue
                        <> lovelaceValueOf (max zero $ minUtxo - lovelaces)
                  }
        )
        txBody.outputs
  in
    wrap txBody { outputs = lovelacesAdded }

-- From https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- | Filter a value to contain only non Ada assets
filterNonAda :: Value -> Value
filterNonAda (Value coins _) = Value coins mempty

requiredSignersToAddress :: RequiredSigner -> Address
requiredSignersToAddress = undefined

addSignatories
  :: Address
  -> Map.Map Address PrivateKey
  -> Array RequiredSigner
  -> TxBody
  -> Either BalanceTxFailure TxBody
addSignatories ownAddr privKeys requiredSigners txBody =
  addSignatories' ownAddr privKeys requiredSigners txBody
    # lmap AddSignatoriesFailure'

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L251
-- | Add the required signatories to the txBody.
addSignatories'
  :: Address
  -> Map.Map Address PrivateKey
  -> Array RequiredSigner
  -> TxBody
  -> Either AddSignatoriesFailure TxBody
addSignatories' ownAddr privKeys requiredSigners txBody =
  Array.foldM
    ( \txBody' addr ->
        case Map.lookup addr privKeys of
          Just privKey -> pure $ txBody' `signBy` privKey
          Nothing -> Left $ wrap $ SigningAddressNotFound addr
    )
    txBody
    $ ownAddr `Array.cons` (requiredSignersToAddress <$> requiredSigners)

getInputValue :: Utxo -> TxBody -> Value
getInputValue utxos (TxBody txBody) =
  Array.foldMap
    getAmount
    (Array.mapMaybe (flip Map.lookup utxos) <<< _.inputs $ txBody)
