module BalanceTx
  ( Actual(..)
  , AddTxCollateralsError(..)
  , BalanceNonAdaOutsError(..)
  , BalanceTxError(..)
  , BalanceTxInsError(..)
  , CannotMinusError(..)
  , Expected(..)
  , GetWalletAddressError(..)
  , GetWalletCollateralError(..)
  , ImpossibleError(..)
  , ReturnAdaChangeError(..)
  , SignTxError(..)
  , GetPublicKeyTransactionInputError(..)
  , UtxoIndexToUtxoError(..)
  , UtxosAtError(..)
  , balanceTxM
  ) where

import Prelude
import Data.Array ((\\), findIndex, modifyAt)
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.BigInt (BigInt, fromInt, quot)
import Data.Either (Either(Left, Right), either, hush, note)
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.List ((:), List(Nil), partition)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe, Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import ProtocolParametersAlonzo
  ( adaOnlyWords
  , coinSize
  , lovelacePerUTxOWord
  , pidSize
  , protocolParamUTxOCostPerWord
  , utxoEntrySizeWithoutVal
  )
import QueryM
  ( FeeEstimateError
  , QueryM
  , calculateMinFee
  , getWalletAddress
  , getWalletCollateral
  , signTransaction
  , utxosAt
  )
import Types.Transaction
  ( DataHash
  , Transaction(Transaction)
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , TxBody(TxBody)
  , Utxo
  )
import Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Types.UnbalancedTransaction
  ( UnbalancedTx(UnbalancedTx)
  , utxoIndexToUtxo
  )
import Serialization.Address (Address, addressPaymentCred, withStakeCredential)
import Helpers (explain)
import Types.Value
  ( filterNonAda
  , geq
  , getLovelace
  , lovelaceValueOf
  , isAdaOnly
  , isPos
  , isZero
  , minus
  , numCurrencySymbols
  , numTokenNames
  , sumTokenNameLengths
  , valueToCoin
  , Value
  )

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs

--------------------------------------------------------------------------------
-- Errors for Balancing functions
--------------------------------------------------------------------------------
-- These derivations may need tweaking when testing to make sure they are easy
-- to read, especially with generic show vs newtype show derivations.
data BalanceTxError
  = GetWalletAddressError' GetWalletAddressError
  | GetWalletCollateralError' GetWalletCollateralError
  | UtxosAtError' UtxosAtError
  | UtxoIndexToUtxoError' UtxoIndexToUtxoError
  | ReturnAdaChangeError' ReturnAdaChangeError
  | AddTxCollateralsError' AddTxCollateralsError
  | GetPublicKeyTransactionInputError' GetPublicKeyTransactionInputError
  | BalanceTxInsError' BalanceTxInsError
  | BalanceNonAdaOutsError' BalanceNonAdaOutsError
  | SignTxError' SignTxError
  | CalculateMinFeeError' FeeEstimateError

derive instance genericBalanceTxError :: Generic BalanceTxError _

instance showBalanceTxError :: Show BalanceTxError where
  show = genericShow

data GetWalletAddressError = CouldNotGetNamiWalletAddress

derive instance genericGetWalletAddressError :: Generic GetWalletAddressError _

instance showGetWalletAddressError :: Show GetWalletAddressError where
  show = genericShow

data GetWalletCollateralError = CouldNotGetNamiCollateral

derive instance genericGetWalletCollateralError :: Generic GetWalletCollateralError _

instance showGetWalletCollateralError :: Show GetWalletCollateralError where
  show = genericShow

data UtxosAtError = CouldNotGetUtxos

derive instance genericUtxosAtError :: Generic UtxosAtError _

instance showUtxosAtError :: Show UtxosAtError where
  show = genericShow

data UtxoIndexToUtxoError = CouldNotConvertUtxoIndex

derive instance genericUtxoIndexToUtxoError :: Generic UtxoIndexToUtxoError _

instance showUtxoIndexToUtxoError :: Show UtxoIndexToUtxoError where
  show = genericShow

data ReturnAdaChangeError
  = ReturnAdaChangeError String
  | ReturnAdaChangeImpossibleError String ImpossibleError
  | ReturnAdaChangeCalculateMinFee FeeEstimateError

derive instance genericReturnAdaChangeError :: Generic ReturnAdaChangeError _

instance showReturnAdaChangeError :: Show ReturnAdaChangeError where
  show = genericShow

data AddTxCollateralsError = CollateralUtxosUnavailable

derive instance genericAddTxCollateralsError :: Generic AddTxCollateralsError _

instance showAddTxCollateralsError :: Show AddTxCollateralsError where
  show = genericShow

data GetPublicKeyTransactionInputError = CannotConvertScriptOutputToTxInput

derive instance genericGetPublicKeyTransactionInputError :: Generic GetPublicKeyTransactionInputError _

instance showGetPublicKeyTransactionInputError :: Show GetPublicKeyTransactionInputError where
  show = genericShow

data BalanceTxInsError
  = InsufficientTxInputs Expected Actual
  | BalanceTxInsCannotMinus CannotMinusError

derive instance genericBalanceTxInsError :: Generic BalanceTxInsError _

instance showBalanceTxInsError :: Show BalanceTxInsError where
  show = genericShow

data CannotMinusError = CannotMinus Actual

derive instance genericCannotMinusError :: Generic CannotMinusError _

instance showCannotMinusError :: Show CannotMinusError where
  show = genericShow

data CollectTxInsError = CollectTxInsInsufficientTxInputs BalanceTxInsError

derive instance genericCollectTxInsError :: Generic CollectTxInsError _

instance showCollectTxInsError :: Show CollectTxInsError where
  show = genericShow

newtype Expected = Expected Value

derive instance genericExpected :: Generic Expected _
derive instance newtypeExpected :: Newtype Expected _

instance showExpected :: Show Expected where
  show = genericShow

newtype Actual = Actual Value

derive instance genericActual :: Generic Actual _
derive instance newtypeActual :: Newtype Actual _

instance showActual :: Show Actual where
  show = genericShow

data BalanceNonAdaOutsError
  = InputsCannotBalanceNonAdaTokens
  | BalanceNonAdaOutsCannotMinus CannotMinusError

derive instance genericBalanceNonAdaOutsError :: Generic BalanceNonAdaOutsError _

instance showBalanceNonAdaOutsError :: Show BalanceNonAdaOutsError where
  show = genericShow

data SignTxError = CouldNotSignTx Address

derive instance genericSignTxError :: Generic SignTxError _

instance showSignTxError :: Show SignTxError where
  show = genericShow

-- | Represents that an error reason should be impossible
data ImpossibleError = Impossible

derive instance genericImpossibleError :: Generic ImpossibleError _

instance showImpossibleError :: Show ImpossibleError where
  show = genericShow

--------------------------------------------------------------------------------
-- Type aliases, temporary placeholder types and functions
--------------------------------------------------------------------------------
-- Output utxos with the amount of lovelaces required.
type MinUtxos = Array (TransactionOutput /\ BigInt)

calculateMinFee'
  :: Transaction
  -> QueryM (Either FeeEstimateError BigInt)
calculateMinFee' = calculateMinFee >>> map (rmap unwrap)

--------------------------------------------------------------------------------
-- Balancing functions and helpers
--------------------------------------------------------------------------------
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L54
-- FIX ME: UnbalancedTx contains requiredSignatories which would be a part of
-- multisig but we don't have such functionality ATM.
-- | Balances an unbalanced transaction
balanceTxM :: UnbalancedTx -> QueryM (Either BalanceTxError Transaction)
balanceTxM (UnbalancedTx { transaction: unbalancedTx, utxoIndex }) = do
  ownAddr' <- getWalletAddress
  collateral' <- getWalletCollateral
  -- Saves on an extra nested case:
  case ownAddr', collateral' of
    Nothing, _ ->
      pure $ Left $ GetWalletAddressError' CouldNotGetNamiWalletAddress
    _, Nothing ->
      pure $ Left $ GetWalletCollateralError' CouldNotGetNamiCollateral
    Just ownAddr, Just collateral -> do
      utxos' <- map unwrap <$> utxosAt ownAddr
      let utxoIndex' = utxoIndexToUtxo utxoIndex
      -- Don't need to sequence, can just do as above
      case utxos', utxoIndex' of
        Nothing, _ ->
          pure $ Left $ UtxosAtError' CouldNotGetUtxos
        _, Nothing ->
          pure $ Left $ UtxoIndexToUtxoError' CouldNotConvertUtxoIndex
        Just utxos, Just utxoIndex'' -> do
          let
            -- Combines utxos at the user address and those from any scripts
            -- involved with the contract in the unbalanced transaction. Don't
            -- add collateral to this for now.
            allUtxos :: Utxo
            allUtxos = utxos `Map.union` utxoIndex''
          -- Add hardcoded Nami 5 Ada and sign before instead of recursively
          -- signing in loop. Could refactor this part into a separate function.
          signedUnbalancedTx <-
            signTransaction (addTxCollateral unbalancedTx collateral)
              <#> note (SignTxError' $ CouldNotSignTx ownAddr)
          case signedUnbalancedTx of
            Left err -> pure $ Left err
            Right sUbTx ->
              -- After adding collateral, we need to balance the inputs and
              -- non-Ada outputs before looping, i.e. we need to add input fees
              -- for the Ada only collateral. No MinUtxos required:
              -- Alternatively add this functionality back to preBalanceTxBody
              -- and only call collateral adder initially.
              -- Balance tx without fees:
              case prebalanceCollateral zero allUtxos ownAddr sUbTx of
                Left err -> pure $ Left err
                Right sUbTx' -> do
                  fees' <- lmap CalculateMinFeeError' <$> calculateMinFee' sUbTx'
                  -- Balance tx with fees:
                  let
                    signedCollUnbalancedTx' = fees' >>= \fees ->
                      prebalanceCollateral
                        (fees + feeBuffer)
                        allUtxos
                        ownAddr
                        sUbTx'
                  case signedCollUnbalancedTx' of
                    Left err -> pure $ Left err
                    Right signedCollUnbalancedTx ->
                      loop allUtxos ownAddr [] signedCollUnbalancedTx >>=
                        either
                          (Left >>> pure)
                          ( returnAdaChange ownAddr allUtxos
                              >>> map (lmap ReturnAdaChangeError')
                          )
  where
  prebalanceCollateral
    :: BigInt
    -> Utxo
    -> Address
    -> Transaction
    -> Either BalanceTxError Transaction
  prebalanceCollateral
    fees'
    utxoIndex'
    ownAddr'
    oldTx'@(Transaction { body: txBody }) =
    balanceTxIns utxoIndex' fees' txBody
      >>= balanceNonAdaOuts ownAddr' utxoIndex'
      >>= \txBody' -> pure $ wrap (unwrap oldTx') { body = txBody' }

  loop
    :: Utxo
    -> Address
    -> MinUtxos
    -> Transaction
    -> QueryM (Either BalanceTxError Transaction)
  loop
    utxoIndex'
    ownAddr'
    prevMinUtxos'
    tx''@(Transaction tx'@{ body: txBody'@(TxBody txB) }) = do
    let
      nextMinUtxos' :: MinUtxos
      nextMinUtxos' =
        calculateMinUtxos $ txB.outputs \\ map fst prevMinUtxos'

      minUtxos' :: MinUtxos
      minUtxos' = prevMinUtxos' <> nextMinUtxos'

    balancedTxBody' :: Either BalanceTxError TxBody <-
      chainedBalancer
        minUtxos'
        utxoIndex'
        ownAddr'
        tx''

    case balancedTxBody' of
      Left err -> pure $ Left err
      Right balancedTxBody'' ->
        if txBody' == balancedTxBody'' then
          pure $ Right $ wrap tx' { body = balancedTxBody'' }
        else
          loop
            utxoIndex'
            ownAddr'
            minUtxos'
            $ wrap tx' { body = balancedTxBody'' }

  chainedBalancer
    :: MinUtxos
    -> Utxo
    -> Address
    -> Transaction
    -> QueryM (Either BalanceTxError TxBody)
  chainedBalancer
    minUtxos'
    utxoIndex'
    ownAddr'
    (Transaction tx'@{ body: txBody' }) =
    do
      pure $ preBalanceTxBody
        minUtxos'
        zero
        utxoIndex'
        ownAddr'
        txBody'
      >>= case _ of
        Left err -> pure $ Left err
        Right txBodyWithoutFees' -> do
          let
            tx'' :: Transaction
            tx'' = wrap tx' { body = txBodyWithoutFees' }
          fees'' <- lmap CalculateMinFeeError' <$> calculateMinFee' tx''
          case fees'' of
            Left err -> pure $ Left err
            Right fees' ->
              pure $ preBalanceTxBody
                minUtxos'
                (fees' + feeBuffer) -- FIX ME: Add 0.5 Ada to ensure enough input for later on in final balancing.
                utxoIndex'
                ownAddr'
                txBody'

  feeBuffer :: BigInt
  feeBuffer = fromInt 500000

-- Nami provides a 5 Ada collateral that we should add the tx before balancing
addTxCollateral :: Transaction -> TransactionUnspentOutput -> Transaction
addTxCollateral (Transaction tx@{ body: TxBody txBody }) txUnspentOutput =
  wrap tx
    { body = wrap txBody
        { collateral = pure $ Array.singleton $ (unwrap txUnspentOutput).input }
    }

-- Transaction should be prebalanced at this point with all excess with Ada
-- where the Ada value of inputs is greater or equal to value of outputs.
-- Also add fees to txBody. This should be called with a Tx with min
-- Ada in each output utxo, namely, after "loop".
returnAdaChange
  :: Address
  -> Utxo
  -> Transaction
  -> QueryM (Either ReturnAdaChangeError Transaction)
returnAdaChange changeAddr utxos (Transaction tx@{ body: TxBody txBody }) =
  lmap ReturnAdaChangeCalculateMinFee
    <$> (calculateMinFee' $ wrap tx)
    >>= case _ of
      Left err -> pure $ Left err
      Right fees -> do
        let
          txOutputs :: Array TransactionOutput
          txOutputs = txBody.outputs

          inputValue :: Value
          inputValue = getInputValue utxos (wrap txBody)

          inputAda :: BigInt
          inputAda = getLovelace $ valueToCoin inputValue

          -- FIX ME, ignore mint value?
          outputValue :: Value
          outputValue = Array.foldMap getAmount txOutputs

          outputAda :: BigInt
          outputAda = getLovelace $ valueToCoin outputValue

          returnAda :: BigInt
          returnAda = inputAda - outputAda - fees
        case compare returnAda zero of
          LT ->
            pure $ Left $
              ReturnAdaChangeImpossibleError
                "Not enough Input Ada to cover output and fees after prebalance."
                Impossible
          EQ -> pure $ Right $ wrap tx { body = wrap txBody { fee = wrap fees } }
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
              Just idx -> pure do
                -- Add the Ada value to the first output utxo of the owner to not
                -- concur fees. This should be Ada only or non-Ada which has min Ada.
                newOutputs :: Array TransactionOutput <-
                  note
                    ( ReturnAdaChangeError
                        "Couldn't modify utxo to return change."
                    ) $
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

                  changeMinUtxo :: BigInt
                  changeMinUtxo = adaOnlyWords * utxoCost

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

                  tx' :: Transaction
                  tx' = wrap tx { body = txBody' }

                fees'' :: Either ReturnAdaChangeError BigInt <-
                  lmap ReturnAdaChangeCalculateMinFee <$> calculateMinFee' tx'

                -- fees should increase.
                pure case fees'' of
                  Left err -> Left err
                  Right fees' -> do
                    -- New return Ada amount should decrease:
                    let
                      returnAda' :: BigInt
                      returnAda' = returnAda + fees - fees'

                    if returnAda' >= changeMinUtxo then do
                      newOutputs :: Array TransactionOutput <-
                        note
                          ( ReturnAdaChangeImpossibleError
                              "Couldn't modify head utxo to add Ada"
                              Impossible
                          )
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
                      Left $
                        ReturnAdaChangeError
                          "ReturnAda' does not cover min. utxo requirement for \
                          \single Ada-only output."

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

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L116
preBalanceTxBody
  :: MinUtxos
  -> BigInt
  -> Utxo
  -> Address
  -> TxBody
  -> Either BalanceTxError TxBody
preBalanceTxBody minUtxos fees utxos ownAddr txBody =
  -- -- Take a single Ada only utxo collateral
  -- addTxCollaterals utxos txBody
  --   >>= balanceTxIns utxos fees -- Add input fees for the Ada only collateral
  --   >>= balanceNonAdaOuts ownAddr utxos
  addLovelaces minUtxos txBody # pure
    >>= balanceTxIns utxos fees -- Adding more inputs if required
    >>= balanceNonAdaOuts ownAddr utxos

-- addTxCollaterals :: Utxo -> TxBody -> Either BalanceTxError TxBody
-- addTxCollaterals utxo txBody =
--   addTxCollaterals' utxo txBody # lmap AddTxCollateralsError

-- -- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L211
-- -- | Pick a collateral from the utxo map and add it to the unbalanced transaction
-- -- | (suboptimally we just pick a random utxo from the tx inputs). TO DO: upgrade
-- -- | to a better coin selection algorithm.
-- addTxCollaterals' :: Utxo -> TxBody -> Either AddTxCollateralsError TxBody
-- addTxCollaterals' utxos (TxBody txBody) = do
--   let
--     txIns :: Array TransactionInput
--     txIns = utxosToTransactionInput $ filterAdaOnly utxos
--   txIn :: TransactionInput <- findPubKeyTxIn txIns
--   pure $ wrap txBody { collateral = Just (Array.singleton txIn) }
--   where
--   filterAdaOnly :: Utxo -> Utxo
--   filterAdaOnly = Map.filter (isAdaOnly <<< getAmount)

--   -- Take head for collateral - can use better coin selection here.
--   findPubKeyTxIn
--     :: Array TransactionInput
--     -> Either AddTxCollateralsError TransactionInput
--   findPubKeyTxIn =
--     note CollateralUtxosUnavailable <<< Array.head

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- | Get TransactionInput such that it is associated to PaymentCredentialKey
-- | and not PaymentCredentialScript, i.e. we want wallets only
getPublicKeyTransactionInput
  :: TransactionInput /\ TransactionOutput
  -> Either GetPublicKeyTransactionInputError TransactionInput
getPublicKeyTransactionInput (txOutRef /\ txOut) =
  explain CannotConvertScriptOutputToTxInput $ do
    paymentCred <- unwrap txOut # (_.address >>> addressPaymentCred)
    -- TEST ME: using StakeCredential to determine whether wallet or script
    paymentCred # withStakeCredential
      { onKeyHash: const $ pure txOutRef
      , onScriptHash: const Nothing
      }


balanceTxIns :: Utxo -> BigInt -> TxBody -> Either BalanceTxError TxBody
balanceTxIns utxos fees txbody =
  balanceTxIns' utxos fees txbody # lmap BalanceTxInsError'

-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- Notice we aren't using protocol parameters for utxo cost per word.
balanceTxIns'
  :: Utxo
  -> BigInt
  -> TxBody
  -> Either BalanceTxInsError TxBody
balanceTxIns' utxos fees (TxBody txBody) = do
  let
    utxoCost :: BigInt
    utxoCost = getLovelace protocolParamUTxOCostPerWord

    changeMinUtxo :: BigInt
    changeMinUtxo = adaOnlyWords * utxoCost

    txOutputs :: Array TransactionOutput
    txOutputs = txBody.outputs

    mintVal :: Value
    mintVal = maybe mempty unwrap txBody.mint

  nonMintedValue <- note (BalanceTxInsCannotMinus $ CannotMinus $ wrap mintVal)
    $ Array.foldMap getAmount txOutputs `minus` mintVal

  let
    minSpending :: Value
    minSpending = lovelaceValueOf (fees + changeMinUtxo) <> nonMintedValue

  txIns :: Array TransactionInput <-
    lmap
      ( \(CollectTxInsInsufficientTxInputs insufficientTxInputs) -> insufficientTxInputs
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
  -> Either CollectTxInsError (Array TransactionInput)
collectTxIns originalTxIns utxos value =
  if isSufficient updatedInputs then pure updatedInputs
  else
    Left $ CollectTxInsInsufficientTxInputs $
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
  -> Either BalanceTxError TxBody
balanceNonAdaOuts changeAddr utxos txBody =
  balanceNonAdaOuts' changeAddr utxos txBody # lmap BalanceNonAdaOutsError'

-- FIX ME: (payment credential) address for change substitute for pkh (Address)
-- https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs#L225
-- | We need to balance non ada values as part of the prebalancer before returning
-- | excess Ada to the owner.
balanceNonAdaOuts'
  :: Address
  -> Utxo
  -> TxBody
  -> Either BalanceNonAdaOutsError TxBody
balanceNonAdaOuts' changeAddr utxos txBody'@(TxBody txBody) = do
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

    mintVal :: Value
    mintVal = maybe mempty unwrap txBody.mint

  nonMintedOutputValue <- note (BalanceNonAdaOutsCannotMinus $ CannotMinus $ wrap mintVal)
    $ outputValue `minus` mintVal

  let (nonMintedAdaOutputValue :: Value) = filterNonAda nonMintedOutputValue

  nonAdaChange <- note (BalanceNonAdaOutsCannotMinus $ CannotMinus $ wrap nonMintedAdaOutputValue)
    $ filterNonAda inputValue `minus` nonMintedAdaOutputValue

  let
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
  if isPos nonAdaChange then pure $ wrap txBody { outputs = outputs }
  else if isZero nonAdaChange then pure $ wrap txBody
  else Left InputsCannotBalanceNonAdaTokens

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
              lovelaces = getLovelace $ valueToCoin outValue

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

getInputValue :: Utxo -> TxBody -> Value
getInputValue utxos (TxBody txBody) =
  Array.foldMap
    getAmount
    (Array.mapMaybe (flip Map.lookup utxos) <<< _.inputs $ txBody)
