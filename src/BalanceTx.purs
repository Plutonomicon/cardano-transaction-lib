module BalanceTx
  ( balanceTxM
  , UnbalancedTransaction(..)
  )
  where

import Prelude
import Data.Array ((\\), findIndex, modifyAt)
import Data.Array as Array
import Data.BigInt (BigInt, fromInt, quot)
import Data.Either (Either(..), hush, note)
import Data.Foldable as Foldable
import Data.List ((:), List(..), partition)
import Data.Map as Map
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Undefined (undefined)

import Ogmios (QueryM)
import ProtocolParametersAlonzo (coinSize, lovelacePerUTxOWord, pidSize, protocolParamUTxOCostPerWord, utxoEntrySizeWithoutVal)
import Types.Ada (adaSymbol, fromValue, getLovelace, lovelaceValueOf)
import Types.Transaction (Address, Credential(..), DataHash, RequiredSigner, Transaction(..), TransactionInput, TransactionOutput(..), TxBody(..), Utxo, UtxoM)
import Types.Value (allTokenNames, emptyValue, flattenValue, geq, getValue, isAdaOnly, isPos, isZero, minus, numCurrencySymbols, numTokenNames, TokenName, Value(..))
import UInt8Array (_byteLengthUint8Array)

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs

-- Output utxos with the amount of lovelaces required.
type MinUtxos = Array (TransactionOutput /\ BigInt)

-- TO DO: convert utxosAt from Ogmios to Transaction space.
utxosAt :: Address -> QueryM UtxoM
utxosAt = undefined

newtype PubKeyHash = PubKeyHash String
newtype PrivateKey = PrivateKey String

-- getPkhFromAddress :: Address -> PubKeyHash
-- getPkhFromAddress = undefined

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

-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs#54
-- | Balances an unbalanced transaction
balanceTxM
  :: Address
  -> UnbalancedTransaction
  -> QueryM (Either String Transaction)
balanceTxM ownAddr (UnbalancedTransaction { unbalancedTx, utxoIndex }) = do
  utxos :: Utxo <- unwrap <$> utxosAt ownAddr
  privKeys :: Map.Map Address PrivateKey <- getPrivKeys
  let -- Combines utxos at the user address and those from any scripts involved
      -- with the contract.
      utxoIndex' :: Utxo
      utxoIndex' = utxos `Map.union` unwrap utxoIndex

  case (unwrap (unwrap unbalancedTx).body).required_signers of
    Nothing -> pure $ Left "balanceTxM: Unknown required signers."
    Just requiredSigners -> do
      prebalancedTx' :: Either String Transaction <-
        loop utxoIndex' ownAddr privKeys requiredSigners [] unbalancedTx
      pure do
        prebalancedTx :: Transaction <- prebalancedTx'
        returnAdaChange ownAddr utxoIndex' prebalancedTx
  where
    loop ::
      Utxo ->
      Address ->
      Map.Map Address PrivateKey ->
      Array RequiredSigner ->
      MinUtxos ->
      Transaction ->
      QueryM (Either String Transaction)
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

          balancedTxBody' :: Either String TxBody
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
      -> Either String TxBody
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
        tx' :: Transaction <- buildTxRaw txBodyWithoutFees'
        fees' :: BigInt <- calculateMinFee tx' -- FIX ME: use txBodyWithoutFees replaced in original tx.
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
returnAdaChange :: Address -> Utxo -> Transaction -> Either String Transaction
returnAdaChange changeAddr utxos (Transaction tx@{ body: TxBody txBody }) = do
  fees :: BigInt <- calculateMinFee $ wrap tx
  let txOutputs :: Array TransactionOutput
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
    LT -> Left "returnAdaChange: Not enough Input Ada to cover output and fees \
           \after prebalance - this should be impossible if called after loop."
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
      let changeIndex :: Maybe Int
          changeIndex =
            findIndex ((==) changeAddr <<< _.address <<< unwrap) txOutputs

      case changeIndex of
        Just idx -> do
          -- Add the Ada value to the first output utxo of the owner to not
          -- concur fees. This should be Ada only or non-Ada which has min Ada.
          newOutputs :: Array TransactionOutput <-
            note "returnAdaChange: Couldn't modify utxo to return change." $
              modifyAt
                idx
                ( \(TransactionOutput o@{ amount })
                    -> TransactionOutput
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
          let utxoCost :: BigInt
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
          tx' :: Transaction <- buildTxRaw txBody'
          fees' :: BigInt <- calculateMinFee tx' -- fees should increase.
          -- New return Ada amount should decrease:
          let returnAda' :: BigInt
              returnAda' = returnAda + fees - fees'

          if returnAda' >= changeMinUtxo
           then do
            newOutputs :: Array TransactionOutput <-
              note "returnAdaChange: Couldn't modify head utxo to add Ada - \
                \this should be impossible." $
                modifyAt
                  0
                  (\(TransactionOutput o)
                      -> TransactionOutput
                            o { amount = lovelaceValueOf returnAda }) $
                  _.outputs <<< unwrap $ txBody'
            pure $
              wrap
                tx { body = wrap txBody { outputs = newOutputs, fee = wrap fees } }
           else
            Left "returnAdaChange: ReturnAda' does not cover min. utxo \
              \requirement for single Ada-only output."

-- Some functionality that builds a Transaction from a TxBody without balancing
-- https://github.com/input-output-hk/cardano-node/blob/b6ca519f97a0e795611a63174687e6bb70c9f752/cardano-cli/src/Cardano/CLI/Shelley/Run/Transaction.hs#L312
-- Is this relevant?
buildTxRaw :: TxBody -> Either String Transaction
buildTxRaw = undefined

-- There is an estimate function here:
-- https://input-output-hk.github.io/cardano-node/cardano-api/src/Cardano.Api.Fees.html#estimateTransactionFee
-- Otherwise, we'd need a Haskell server to run the "exact" version, since we
-- can't run the Plutus interpreter via Purescript.
calculateMinFee :: Transaction -> Either String BigInt
calculateMinFee = undefined

calculateMinUtxos
  :: Array TransactionOutput
  -> MinUtxos
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
      let outputValue :: Value
          outputValue = txOut'.amount
      in if isAdaOnly outputValue
          then utxoEntrySizeWithoutVal + coinSize -- 29 in Alonzo
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
        lenAdd = \c a -> c + (_byteLengthUint8Array $ unwrap a)

-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs#L116
preBalanceTxBody
  :: MinUtxos
  -> BigInt
  -> Utxo
  -> Address
  -> Map.Map Address PrivateKey
  -> Array RequiredSigner
  -> TxBody
  -> Either String TxBody
preBalanceTxBody minUtxos fees utxos ownAddr privKeys requiredSigners txBody =
  addTxCollaterals utxos txBody -- Take a single Ada only utxo collateral
    >>= balanceTxIns utxos fees -- Add input fees for the Ada only collateral
    >>= balanceNonAdaOuts ownAddr utxos
    >>= pure <<< addLovelaces minUtxos
    >>= balanceTxIns utxos fees -- Adding more inputs if required
    >>= balanceNonAdaOuts ownAddr utxos
    >>= addSignatories ownAddr privKeys requiredSigners

-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs#L211
-- | Pick a collateral from the utxo map and add it to the unbalanced transaction
-- (suboptimally we just pick a random utxo from the tx inputs). TO DO: upgrade
-- to a better coin selection algorithm.
addTxCollaterals :: Utxo -> TxBody -> Either String TxBody
addTxCollaterals utxos (TxBody txBody) = do
  let txIns :: Array TransactionInput
      txIns = utxosToTransactionInput $ filterAdaOnly utxos
  txIn :: TransactionInput <- findPubKeyTxIn txIns
  pure $ wrap txBody { collateral = Just (Array.singleton txIn) }
  where
    filterAdaOnly :: Utxo -> Utxo
    filterAdaOnly = Map.filter (isAdaOnly <<< getAmount)

    -- FIX ME: Plutus has Maybe TxInType e.g. Just ConsumePublicKeyAddress)
    -- We can't distinguish where a utxo is from a wallet (pubkey) or script it seems.
    -- for now, we take the head. The Haskell logic is pasted below:
    -- findPubKeyTxIn = \case
    --   x@(TxIn _ (Just ConsumePublicKeyAddress)) : _ -> Right x
    --   x@(TxIn _ Nothing) : _ -> Right x
    --   _ : xs -> findPubKeyTxIn xs
    --   _ -> Left "There are no utxos to be used as collateral"
    findPubKeyTxIn :: Array TransactionInput -> Either String TransactionInput
    findPubKeyTxIn =
      note "addTxCollaterals: There are no utxos to be used as collateral"
        <<< Array.head

-- FIX ME: may need to revisit for credential granularity. See "txOutToTxIn" in
-- -- Converting a chain index transaction output to a transaction input type
-- txOutToTxIn :: (TxOutRef, TxOut) -> Either Text TxIn
-- txOutToTxIn (txOutRef, txOut) =
--   case addressCredential (txOutAddress txOut) of
--     PubKeyCredential _ -> Right $ Tx.pubKeyTxIn txOutRef
--     ScriptCredential _ -> Left "Cannot covert a script output to TxIn"
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
toEitherTransactionInput
  :: TransactionInput /\ TransactionOutput
  -> Either String TransactionInput
toEitherTransactionInput (txOutRef /\ txOut) =
  case txOutPaymentCredentials txOut of
    -- FIX ME: need to determine it's a pubkey credential as opposed to script
    -- credential.
    Credential _ ->
      pure txOutRef
    _ -> -- Currently unreachable:
      Left "toEitherTransactionInput: Cannot convert an output to \
        \TransactionInput"

addressPaymentCredentials :: Address -> Credential
addressPaymentCredentials = _.payment <<< unwrap <<< _."AddrType" <<< unwrap

-- FIX ME: do we need granularity for staking credential? We need pkh?
txOutPaymentCredentials :: TransactionOutput -> Credential
txOutPaymentCredentials = addressPaymentCredentials <<< _.address  <<< unwrap

-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
-- Notice we aren't using protocol parameters for utxo cost per word.
balanceTxIns :: Utxo -> BigInt -> TxBody -> Either String TxBody
balanceTxIns utxos fees (TxBody txBody) = do
  let utxoCost :: BigInt
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
          `minus` fromMaybe emptyValue txBody.mint

      minSpending :: Value
      minSpending = lovelaceValueOf (fees + changeMinUtxo) <> nonMintedValue

  txIns :: Array TransactionInput <-
    collectTxIns txBody.inputs utxos minSpending
  -- Original code uses Set append which is union. Array unions behave
  -- a little differently as it removes duplicates in the second argument.
  -- but all inputs should be unique anyway so I think this is fine.
  pure $ wrap
    txBody
      { inputs = Array.union txIns txBody.inputs
      }

-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
-- | Getting the necessary input utxos to cover the fees for the transaction
collectTxIns
  :: Array TransactionInput
  -> Utxo
  -> Value
  -> Either String (Array TransactionInput)
collectTxIns originalTxIns utxos value =
  if isSufficient updatedInputs
   then pure updatedInputs
   else
    Left $
      "collectTxIns: Insufficient tx inputs, needed: "
      <> show (flattenValue value)
      <> ", got: "
      <> show (flattenValue $ txInsValue updatedInputs)
  where
    updatedInputs :: Array TransactionInput
    updatedInputs =
      Foldable.foldl
        ( \newTxIns txIn ->
            if isSufficient newTxIns
             then newTxIns
             else
              -- set insertion in original code.
              if txIn `Array.elem` newTxIns
                then newTxIns
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

-- FIX ME: toEitherTransactionInput may need fixing depending on our data types.
utxosToTransactionInput :: Utxo -> Array TransactionInput
utxosToTransactionInput =
  Array.mapMaybe (hush <<< toEitherTransactionInput) <<< Map.toUnfoldable

-- FIX ME: (payment credential) address for change substitute for pkh (Address)
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs#L225
-- | We need to balance non ada values as part of the prebalancer before returning
-- | excess Ada to the owner.
balanceNonAdaOuts :: Address -> Utxo -> TxBody -> Either String TxBody
balanceNonAdaOuts changeAddr utxos txBody'@(TxBody txBody) =
  let  -- FIX ME: Similar to Address issue, need pkh.
      -- payCredentials :: Credential
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
        outputValue `minus` fromMaybe emptyValue txBody.mint

      nonAdaChange :: Value
      nonAdaChange =
        filterNonAda inputValue `minus` filterNonAda nonMintedOutputValue

      outputs :: Array TransactionOutput
      outputs =
        Array.fromFoldable $
          case partition
            ((==) changeAddr <<< _.address <<< unwrap) --  <<< txOutPaymentCredentials)
            $ Array.toUnfoldable txOutputs of
              { no: txOuts, yes: Nil } ->
                TransactionOutput
                  { address: changeAddr,
                    amount: nonAdaChange,
                    data_hash: Nothing
                  } : txOuts
              { no: txOuts'
              , yes: TransactionOutput txOut@{ amount: v } : txOuts
              } ->
                TransactionOutput
                  txOut { amount = v <> nonAdaChange } : txOuts <> txOuts'

   -- Original code uses "isNat" because there is a guard against zero, see
   -- isPos for more detail.
   in if isPos nonAdaChange
       then pure $ wrap txBody { outputs = outputs }
       else
        if isZero nonAdaChange
         then pure $ wrap txBody
         else Left "balanceNonAdaOuts: Not enough inputs to balance tokens."

getAmount :: TransactionOutput -> Value
getAmount = _.amount <<< unwrap

-- | Add min lovelaces to each tx output
addLovelaces :: MinUtxos -> TxBody -> TxBody
addLovelaces minLovelaces (TxBody txBody) =
  let lovelacesAdded :: Array TransactionOutput
      lovelacesAdded =
        map
          ( \txOut' ->
              let txOut = unwrap txOut'

                  outValue :: Value
                  outValue = txOut.amount

                  lovelaces :: BigInt
                  lovelaces = getLovelace $ fromValue outValue

                  minUtxo :: BigInt
                  minUtxo = fromMaybe zero $ Foldable.lookup txOut' minLovelaces
              in wrap
                  txOut
                    { amount =
                        outValue
                          <> lovelaceValueOf (max zero $ minUtxo - lovelaces)
                    }
           ) txBody.outputs
   in wrap txBody { outputs = lovelacesAdded }

-- From https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
-- | Filter a value to contain only non Ada assets
filterNonAda :: Value -> Value
filterNonAda =
  Value <<< Map.filterKeys (_ /= adaSymbol) <<< getValue

requiredSignersToAddress :: RequiredSigner -> Address
requiredSignersToAddress = undefined

-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs#L251
-- | Add the required signatories to the txBody.
addSignatories
  :: Address
  -> Map.Map Address PrivateKey
  -> Array RequiredSigner
  -> TxBody
  -> Either String TxBody
addSignatories ownAddr privKeys requiredSigners txBody =
  Array.foldM
    ( \txBody' addr ->
        case Map.lookup addr privKeys of
          Just privKey -> pure $ txBody' `signBy` privKey
          Nothing -> Left "addSignatories: Signing address not found."
    )
    txBody
    $ ownAddr `Array.cons` (requiredSignersToAddress <$> requiredSigners)

-- FIX ME: We need proper signing functionality. Do we need it to sign a TxBody
-- or Transaction?
signBy :: TxBody -> PrivateKey -> TxBody
signBy = undefined

getInputValue :: Utxo -> TxBody -> Value
getInputValue utxos (TxBody txBody) =
  Array.foldMap
    getAmount
    (Array.mapMaybe (flip Map.lookup utxos) <<< _.inputs $ txBody)
