module PreBalanceTx
  ( balanceTxM
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
import Data.String.CodeUnits (length)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Undefined (undefined)

-- import Address (addressPubKeyHash, PubKeyHash)
import Ogmios (QueryM)
import ProtocolParametersAlonzo (coinSize, lovelacePerUTxOWord, pidSize, protocolParamUTxOCostPerWord, utxoEntrySizeWithoutVal)
import Types.Ada (adaSymbol, fromValue, getLovelace, lovelaceValueOf)
import Types.Transaction (Address, Credential(..), RequiredSigner, Transaction(..), TransactionInput, TransactionOutput(..), TxBody(..), Utxo, UtxoM)
import Types.Value (allTokenNames, emptyValue, flattenValue, geq, getValue, isAdaOnly, isPos, isZero, minus, numCurrencySymbols, numTokenNames, TokenName, Value(..))

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs

-- Output utxos with the amount of lovelaces required.
type MinUtxos = Array (TransactionOutput /\ BigInt)

-- TO DO: convert utxosAt from Ogmios to Transaction space.
utxosAt :: Address -> QueryM UtxoM
utxosAt = undefined

balanceTxM
  :: Address
  -> Map.Map Address RequiredSigner -- FIX ME: take from unbalanced tx?
  -> Array Address -- FIX ME: take from unbalanced tx?
  -> Transaction -- unbalanced transaction, FIX ME: do we need a newtype wrapper?
  -> QueryM (Either String Transaction)
balanceTxM ownAddr addReqSigners requiredAddrs unbalancedTx = do
  utxos :: Utxo <- unwrap <$> utxosAt ownAddr -- Do we want :: Either String UtxoM here?
  let utxoIndex :: Utxo
      utxoIndex = utxos  -- FIX ME: include newtype wrapper? UNWRAP
      _unwrapUnbalancedTx = unwrap unbalancedTx
  prebalancedTx :: Either String Transaction <-
    loop utxoIndex ownAddr addReqSigners requiredAddrs [] unbalancedTx
  pure do
    prebalancedTx' :: Transaction <- prebalancedTx
    returnAdaChange ownAddr utxos prebalancedTx'
  where
    loop ::
      Utxo ->
      Address ->
      Map.Map Address RequiredSigner ->
      Array Address ->
      MinUtxos ->
      Transaction ->
      QueryM (Either String Transaction)
    loop
      utxoIndex'
      ownAddr'
      addReqSigners'
      requiredAddrs'
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
              addReqSigners'
              requiredAddrs'
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
              addReqSigners'
              requiredAddrs'
              minUtxos'
              $ wrap tx' { body = balancedTxBody'' }

    chainedBalancer
      :: MinUtxos
      -> Utxo
      -> Address
      -> Map.Map Address RequiredSigner
      -> Array Address
      -> TxBody
      -> Either String TxBody
    chainedBalancer
      minUtxos' utxoIndex' ownAddr' addReqSigners' requiredAddrs' txBody' = do
        txBodyWithoutFees' :: TxBody <-
          preBalanceTxBody
            minUtxos'
            zero
            utxoIndex'
            ownAddr'
            addReqSigners'
            requiredAddrs'
            txBody'
        -- buildTxRaw shouldn't include a hardcoded collateral, Nami may do this
        -- so be sure to change code as needed.
        tx' :: Transaction <- buildTxRaw txBodyWithoutFees'
        fees' :: BigInt <- calculateMinFee tx' -- FIX ME: use txBodyWithoutFees replaced in original tx.
        preBalanceTxBody
          minUtxos'
            (fees' + fromInt 500000) -- FIX ME: Add 0.5 Ada to ensure enough input for later on in final balancing.
            utxoIndex'
            ownAddr'
            addReqSigners'
            requiredAddrs'
            txBody'

--       let minUtxos = prevMinUtxos ++ nextMinUtxos

--       lift $ printLog @w Debug $ "Min utxos: " ++ show minUtxos

--       txWithoutFees <-
--         hoistEither $ preBalanceTx pabConf.pcProtocolParams minUtxos 0 utxoIndex ownPkh privKeys requiredSigs tx

--       lift $ createDirectoryIfMissing @w False (Text.unpack pabConf.pcTxFileDir)
--       lift $ CardanoCLI.buildTx @w pabConf ownPkh (CardanoCLI.BuildRaw 0) txWithoutFees
--       fees <- newEitherT $ CardanoCLI.calculateMinFee @w pabConf txWithoutFees

--       lift $ printLog @w Debug $ "Fees: " ++ show fees

--       balancedTx <- hoistEither $ preBalanceTx pabConf.pcProtocolParams minUtxos fees utxoIndex ownPkh privKeys requiredSigs tx

--       if balancedTx == tx
--         then pure balancedTx
--         else loop utxoIndex privKeys requiredSigs minUtxos balancedTx

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
      inputValue =
        Array.foldMap
          getAmount
          (Array.mapMaybe (flip Map.lookup utxos) <<< _.inputs $ txBody)

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
          tx' <- buildTxRaw txBody'
          fees' <- calculateMinFee tx' -- fees should increase.
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

buildTxRaw :: TxBody -> Either String Transaction
buildTxRaw = undefined

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
dataHashSize :: Maybe String -> BigInt -- Should we add type safety?
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
    -- FIX ME: Is this correct? The formula is actually based on the length of the
    -- bytestring representation, but we are using strings.
    -- | Sum of the length of the strings of distinct token names.
    sumTokenNameLengths :: Value -> BigInt
    sumTokenNameLengths = Foldable.foldl lenAdd zero <<< allTokenNames
      where
        lenAdd :: BigInt -> TokenName -> BigInt
        lenAdd = \c a -> c + fromInt (length $ unwrap a)

preBalanceTxBody
  :: MinUtxos
  -> BigInt
  -> Utxo
  -> Address
  -> Map.Map Address RequiredSigner
  -> Array Address
  -> TxBody
  -> Either String TxBody
preBalanceTxBody minUtxos fees utxos ownAddr addReqSigners requiredAddrs txBody =
  addTxCollaterals utxos txBody -- Take a single Ada only utxo collateral
    >>= balanceTxIns utxos fees -- Add input fees for the Ada only collateral
    >>= balanceNonAdaOuts ownAddr utxos
    >>= pure <<< addLovelaces minUtxos
    >>= balanceTxIns utxos fees -- Adding more inputs if required
    >>= balanceNonAdaOuts ownAddr utxos
    >>= addSignatories ownAddr addReqSigners requiredAddrs
    -- requiredAddrs are required signatures

-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
{- | Pick a collateral from the utxo map and add it to the unbalanced transaction
 (suboptimally we just pick a random utxo from the tx inputs)
-}
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
  -- FIX ME? Original code uses Set append which is union. Array unions behave
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
      not (Array.null txIns')
        && (txInsValue txIns') `geq` value

    -- FIX ME? Could refactor into a function as used in balanceNonAdaOuts
    txInsValue :: Array TransactionInput -> Value
    txInsValue =
      Array.foldMap getAmount <<< Array.mapMaybe (flip Map.lookup utxos)

-- FIX ME: toEitherTransactionInput may need fixing depending on our data types.
utxosToTransactionInput :: Utxo -> Array TransactionInput
utxosToTransactionInput =
  Array.mapMaybe (hush <<< toEitherTransactionInput) <<< Map.toUnfoldable

-- FIX ME: (payment credential) address for change substitute for pkh (Address)
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
-- | We need to balance non ada values, as the cardano-cli is unable to balance
-- | them (as of 2021/09/24). FIX ME: We aren't using CLI so need to balance ada
-- | values too.
balanceNonAdaOuts :: Address -> Utxo -> TxBody -> Either String TxBody
balanceNonAdaOuts changeAddr utxos (TxBody txBody) =
  let  -- FIX ME: Similar to Address issue, need pkh.
      payCredentials :: Credential
      payCredentials = addressPaymentCredentials changeAddr

      -- FIX ME: once both BaseAddresses are merged into one.
      -- pkh :: PubKeyHash
      -- pkh = addressPubKeyHash (unwrap changeAddr)."AddrType"

      txOutputs :: Array TransactionOutput
      txOutputs = txBody.outputs

      inputValue :: Value
      inputValue =
        Array.foldMap
          getAmount
          (Array.mapMaybe (flip Map.lookup utxos) <<< _.inputs $ txBody)

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
            ((==) payCredentials <<< txOutPaymentCredentials)
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

-- From https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
{- | Add the required signatories to the  Be aware if the signature
itself is invalid, and will be ignored. Only the pub key hashes are used,
mapped to signing key files on disk.
-}
addSignatories
  :: Address
  -> Map.Map Address RequiredSigner
  -> Array Address
  -> TxBody
  -> Either String TxBody
addSignatories ownAddr addReqSigners requiredAddrs txBody =
  Array.foldM
    ( \txBody' addr ->
        case Map.lookup addr addReqSigners of
          Just reqSigner -> pure $ txBody' `signBy` reqSigner
          Nothing -> Left "addSignatories: Signing key not found."
    )
    txBody
    $ Array.cons ownAddr requiredAddrs

signBy :: TxBody -> RequiredSigner -> TxBody
signBy (TxBody txBody) reqSigner =
  wrap $ txBody # case txBody.required_signers of
    Just xs ->
      _{ required_signers = Just $ reqSigner `Array.cons` xs }
    Nothing ->
      _{ required_signers = Just $ [reqSigner] }
