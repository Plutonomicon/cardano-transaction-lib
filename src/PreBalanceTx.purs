module PreBalanceTx
  ( preBalanceTxBody
  , preBalanceTxM
  )
  where

import Prelude
import Control.Monad.Reader.Trans (runReaderT)
import Data.Array ((\\))
import Data.Array as Array
import Data.BigInt (BigInt, fromInt, quot)
import Data.Either (Either(..), hush, note)
import Data.Foldable as Foldable
import Data.List ((:), List(..), partition)
import Data.Map as Map
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Newtype (over, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (length)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Aff (Aff)
import Undefined (undefined)

import Ogmios (QueryConfig, QueryM)
import ProtocolParametersAlonzo (coinSize, lovelacePerUTxOWord, pidSize, protocolParamUTxOCostPerWord, utxoEntrySizeWithoutVal)
import Types.Ada (adaSymbol, fromValue, getLovelace, lovelaceValueOf)
import Types.Transaction (Address, Credential(..), RequiredSigner, Transaction(..), TransactionInput, TransactionOutput(..), TxBody(..), Utxo, UtxoM)
import Types.Value (allTokenNames, emptyValue, flattenValue, geq, getValue, isAdaOnly, isPos, isZero, minus, numCurrencySymbols, numTokenNames, TokenName, Value(..))

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs

-- TO DO: convert utxosAt from Ogmios to Transaction space.
utxosAt :: Address -> QueryM UtxoM
utxosAt = undefined

preBalanceTxM
  :: QueryConfig
  -> Address
  -> Map.Map Address RequiredSigner -- FIX ME: take from unbalanced tx?
  -> Array Address -- FIX ME: take from unbalanced tx?
  -> Transaction -- unbalanced transaction, FIX ME: do we need a newtype wrapper?
  -> Aff (Either String Transaction)
preBalanceTxM qConfig ownAddr addReqSigners requiredAddrs unbalancedTx =
  runReaderT
    do
      utxos <- unwrap <$> utxosAt ownAddr -- Do we want :: Either String UtxoM here?
      let utxoIndex = utxos  -- FIX ME: include newtype wrapper? UNWRAP
          _unwrapUnbalancedTx = unwrap unbalancedTx
      loop utxoIndex ownAddr addReqSigners requiredAddrs [] unbalancedTx
  qConfig
  where
    loop ::
      Utxo ->
      Address ->
      Map.Map Address RequiredSigner ->
      Array Address ->
      Array (TransactionOutput /\ BigInt) ->
      Transaction ->
      QueryM (Either String Transaction)
    loop
      utxoIndex'
      ownAddr'
      addReqSigners'
      requiredAddrs'
      prevMinUtxos'
      (Transaction unwrapTx') = do
      let txBody' :: TxBody
          txBody' = unwrapTx'.body

          unwrapTxBody' = unwrap txBody'

          nextMinUtxos' :: Array (TransactionOutput /\ BigInt)
          nextMinUtxos' =
            calculateMinUtxos $ unwrapTxBody'.outputs \\ map fst prevMinUtxos'

          minUtxos' :: Array (TransactionOutput /\ BigInt)
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
           then pure $ Right $ wrap unwrapTx' { body = balancedTxBody'' }
           else
            loop
              utxoIndex'
              ownAddr'
              addReqSigners'
              requiredAddrs'
              prevMinUtxos'
              $ wrap unwrapTx' { body = balancedTxBody'' }

    chainedBalancer
      :: Array (TransactionOutput /\ BigInt)
      -> Utxo
      -> Address
      -> Map.Map Address RequiredSigner
      -> Array Address
      -> TxBody
      -> Either String TxBody
    chainedBalancer
      -- FIX ME: Original code writes to disk
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
        tx' :: Transaction <- buildTx txBodyWithoutFees'
        fees' :: BigInt <- calculateMinFee tx' -- FIX ME: use txBodyWithoutFees replaced in original tx.
        preBalanceTxBody
          minUtxos'
            fees'
            utxoIndex'
            ownAddr'
            addReqSigners'
            requiredAddrs'
            txBody'

      -- pure $ Right tx

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

-- -- https://github.com/input-output-hk/cardano-ledger/blob/master/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Tools.hs
-- -- | Checks all inputs are contained in utxos.
-- basicValidation
--   :: Transaction
--   -- | The current UTxO set (or the relevant portion for the transaction).
--   -> Utxo
--   -> Maybe String
-- basicValidation tx utxo =
--   if Array.null badInputs
--    then Nothing
--    else Just $ "The following inputs are not part of utxo map" <> show badInputs
--   where
--     txInputs :: Array TransactionInput
--     txInputs = _.inputs <<< unwrap <<< _.body <<< unwrap $ tx

--     badInputs :: Array TransactionInput
--     badInputs = Array.filter (not <<< flip Map.member utxo) txInputs

buildTx :: TxBody -> Either String Transaction
buildTx = undefined

calculateMinFee :: Transaction -> Either String BigInt
calculateMinFee = undefined

calculateMinUtxos
  :: Array TransactionOutput
  -> Array (TransactionOutput /\ BigInt)
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
      in case isAdaOnly outputValue of
            true -> utxoEntrySizeWithoutVal + coinSize -- 29 in Alonzo
            false -> utxoEntrySizeWithoutVal
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
  :: Array (TransactionOutput /\ BigInt)
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
addTxCollaterals utxos txBody = do
  let txIns :: Array TransactionInput
      txIns = utxosToTransactionInput $ filterAdaOnly utxos
  txIn :: TransactionInput <- findPubKeyTxIn txIns
  pure $
    over TxBody _{ collateral = Just (Array.singleton txIn) } txBody
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
balanceTxIns utxos fees txBody = do
  let unwrapTxBody = unwrap txBody

      utxoCost :: BigInt
      utxoCost = getLovelace protocolParamUTxOCostPerWord

      -- An ada-only UTxO entry is 29 words. More details about min utxo
      -- calculation can be found here:
      -- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0028#rationale-for-parameter-choices
      changeMinUtxo :: BigInt
      changeMinUtxo = (fromInt 29) * utxoCost

      txOutputs :: Array TransactionOutput
      txOutputs = unwrapTxBody.outputs

      nonMintedValue :: Value
      nonMintedValue =
        Array.foldMap getAmount txOutputs
          `minus` fromMaybe emptyValue unwrapTxBody.mint

      minSpending :: Value
      minSpending = lovelaceValueOf (fees + changeMinUtxo) <> nonMintedValue

  txIns :: Array TransactionInput <-
    collectTxIns unwrapTxBody.inputs utxos minSpending
  -- FIX ME? Original code uses Set append which is union so we use this then
  -- convert back to arrays. We could maybe use Array.union depending on _.inputs.
  -- This would mean using just Arrays for collectTxIns.
  pure $ wrap
    unwrapTxBody
      { inputs =
          Set.toUnfoldable
            (Set.fromFoldable txIns <> Set.fromFoldable unwrapTxBody.inputs)
      }

-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
-- | Getting the necessary input utxos to cover the fees for the transaction
collectTxIns
  :: Array TransactionInput
  -> Utxo
  -> Value
  -> Either String (Array TransactionInput)
collectTxIns originalTxIns utxos value =
  if isSufficient $ Set.fromFoldable updatedInputs
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
      Set.toUnfoldable $ Foldable.foldl
        ( \newTxIns txIn ->
            if isSufficient newTxIns
             then newTxIns
             else Set.insert txIn newTxIns -- set insertion in original code.
             -- Could use another if then else with `Array.elem`.
        )
        (Set.fromFoldable originalTxIns)
        (Set.fromFoldable $ utxosToTransactionInput utxos)

    isSufficient :: Set TransactionInput -> Boolean
    isSufficient txIns' =
      not (Set.isEmpty txIns')
        && (txInsValue $ Set.toUnfoldable txIns') `geq` value

    -- FIX ME? Could refactor into a function as used in balanceNonAdaOuts
    -- Use Array so we don't need Ord instance on TransactionOutput from
    -- Set.mapMaybe - we don't want an Ord instance on Value.
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
balanceNonAdaOuts changeAddr utxos txBody =
  let unwrapTxBody = unwrap txBody

      -- FIX ME: Similar to Address issue, need pkh.
      payCredentials :: Credential
      payCredentials = addressPaymentCredentials changeAddr

      txOutputs :: Array TransactionOutput
      txOutputs = unwrapTxBody.outputs

      inputValue :: Value
      inputValue =
        Array.foldMap
          getAmount
          (Array.mapMaybe (flip Map.lookup utxos) <<< _.inputs $ unwrapTxBody)

      outputValue :: Value
      outputValue = Array.foldMap getAmount txOutputs

      nonMintedOutputValue :: Value
      nonMintedOutputValue =
        outputValue `minus` fromMaybe emptyValue unwrapTxBody.mint

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
       then pure $ wrap unwrapTxBody { outputs = outputs }
       else
        if isZero nonAdaChange
         then pure txBody
         else Left "balanceNonAdaOuts: Not enough inputs to balance tokens."

getAmount :: TransactionOutput -> Value
getAmount = _.amount <<< unwrap

-- | Add min lovelaces to each tx output
addLovelaces :: Array (TransactionOutput /\ BigInt) -> TxBody -> TxBody
addLovelaces minLovelaces txBody =
  let unwrapTxBody = unwrap txBody

      lovelacesAdded :: Array TransactionOutput
      lovelacesAdded =
        map
          ( \txOut ->
              let unwrapTxOut = unwrap txOut

                  outValue :: Value
                  outValue = unwrapTxOut.amount

                  lovelaces :: BigInt
                  lovelaces = getLovelace $ fromValue outValue

                  minUtxo :: BigInt
                  minUtxo = fromMaybe zero $ Foldable.lookup txOut minLovelaces
              in wrap
                  unwrapTxOut
                    { amount =
                        outValue
                          <> lovelaceValueOf (max zero $ minUtxo - lovelaces)
                    }
           ) unwrapTxBody.outputs
   in wrap unwrapTxBody { outputs = lovelacesAdded }

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
signBy txBody reqSigner =
  let unwrapTxBody = unwrap txBody
   in wrap $ unwrapTxBody # case unwrapTxBody.required_signers of
        Just xs ->
          _{ required_signers = Just $ reqSigner `Array.cons` xs }
        Nothing ->
          _{ required_signers = Just $ [reqSigner] }
