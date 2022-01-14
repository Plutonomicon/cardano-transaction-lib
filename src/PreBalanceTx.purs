module PreBalanceTx
  ( preBalanceTx
  ) where

import Prelude
import Data.Array as Array
import Data.BigInt (BigInt, fromInt)
import Data.Either (Either(..), hush, note)
import Data.Foldable as Foldable
import Data.List ((:), List(..), partition)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (over, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\), type (/\))

import Ada (adaSymbol, fromValue, getLovelace, lovelaceValueOf)
import ProtocolParametersAlonzo (protocolParamUTxOCostPerWord)
import Types.Transaction as Transaction
import Value (emptyValue, flattenValue, geq, getValue, isAdaOnly, isPos, isZero, minus)

-- This module replicates functionality from
-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs

preBalanceTx ::
  Array (Transaction.TransactionOutput /\ BigInt) ->
  BigInt ->
  Transaction.Utxo ->
  Transaction.Address ->
  Map.Map Transaction.Address Transaction.RequiredSigner ->
  Array Transaction.Address ->
  Transaction.TxBody ->
  Either String Transaction.TxBody
preBalanceTx minUtxos fees utxos ownAddr addReqSigners requiredAddrs tx =
  addTxCollaterals utxos tx -- Take a single Ada only utxo collateral
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
addTxCollaterals
  :: Transaction.Utxo
  -> Transaction.TxBody
  -> Either String Transaction.TxBody
addTxCollaterals utxos txBody = do
  let txIns :: Array Transaction.TransactionInput
      txIns = utxosToTransactionInput $ filterAdaOnly utxos
  txIn :: Transaction.TransactionInput <- findPubKeyTxIn txIns
  pure $
    over Transaction.TxBody _{ collateral = Just (Array.singleton txIn) } txBody
  where
    filterAdaOnly :: Transaction.Utxo -> Transaction.Utxo
    filterAdaOnly = Map.filter (isAdaOnly <<< getAmount)

    -- FIX ME: Plutus has Maybe TxInType e.g. Just ConsumePublicKeyAddress)
    -- for now, we take the head. The Haskell logic is pasted below:
    -- findPubKeyTxIn = \case
    --   x@(TxIn _ (Just ConsumePublicKeyAddress)) : _ -> Right x
    --   x@(TxIn _ Nothing) : _ -> Right x
    --   _ : xs -> findPubKeyTxIn xs
    --   _ -> Left "There are no utxos to be used as collateral"
    findPubKeyTxIn
      :: Array Transaction.TransactionInput
      -> Either String Transaction.TransactionInput
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
  :: (Transaction.TransactionInput /\ Transaction.TransactionOutput)
  -> Either String Transaction.TransactionInput
toEitherTransactionInput (txOutRef /\ txOut) =
  case txOutPaymentCredentials txOut of
    -- FIX ME: need to determine it's a pubkey credential as opposed to script
    -- credential.
    Transaction.Credential _ ->
      pure txOutRef
    _ -> -- Currently unreachable:
      Left "toEitherTransactionInput: Cannot convert an output to \
        \TransactionInput"

addressPaymentCredentials :: Transaction.Address -> Transaction.Credential
addressPaymentCredentials = _.payment <<< unwrap <<< _."AddrType" <<< unwrap

-- FIX ME: do we need granularity for staking credential? We need pkh?
txOutPaymentCredentials
  :: Transaction.TransactionOutput
  -> Transaction.Credential
txOutPaymentCredentials = addressPaymentCredentials <<< _.address  <<< unwrap

-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
-- Notice we aren't using protocol parameters for utxo cost per word.
balanceTxIns
  :: Transaction.Utxo
  -> BigInt
  -> Transaction.TxBody
  -> Either String Transaction.TxBody
balanceTxIns utxos fees txBody = do
  let unwrapTxBody = unwrap txBody

      utxoCost :: BigInt
      utxoCost = getLovelace protocolParamUTxOCostPerWord

      -- An ada-only UTxO entry is 29 words. More details about min utxo
      -- calculation can be found here:
      -- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0028#rationale-for-parameter-choices
      changeMinUtxo :: BigInt
      changeMinUtxo = (fromInt 29) * utxoCost

      txOutputs :: Array Transaction.TransactionOutput
      txOutputs = unwrapTxBody.outputs

      nonMintedValue :: Transaction.Value
      nonMintedValue =
        Array.foldMap getAmount txOutputs
          `minus` fromMaybe emptyValue unwrapTxBody.mint

      minSpending :: Transaction.Value
      minSpending = lovelaceValueOf (fees + changeMinUtxo) <> nonMintedValue

  txIns :: Array Transaction.TransactionInput
    <- collectTxIns unwrapTxBody.inputs utxos minSpending
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
  :: Array Transaction.TransactionInput
  -> Transaction.Utxo
  -> Transaction.Value
  -> Either String (Array Transaction.TransactionInput)
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
    updatedInputs :: Array Transaction.TransactionInput
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

    isSufficient :: Set Transaction.TransactionInput -> Boolean
    isSufficient txIns' =
      not (Set.isEmpty txIns')
        && (txInsValue $ Set.toUnfoldable txIns') `geq` value

    -- FIX ME? Could refactor into a function as used in balanceNonAdaOuts
    -- Use Array so we don't need Ord instance on TransactionOutput from
    -- Set.mapMaybe - we don't want an Ord instance on Value.
    txInsValue :: Array Transaction.TransactionInput -> Transaction.Value
    txInsValue =
      Array.foldMap getAmount <<< Array.mapMaybe (flip Map.lookup utxos)

-- FIX ME: toEitherTransactionInput may need fixing depending on our data types.
utxosToTransactionInput
  :: Transaction.Utxo
  -> Array Transaction.TransactionInput
utxosToTransactionInput =
  Array.mapMaybe (hush <<< toEitherTransactionInput) <<< Map.toUnfoldable

-- https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
-- | We need to balance non ada values, as the cardano-cli is unable to balance
-- | them (as of 2021/09/24). FIX ME: We aren't using CLI so need to balance ada
-- | values too.
balanceNonAdaOuts
  :: Transaction.Address -- FIX ME: (payment credential) address for change substitute for pkh.
  -> Transaction.Utxo
  -> Transaction.TxBody
  -> Either String Transaction.TxBody
balanceNonAdaOuts changeAddr utxos txBody =
  let unwrapTxBody = unwrap txBody

      -- FIX ME: Similar to Transaction.Address issue, need pkh.
      payCredentials :: Transaction.Credential
      payCredentials = addressPaymentCredentials changeAddr

      txOutputs :: Array Transaction.TransactionOutput
      txOutputs = unwrapTxBody.outputs

      inputValue :: Transaction.Value
      inputValue =
        Array.foldMap
          getAmount
          (Array.mapMaybe (flip Map.lookup utxos) <<< _.inputs $ unwrapTxBody)

      outputValue :: Transaction.Value
      outputValue = Array.foldMap getAmount txOutputs

      nonMintedOutputValue:: Transaction.Value
      nonMintedOutputValue =
        outputValue `minus` fromMaybe emptyValue unwrapTxBody.mint

      nonAdaChange :: Transaction.Value
      nonAdaChange =
        filterNonAda inputValue `minus` filterNonAda nonMintedOutputValue

      outputs :: Array Transaction.TransactionOutput
      outputs =
        Array.fromFoldable $
          case partition
            ((==) payCredentials <<< txOutPaymentCredentials)
            $ Array.toUnfoldable txOutputs of
              { no: txOuts, yes: Nil } ->
                Transaction.TransactionOutput
                  { address: changeAddr,
                    amount: nonAdaChange,
                    data_hash: Nothing
                  } : txOuts
              { no: txOuts'
              , yes: Transaction.TransactionOutput txOut@{ amount: v } : txOuts
              } ->
                Transaction.TransactionOutput
                  txOut { amount = v <> nonAdaChange } : txOuts <> txOuts'

   -- Original code uses "isNat" because there is a guard against zero, see
   -- isPos for more detail.
   in if isPos nonAdaChange
       then pure $ wrap unwrapTxBody { outputs = outputs }
       else
        if isZero nonAdaChange
         then pure txBody
         else Left "balanceNonAdaOuts: Not enough inputs to balance tokens."

getAmount :: Transaction.TransactionOutput -> Transaction.Value
getAmount = _.amount <<< unwrap

-- | Add min lovelaces to each tx output
addLovelaces
  :: Array (Transaction.TransactionOutput /\ BigInt)
  -> Transaction.TxBody
  -> Transaction.TxBody
addLovelaces minLovelaces txBody =
  let unwrapTxBody = unwrap txBody

      lovelacesAdded :: Array Transaction.TransactionOutput
      lovelacesAdded =
        map
          ( \txOut ->
              let unwrapTxOut = unwrap txOut

                  outValue :: Transaction.Value
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
filterNonAda :: Transaction.Value -> Transaction.Value
filterNonAda =
  Transaction.Value <<< Map.filterKeys (_ /= adaSymbol) <<< getValue

-- From https://github.com/mlabs-haskell/mlabs-pab/blob/master/src/MLabsPAB/PreBalance.hs
{- | Add the required signatories to the transaction. Be aware if the signature
itself is invalid, and will be ignored. Only the pub key hashes are used,
mapped to signing key files on disk.
-}
addSignatories
  :: Transaction.Address
  -> Map.Map Transaction.Address Transaction.RequiredSigner
  -> Array Transaction.Address
  -> Transaction.TxBody
  -> Either String Transaction.TxBody
addSignatories ownAddr addReqSigners requiredAddrs txBody =
  Array.foldM
    ( \txBody' addr ->
        case Map.lookup addr addReqSigners of
          Just reqSigner -> pure $ txBody' `signBy` reqSigner
          Nothing -> Left "addSignatories: Signing key not found."
    )
    txBody
    $ Array.cons ownAddr requiredAddrs

signBy :: Transaction.TxBody -> Transaction.RequiredSigner -> Transaction.TxBody
signBy txBody reqSigner =
  let unwrapTxBody = unwrap txBody
   in wrap $ unwrapTxBody # case unwrapTxBody.required_signers of
        Just xs ->
          _{ required_signers = Just $ reqSigner `Array.cons` xs }
        Nothing ->
          _{ required_signers = Just $ [reqSigner] }