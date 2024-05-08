module Ctl.Internal.UseConstraints where

import Data.Lens
import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types
  ( Certificate
      ( StakeDelegation
      , PoolRetirement
      , PoolRegistration
      , StakeDeregistration
      , StakeRegistration
      )
  , DataHash
  , NetworkId
  , PlutusData
  , PlutusScript
  , ScriptHash
  , ScriptRef(NativeScriptRef, PlutusScriptRef)
  , StakeCredential(StakeCredential)
  , Transaction
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionUnspentOutput(TransactionUnspentOutput)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Value(Value)
  )
import Cardano.Types.Address
  ( Address(BaseAddress, EnterpriseAddress)
  , getPaymentCredential
  )
import Cardano.Types.Coin as Coin
import Cardano.Types.CostModel (CostModel(..))
import Cardano.Types.Credential
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  , asScriptHash
  )
import Cardano.Types.Credential as Credential
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.DataHash as Datum
import Cardano.Types.DataHash as PlutusData
import Cardano.Types.Int as Int
import Cardano.Types.Language (Language)
import Cardano.Types.Mint as Mint
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash, OutputDatum))
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionBody (TransactionBody(..))
import Contract.Constraints
  ( Constraint(..)
  , Constraints
  , DatumWitness(..)
  , RefInputAction(..)
  , ScriptWitness(..)
  , SpendWitness(..)
  )
import Contract.Log (logWarn')
import Control.Monad (unless)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (Except)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State (StateT(..))
import Control.Monad.State.Trans (get, gets, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.BalanceTx.RedeemerIndex
  ( RedeemerPurpose(ForReward, ForCert, ForMint, ForSpend)
  , UnindexedRedeemer(UnindexedRedeemer)
  , unindexedRedeemerToRedeemer
  )
import Ctl.Internal.Contract (getProtocolParameters)
import Ctl.Internal.Contract.Monad (Contract, getQueryHandle, wrapQueryM)
import Ctl.Internal.Helpers (liftEither, liftM, unsafeFromJust)
import Ctl.Internal.Lens
  ( _address
  , _body
  , _certs
  , _datum
  , _inputs
  , _isValid
  , _mint
  , _nativeScripts
  , _networkId
  , _output
  , _outputs
  , _plutusData
  , _plutusScripts
  , _referenceInputs
  , _requiredSigners
  , _scriptDataHash
  , _withdrawals
  , _witnessSet
  )
import Ctl.Internal.ProcessConstraints.Error
  ( MkUnbalancedTxError
      ( CannotSatisfyAny
      , CannotWithdrawRewardsNativeScript
      , CannotWithdrawRewardsPlutusScript
      , CannotWithdrawRewardsPubKey
      , DatumWrongHash
      , CannotMintZero
      , ExpectedPlutusScriptGotNativeScript
      , CannotFindDatum
      , CannotQueryDatum
      , CannotGetValidatorHashFromAddress
      , TxOutRefWrongType
      , CannotConvertPOSIXTimeRange
      , NumericOverflow
      , WrongRefScriptHash
      , ValidatorHashNotFound
      , MintingPolicyNotFound
      , DatumNotFound
      , TxOutRefNotFound
      , CannotSolveTimeConstraints
      , OwnPubKeyAndStakeKeyMissing
      )
  )
import Ctl.Internal.ProcessConstraints.UnbalancedTx (UnbalancedTx)
import Ctl.Internal.QueryM.Pools
  ( getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  )
import Ctl.Internal.Service.Error (ClientError, pprintClientError)
import Ctl.Internal.Transaction
  ( attachDatum
  , attachNativeScript
  , attachPlutusScript
  , setScriptDataHash
  )
import Ctl.Internal.Types.Interval
  ( POSIXTimeRange
  , always
  , intersection
  , isEmpty
  , posixTimeRangeToTransactionValidity
  )
import Ctl.Internal.Types.ScriptLookups (ScriptLookups)
import Ctl.Internal.Types.Val as Val
import Data.Array (cons, nub, partition, toUnfoldable, zip)
import Data.Array (mapMaybe, singleton, (:)) as Array
import Data.Bifunctor (lmap)
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either(Left, Right), either, hush, isRight, note)
import Data.Foldable (foldM)
import Data.Lens (Lens', (%=), (%~), (.=), (.~), (<>=))
import Data.Lens.Getter (to, use)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(Nil, Cons))
import Data.Map (Map, empty, fromFoldable, lookup, union)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, isJust, maybe)
import Data.Newtype (over, unwrap, wrap)
import Data.Set as Set
import Data.Traversable (for, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)
import Prelude (join) as Bind
import Type.Proxy (Proxy(..))

type Context =
  { transaction :: Transaction
  , costModels :: Map Language CostModel
  , redeemers :: Array UnindexedRedeemer
  , datums :: Array PlutusData
  }

_transaction
  :: Lens' Context Transaction
_transaction = prop (Proxy :: Proxy "transaction")

_redeemers
  :: Lens' Context (Array UnindexedRedeemer)
_redeemers = prop (Proxy :: Proxy "redeemers")

_datums
  :: Lens' Context (Array PlutusData)
_datums = prop (Proxy :: Proxy "datums")

data ExpectedOutputType = ScriptHashOutput | PubKeyHashOutput

data TxBuildError
  = WrongSpendWitnessType TransactionUnspentOutput
  | QueryError ClientError
  | DatumHashLookupError DataHash
  | IncorrectDatumHash TransactionUnspentOutput PlutusData DataHash
  | WrongOutputType ExpectedOutputType TransactionUnspentOutput

explainTxBuildError :: TxBuildError -> String
explainTxBuildError (WrongSpendWitnessType utxo) =
  "SpendWitness is incompatible with the given output. The output does not contain a datum: "
    <> show utxo
explainTxBuildError (QueryError clientError) =
  "Query error: " <> pprintClientError clientError
explainTxBuildError (DatumHashLookupError dataHash) =
  "The UTxO you are trying to spend contains a datum hash. You didn't provide a `DatumWitness` value corresponding to this hash, so CTL tried to look it up, using a database of datums observed on-chain. This lookup failed. Datum hash: "
    <> show dataHash
explainTxBuildError (IncorrectDatumHash utxo datum datumHash) =
  "You provided a `DatumWitness` with a datum that does not match the datum hash present in a transaction output.\n  Datum: "
    <> show datum
    <> " (CBOR: "
    <> byteArrayToHex (unwrap $ encodeCbor datum)
    <> ")\n  Datum hash: "
    <> byteArrayToHex (unwrap $ encodeCbor datumHash)
    <> "\n  UTxO: "
    <> show utxo
explainTxBuildError _ = "TODO"

type M a = StateT Context (ExceptT TxBuildError Contract) a

processConstraints :: Constraints -> M Unit
processConstraints = traverse_ processConstraint

processConstraint :: Constraint -> M Unit
processConstraint = case _ of
  SpendOutput utxo spendWitness -> do
    _transaction <<< _body <<< _inputs
      %= pushUnique (unwrap utxo).input
    useSpendWitness utxo spendWitness
  RequireSignature ppkh -> do
    _transaction <<< _body <<< _requiredSigners <>=
      [ wrap $ unwrap $ unwrap ppkh ]
  _ -> pure unit

assertOutputType :: ExpectedOutputType -> TransactionUnspentOutput -> M Unit
assertOutputType outputType utxo = do
  let
    mbCredential =
      (getPaymentCredential (utxo ^. _output <<< _address) <#> unwrap)
        >>= case outputType of
          ScriptHashOutput -> Credential.asScriptHash >>> void
          PubKeyHashOutput -> Credential.asPubKeyHash >>> void
  unless (isJust mbCredential) do
    throwError $ WrongOutputType outputType utxo

-- | Tries to modify the transaction to make it consume a given output.
-- | Uses a `SpendWitness` to try to satisfy spending requirements.
useSpendWitness :: TransactionUnspentOutput -> SpendWitness -> M Unit
useSpendWitness utxo = case _ of
  PubKeyOutput -> do
    assertOutputType PubKeyHashOutput utxo
  NativeScriptOutput nsWitness -> do
    assertOutputType ScriptHashOutput utxo
    -- attach the script
    case nsWitness of
      ScriptValue ns -> do
        _transaction <<< _witnessSet <<< _nativeScripts
          %= pushUnique ns
      ScriptReference refInput refInputAction -> do
        _transaction <<< _body <<< refInputActionToLens refInputAction
          %= pushUnique refInput
  PlutusScriptOutput plutusScriptWitness redeemerDatum mbDatumWitness -> do
    assertOutputType ScriptHashOutput utxo
    -- attach the script
    case plutusScriptWitness of
      ScriptValue ps -> do
        _transaction <<< _witnessSet <<< _plutusScripts
          %= pushUnique ps
      ScriptReference input action -> do
        _transaction <<< _body <<< refInputActionToLens action
          %= pushUnique input
    -- attach the datum
    useDatumWitnessForUtxo utxo mbDatumWitness
    -- attach the redeemer
    let
      uiRedeemer = UnindexedRedeemer
        { purpose: ForSpend (unwrap utxo).input
        , datum: unwrap redeemerDatum
        }
    _redeemers %= pushUnique uiRedeemer

-- | Tries to modify the transaction state to make it consume a given script output.
-- | Uses a `DatumWitness` if the UTxO datum is provided as a hash.
useDatumWitnessForUtxo
  :: TransactionUnspentOutput -> Maybe DatumWitness -> M Unit
useDatumWitnessForUtxo utxo datumWitness = do
  case utxo ^. _output <<< _datum of
    -- script outputs must have a datum
    Nothing -> throwError $ WrongSpendWitnessType utxo
    -- if the datum is inline, we don't need to attach it as witness
    Just (OutputDatum providedDatum) -> do
      when (isJust datumWitness) do
        logWarn' $
          "You've provided an optional `DatumWitness` in `PlutusScriptWitness`, but the output you are spending already contains an inline datum (not just a datum hash). You can omit the provided datum witness. You provided: "
            <> show providedDatum
    -- if the datum is provided as hash,
    Just (OutputDatumHash datumHash) ->
      case datumWitness of
        -- if the datum witness was not provided, look the datum up
        Nothing -> do
          queryHandle <- lift $ lift $ getQueryHandle
          mbDatum <- liftEither =<< lmap QueryError <$> do
            liftAff $ queryHandle.getDatumByHash datumHash
          datum <- liftM (DatumHashLookupError datumHash) mbDatum
          _datums %= pushUnique datum
          _transaction <<< _witnessSet <<< _plutusData %= pushUnique datum
        -- if the datum was provided, check it's hash. if it matches the one
        -- specified in the output, use that datum.
        Just (DatumValue providedDatum)
          | datumHash == PlutusData.hashPlutusData providedDatum -> do
              _datums %= pushUnique providedDatum
              _transaction <<< _witnessSet <<< _plutusData
                %= pushUnique providedDatum
        -- otherwise, fail
        Just (DatumValue providedDatum) -> do
          throwError $ IncorrectDatumHash utxo providedDatum datumHash
        -- if a reference input is provided, we just attach it.
        -- TODO: consider querying for the inline datum to check if it matches.
        Just (DatumReference datumWitnessRef refInputAction) -> do
          _transaction <<< _body <<< refInputActionToLens refInputAction
            %= pushUnique datumWitnessRef

-- | Depending on `RefInputAction` value, we either want to spend a reference
-- | UTxO, or just reference it.
refInputActionToLens
  :: RefInputAction
  -> Lens' TransactionBody (Array TransactionInput)
refInputActionToLens =
  case _ of
    ReferenceInput -> _referenceInputs
    SpendInput -> _inputs

-- | Ensures uniqueness of an element
pushUnique :: forall a. Ord a => a -> Array a -> Array a
pushUnique x xs = nub $ xs <> [ x ]

-- Ensures uniqueness
appendInput
  :: TransactionInput -> Array TransactionInput -> Array TransactionInput
appendInput a b = Set.toUnfoldable (Set.singleton a <> Set.fromFoldable b)
