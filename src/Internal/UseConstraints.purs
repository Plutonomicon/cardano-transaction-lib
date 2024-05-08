module Ctl.Internal.UseConstraints where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types
  ( DataHash
  , NetworkId
  , PlutusData
  , PlutusScript
  , StakeCredential
  , Transaction
  , TransactionInput
  , TransactionUnspentOutput
  )
import Cardano.Types.Address (getPaymentCredential)
import Cardano.Types.Certificate (Certificate(..))
import Cardano.Types.Coin (Coin)
import Cardano.Types.CostModel (CostModel)
import Cardano.Types.Credential as Credential
import Cardano.Types.DataHash as PlutusData
import Cardano.Types.Language (Language)
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash, OutputDatum))
import Cardano.Types.TransactionBody (TransactionBody)
import Contract.Constraints
  ( Constraint(..)
  , Constraints
  , CredentialWitness(..)
  , DatumWitness(..)
  , OutputWitness(..)
  , RefInputAction(..)
  , ScriptWitness(..)
  )
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (Except)
import Control.Monad.State (StateT)
import Control.Monad.State.Trans (gets)
import Ctl.Internal.BalanceTx.RedeemerIndex
  ( RedeemerPurpose(ForSpend, ForCert)
  , UnindexedRedeemer(UnindexedRedeemer)
  )
import Ctl.Internal.Lens
  ( _address
  , _body
  , _certs
  , _datum
  , _inputs
  , _nativeScripts
  , _output
  , _plutusData
  , _plutusScripts
  , _referenceInputs
  , _requiredSigners
  , _withdrawals
  , _witnessSet
  )
import Data.Array (nub)
import Data.ByteArray (byteArrayToHex)
import Data.Lens (Lens', (%=), (<>=), (^.))
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
import Data.Traversable (traverse_)
import Type.Proxy (Proxy(..))

type Context =
  { transaction :: Transaction
  , costModels :: Map Language CostModel
  , redeemers :: Array UnindexedRedeemer
  , datums :: Array PlutusData
  , networkId :: NetworkId
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

data ExpectedWitnessType = ScriptHashWitness | PubKeyHashWitness

explainExpectedWitnessType :: ExpectedWitnessType -> String
explainExpectedWitnessType ScriptHashWitness = "ScriptHash"
explainExpectedWitnessType PubKeyHashWitness = "PubKeyHash"

data CredentialAction = StakeCert | Withdrawal | Minting

explainCredentialAction :: CredentialAction -> String
explainCredentialAction StakeCert = "This stake certificate"
explainCredentialAction Withdrawal = "This stake rewards withdrawal"
explainCredentialAction Minting = "This mint"

data TxBuildError
  = WrongSpendWitnessType TransactionUnspentOutput
  | DatumHashLookupError DataHash
  | IncorrectDatumHash TransactionUnspentOutput PlutusData DataHash
  | WrongOutputType ExpectedWitnessType TransactionUnspentOutput
  | WrongStakeCredentialType CredentialAction ExpectedWitnessType
      StakeCredential
  | DatumWitnessNotProvided TransactionUnspentOutput
  | UnneededDatumWitness TransactionUnspentOutput DatumWitness

explainTxBuildError :: TxBuildError -> String
explainTxBuildError (WrongSpendWitnessType utxo) =
  "`OutputWitness` is incompatible with the given output. The output does not contain a datum: "
    <> show utxo
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
explainTxBuildError (WrongOutputType ScriptHashWitness utxo) =
  "The UTxO you provided requires a Script witness to unlock. UTxO: " <> show
    utxo
explainTxBuildError (WrongOutputType PubKeyHashWitness utxo) =
  "The UTxO you provided requires a PubKeyHash witness to unlock. UTxO: " <>
    show utxo
explainTxBuildError
  (WrongStakeCredentialType operation expWitnessType stakeCredential) =
  explainCredentialAction operation <> " requires a "
    <> explainExpectedWitnessType expWitnessType
    <> " witness: "
    <> show stakeCredential
explainTxBuildError (DatumWitnessNotProvided utxo) =
  "The UTxO you are trying to spend contains a datum hash. A matching `DatumWitness` is required. Use `getDatumByHash`. UTxO: "
    <> show utxo
explainTxBuildError (UnneededDatumWitness utxo witness) =
  "You've provided an optional `DatumWitness`, but the output you are spending already contains an inline datum (not just a datum hash). You should omit the provided datum witness. You provided: "
    <> show witness
    <> " for the UTxO: "
    <> show utxo
explainTxBuildError _ = "TODO"

type M a = StateT Context (Except TxBuildError) a

processConstraints :: Constraints -> M Unit
processConstraints = traverse_ processConstraint

processConstraint :: Constraint -> M Unit
processConstraint = case _ of
  SpendOutput utxo spendWitness -> do
    _transaction <<< _body <<< _inputs
      %= pushUnique (unwrap utxo).input
    useSpendWitness utxo spendWitness
  RegisterStake stakeCredential -> do
    _transaction <<< _body <<< _certs %= pushUnique
      (StakeRegistration stakeCredential)
  IssueCertificate cert witness -> do
    useCertificateWitness cert witness
  WithdrawStake stakeCredential amount witness -> do
    useWithdrawRewardsWitness stakeCredential amount witness
  RequireSignature ppkh -> do
    _transaction <<< _body <<< _requiredSigners <>=
      [ wrap $ unwrap $ unwrap ppkh ]
  RegisterPool poolParams -> do
    _transaction <<< _body <<< _certs %= pushUnique
      (PoolRegistration poolParams)
  RetirePool poolKeyHash epoch -> do
    _transaction <<< _body <<< _certs %= pushUnique
      (PoolRetirement { poolKeyHash, epoch })

assertOutputType :: ExpectedWitnessType -> TransactionUnspentOutput -> M Unit
assertOutputType outputType utxo = do
  let
    mbCredential =
      (getPaymentCredential (utxo ^. _output <<< _address) <#> unwrap)
        >>= case outputType of
          ScriptHashWitness -> Credential.asScriptHash >>> void
          PubKeyHashWitness -> Credential.asPubKeyHash >>> void
  unless (isJust mbCredential) do
    throwError $ WrongOutputType outputType utxo

assertStakeCredentialType
  :: CredentialAction -> ExpectedWitnessType -> StakeCredential -> M Unit
assertStakeCredentialType action expectedType credential = do
  let
    mbCredential =
      case expectedType of
        ScriptHashWitness ->
          void $ Credential.asScriptHash $ unwrap credential
        PubKeyHashWitness ->
          void $ Credential.asPubKeyHash $ unwrap credential
  unless (isJust mbCredential) do
    throwError $ WrongStakeCredentialType action expectedType credential

useCertificateWitness :: Certificate -> CredentialWitness -> M Unit
useCertificateWitness cert witness = do
  _transaction <<< _body <<< _certs %= pushUnique cert
  case cert of
    StakeDeregistration stakeCredential -> do
      useCertificateCredentialWitness cert witness
      useCredentialWitness StakeCert stakeCredential witness
    StakeDelegation stakeCredential _ -> do
      useCertificateCredentialWitness cert witness
      useCredentialWitness StakeCert stakeCredential witness
    StakeRegistration _ -> pure unit
    PoolRegistration _ -> pure unit
    PoolRetirement _ -> pure unit
    GenesisKeyDelegation _ -> pure unit
    MoveInstantaneousRewardsCert _ -> pure unit

useCertificateCredentialWitness :: Certificate -> CredentialWitness -> M Unit
useCertificateCredentialWitness cert witness = do
  case witness of
    PubKeyCredential -> pure unit
    NativeScriptCredential _nsWitness -> pure unit
    PlutusScriptCredential _plutusScriptWitness redeemerDatum -> do
      -- attach the redeemer
      let
        uiRedeemer = UnindexedRedeemer
          { purpose: ForCert cert
          , datum: unwrap redeemerDatum
          }
      _redeemers %= pushUnique uiRedeemer

useCredentialWitness
  :: CredentialAction -> StakeCredential -> CredentialWitness -> M Unit
useCredentialWitness credentialAction stakeCredential witness = do
  case witness of
    PubKeyCredential -> do
      assertStakeCredentialType credentialAction PubKeyHashWitness
        stakeCredential
    NativeScriptCredential nsWitness -> do
      assertStakeCredentialType credentialAction ScriptHashWitness
        stakeCredential
      useNativeScriptWitness nsWitness
    PlutusScriptCredential plutusScriptWitness _ -> do
      assertStakeCredentialType credentialAction ScriptHashWitness
        stakeCredential
      usePlutusScriptWitness plutusScriptWitness

useWithdrawRewardsWitness
  :: StakeCredential -> Coin -> CredentialWitness -> M Unit
useWithdrawRewardsWitness stakeCredential amount witness = do
  useCredentialWitness Withdrawal stakeCredential witness
  networkId <- gets _.networkId
  let
    rewardAddress =
      { networkId
      , stakeCredential
      }
  _transaction <<< _body <<< _withdrawals %=
    Map.insert rewardAddress amount
  pure unit

-- | Tries to modify the transaction to make it consume a given output.
-- | Uses a `SpendWitness` to try to satisfy spending requirements.
useSpendWitness :: TransactionUnspentOutput -> OutputWitness -> M Unit
useSpendWitness utxo = case _ of
  PubKeyOutput -> do
    assertOutputType PubKeyHashWitness utxo
  NativeScriptOutput nsWitness -> do
    assertOutputType ScriptHashWitness utxo
    -- attach the script
    useNativeScriptWitness nsWitness
  PlutusScriptOutput plutusScriptWitness redeemerDatum mbDatumWitness -> do
    assertOutputType ScriptHashWitness utxo
    -- attach the script
    usePlutusScriptWitness plutusScriptWitness
    -- attach the datum
    useDatumWitnessForUtxo utxo mbDatumWitness
    -- attach the redeemer
    let
      uiRedeemer = UnindexedRedeemer
        { purpose: ForSpend (unwrap utxo).input
        , datum: unwrap redeemerDatum
        }
    _redeemers %= pushUnique uiRedeemer

usePlutusScriptWitness :: ScriptWitness PlutusScript -> M Unit
usePlutusScriptWitness =
  case _ of
    ScriptValue ps -> do
      _transaction <<< _witnessSet <<< _plutusScripts
        %= pushUnique ps
    ScriptReference input action -> do
      _transaction <<< _body <<< refInputActionToLens action
        %= pushUnique input

useNativeScriptWitness :: ScriptWitness NativeScript -> M Unit
useNativeScriptWitness =
  case _ of
    ScriptValue ns -> do
      _transaction <<< _witnessSet <<< _nativeScripts
        %= pushUnique ns
    ScriptReference refInput refInputAction -> do
      _transaction <<< _body <<< refInputActionToLens refInputAction
        %= pushUnique refInput

-- | Tries to modify the transaction state to make it consume a given script output.
-- | Uses a `DatumWitness` if the UTxO datum is provided as a hash.
useDatumWitnessForUtxo
  :: TransactionUnspentOutput -> Maybe DatumWitness -> M Unit
useDatumWitnessForUtxo utxo mbDatumWitness = do
  case utxo ^. _output <<< _datum of
    -- script outputs must have a datum
    Nothing -> throwError $ WrongSpendWitnessType utxo
    -- if the datum is inline, we don't need to attach it as witness
    Just (OutputDatum _providedDatum) -> do
      case mbDatumWitness of
        Just datumWitness ->
          throwError $ UnneededDatumWitness utxo datumWitness
        Nothing -> pure unit
    -- if the datum is provided as hash,
    Just (OutputDatumHash datumHash) ->
      case mbDatumWitness of
        -- if the datum witness was not provided, look the datum up
        Nothing -> do
          throwError $ DatumWitnessNotProvided utxo
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
