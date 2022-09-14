module CTL.Internal.Types.TypedTxOut
  ( TypeCheckError
      ( WrongValidatorAddress
      , ExpectedScriptGotPubkey
      , WrongRedeemerType
      , WrongDatumType
      , CannotQueryDatum
      , CannotMakeTypedTxOut
      , UnknownRef
      )
  , TypedTxOut
  , TypedTxOutRef
  , mkTypedTxOut
  , typeTxOut
  , typeTxOutRef
  , typedTxOutAddress
  , typedTxOutDatumHash
  , typedTxOutRefAddress
  , typedTxOutRefDatumHash
  , typedTxOutRefInput
  , typedTxOutRefValue
  , typedTxOutTxOut
  , typedTxOutValue
  ) where

-- DO NOT export data constructors for `TypedTxOut` and `TypedTxOutRef`.

-- | This module defines typed versions the transaction input and output types
-- | to ensure that the validator script type agrees with the attached inputs
-- | and outputs.
-- | https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger/html/src/Ledger.Typed.Tx.html

import Prelude

import CTL.Internal.Cardano.Types.Transaction (TransactionOutput(TransactionOutput))
import CTL.Internal.Cardano.Types.Value (Value)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import CTL.Internal.FromData (class FromData, fromData)
import CTL.Internal.Hashing (datumHash) as Hashing
import CTL.Internal.Helpers (liftM)
import CTL.Internal.IsData (class IsData)
import CTL.Internal.QueryM (QueryM, getDatumByHash)
import CTL.Internal.Scripts (typedValidatorEnterpriseAddress)
import CTL.Internal.Serialization.Address (Address, NetworkId)
import CTL.Internal.ToData (class ToData, toData)
import CTL.Internal.Types.Datum (DataHash, Datum(Datum))
import CTL.Internal.Types.OutputDatum (OutputDatum(OutputDatumHash), outputDatumDataHash)
import CTL.Internal.Types.PlutusData (PlutusData)
import CTL.Internal.Types.Transaction (TransactionInput)
import CTL.Internal.Types.TypedValidator (class DatumType, TypedValidator)

-- | A `TransactionInput` tagged by a phantom type: and the
-- | connection type of the output.
-- | Plutus uses wraps this type with a `TxIn` data type instead with optionally
-- | carries the address type. We don't include such a type in our setup.
-- | Note that `TypedTxOut` is implicitly constrained by its smart
-- | constructor.
newtype TypedTxOutRef (validator :: Type) (datum :: Type) = TypedTxOutRef
  { txOutRef :: TransactionInput, typedTxOut :: TypedTxOut validator datum }

-- `DatumType validator datum` not needed but this replicates Plutus and provides extra
-- type safety.
derive newtype instance
  ( DatumType validator datum
  , Eq datum
  ) =>
  Eq (TypedTxOutRef validator datum)

-- | Extract the `Address` of a `TypedTxOutRef`
typedTxOutRefAddress
  :: forall (validator :: Type) (datum :: Type)
   . DatumType validator datum
  => TypedTxOutRef validator datum
  -> Address
typedTxOutRefAddress (TypedTxOutRef { typedTxOut }) =
  typedTxOutAddress typedTxOut

-- | Extract the `DataHash` of a `TypedTxOutRef`
typedTxOutRefDatumHash
  :: forall (validator :: Type) (datum :: Type)
   . DatumType validator datum
  => TypedTxOutRef validator datum
  -> Maybe DataHash
typedTxOutRefDatumHash (TypedTxOutRef { typedTxOut }) =
  typedTxOutDatumHash typedTxOut

-- | Extract the `Value` of a `TypedTxOutRef`
typedTxOutRefValue
  :: forall (validator :: Type) (datum :: Type)
   . DatumType validator datum
  => TypedTxOutRef validator datum
  -> Value
typedTxOutRefValue (TypedTxOutRef { typedTxOut }) = typedTxOutValue typedTxOut

-- | Extract the `TransactionInput` of a `TypedTxOutRef`
typedTxOutRefInput
  :: forall (validator :: Type) (datum :: Type)
   . DatumType validator datum
  => TypedTxOutRef validator datum
  -> TransactionInput
typedTxOutRefInput (TypedTxOutRef { txOutRef }) = txOutRef

-- A `TransactionOutput` tagged by a phantom type: and the connection type of
-- the output. DO NOT import as extra constraints are required so only import
-- the smart constructor `mkTypedTxOut`
newtype TypedTxOut (validator :: Type) (datum :: Type) = TypedTxOut
  { txOut :: TransactionOutput, data :: datum }

-- `DatumType a b` not needed but this replicates Plutus and provides extra
-- type safety.
derive newtype instance
  ( DatumType validator datum
  , Eq datum
  ) =>
  Eq (TypedTxOut validator datum)

-- | Extract the `Address` of a `TypedTxOut`
typedTxOutAddress
  :: forall (validator :: Type) (datum :: Type)
   . DatumType validator datum
  => TypedTxOut validator datum
  -> Address
typedTxOutAddress (TypedTxOut { txOut }) = (unwrap txOut).address

-- | Extract the `DataHash` of a `TypedTxOut`
typedTxOutDatumHash
  :: forall (validator :: Type) (datum :: Type)
   . DatumType validator datum
  => TypedTxOut validator datum
  -> Maybe DataHash
typedTxOutDatumHash (TypedTxOut { txOut }) = outputDatumDataHash
  (unwrap txOut).datum

-- | Extract the `Value` of a `TypedTxOut`
typedTxOutValue
  :: forall (validator :: Type) (datum :: Type)
   . DatumType validator datum
  => TypedTxOut validator datum
  -> Value
typedTxOutValue (TypedTxOut { txOut }) = (unwrap txOut).amount

-- | Extract the `TxOut` ~ `TransactionOutput` of a `TypedTxOut`
typedTxOutTxOut
  :: forall (validator :: Type) (datum :: Type)
   . DatumType validator datum
  => TypedTxOut validator datum
  -> TransactionOutput
typedTxOutTxOut (TypedTxOut { txOut }) = txOut

-- Purescript's lack of dependent types requires this smart constructor as we
-- cannot constrain the `TypedTxOut` datatype.
-- | Smart constructor to create a `TypedTxOut` from a network ID,
-- | a correctly-typed data, script, an address, and a value. A smart
-- | constructor is required because extra constraints are needed.
-- | `TransactionOutput` is tagged by a phantom type.
mkTypedTxOut
  :: forall (validator :: Type) (datum :: Type)
   . DatumType validator datum
  => ToData datum
  => NetworkId
  -> TypedValidator validator
  -> datum
  -> Value
  -> Maybe (TypedTxOut validator datum)
mkTypedTxOut networkId typedVal dt amount =
  let
    mDHash = Hashing.datumHash $ Datum $ toData dt
    -- FIX ME: This is hardcoded to enterprise address, it seems like Plutus'
    -- "validatorAddress" also currently doesn't account for staking.
    address = typedValidatorEnterpriseAddress networkId typedVal
  in
    case mDHash of
      Nothing -> Nothing
      Just dHash ->
        Just <<< mkTypedTxOut' dt $
          wrap
            { address
            , amount
            -- TODO: populate properly
            -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/691
            , datum: OutputDatumHash dHash
            , scriptRef: Nothing
            }
  where
  mkTypedTxOut'
    :: datum -- Data
    -> TransactionOutput
    -> TypedTxOut validator datum
  mkTypedTxOut' dat txOut = TypedTxOut { txOut, data: dat }

-- | An error we can get while trying to type an existing transaction part.
data TypeCheckError
  = WrongValidatorAddress Address Address
  | ExpectedScriptGotPubkey
  | WrongRedeemerType PlutusData
  | WrongDatumType PlutusData
  | CannotQueryDatum DataHash
  | CannotMakeTypedTxOut
  | UnknownRef

derive instance Generic TypeCheckError _
derive instance Eq TypeCheckError
derive instance Ord TypeCheckError

instance Show TypeCheckError where
  show = genericShow

-- | Checks that the given validator hash is consistent with the actual validator.
checkValidatorAddress
  :: forall (validator :: Type) (m :: Type -> Type)
   . Monad m
  => NetworkId
  -> TypedValidator validator
  -> Address
  -> m (Either TypeCheckError Unit)
checkValidatorAddress networkId typedVal actualAddr = runExceptT do
  let expectedAddr = typedValidatorEnterpriseAddress networkId typedVal
  unless (expectedAddr == actualAddr)
    $ throwError
    $ WrongValidatorAddress expectedAddr actualAddr

-- -- | Checks that the given redeemer script has the right type.
-- checkRedeemer
--   :: forall (a :: Type) (b :: Type) (m :: Type -> Type)
--    . Monad m
--   => RedeemerType a b
--   => FromData b
--   => TypedValidator a
--   -> Redeemer
--   -> m (Either TypeCheckError b)
-- checkRedeemer _ (Redeemer pd) =
--   runExceptT $ liftM (WrongRedeemerType pd) (fromData pd :: Maybe b)

-- | Checks that the given datum has the right type.
checkDatum
  :: forall (validator :: Type) (datum :: Type) (m :: Type -> Type)
   . Monad m
  => DatumType validator datum
  => FromData datum
  => TypedValidator validator
  -> Datum
  -> m (Either TypeCheckError datum)
checkDatum _ (Datum pd) =
  runExceptT $ liftM (WrongDatumType pd) (fromData pd :: Maybe datum)

-- | Create a `TypedTxOut` from an existing `TransactionInput` by
-- | checking the types of its parts.
typeTxOut
  :: forall (validator :: Type) (datum :: Type)
   . DatumType validator datum
  => IsData datum
  => NetworkId
  -> TypedValidator validator
  -> TransactionOutput
  -> QueryM (Either TypeCheckError (TypedTxOut validator datum))
typeTxOut
  networkId
  typedVal
  (TransactionOutput { address, amount, datum }) =
  runExceptT do
    -- Assume `Nothing` is a public key.
    dHash <- liftM ExpectedScriptGotPubkey $ outputDatumDataHash datum
    void $ checkValidatorAddress networkId typedVal address
    pd <- ExceptT $ getDatumByHash dHash <#> note (CannotQueryDatum dHash)
    dtOut <- ExceptT $ checkDatum typedVal pd
    except $
      note CannotMakeTypedTxOut (mkTypedTxOut networkId typedVal dtOut amount)

-- | Create a `TypedTxOutRef` from an existing `TransactionInput`
-- | by checking the types of its parts. To do this we need to cross-reference
-- | against the validator script and be able to look up the `TransactionInput` to
-- | which this reference points.
typeTxOutRef
  :: forall (validator :: Type) (datum :: Type) (m :: Type -> Type)
   . DatumType validator datum
  => IsData datum
  => NetworkId
  -> (TransactionInput -> Maybe TransactionOutput)
  -> TypedValidator validator
  -> TransactionInput
  -> QueryM (Either TypeCheckError (TypedTxOutRef validator datum))
typeTxOutRef networkId lookupRef typedVal txOutRef = runExceptT do
  out <- liftM UnknownRef (lookupRef txOutRef)
  typedTxOut <- ExceptT $ typeTxOut networkId typedVal out
  pure $ TypedTxOutRef { txOutRef, typedTxOut }
