module Types.TypedTxOut
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
import Cardano.Types.Transaction (TransactionOutput(TransactionOutput))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import FromData (class FromData, fromData)
import Hashing (datumHash) as Hashing
import Helpers (liftM)
import QueryM (QueryM, getDatumByHash)
import Scripts (typedValidatorEnterpriseAddress)
import Serialization.Address (Address, NetworkId)
import ToData (class ToData, toData)
import Type.Proxy (Proxy)
import Types.Datum (DataHash, Datum(Datum))
import Types.PlutusData (PlutusData)
import Types.Transaction (TransactionInput)
import Types.TypedValidator
  ( class DatumType
  , TypedValidator
  )
import Cardano.Types.Value (Value)

-- | A `TransactionInput` tagged by a phantom type: and the
-- | connection type of the output.
-- | Plutus uses wraps this type with a `TxIn` data type instead with optionally
-- | carries the address type. We don't include such a type in our setup.
-- | Note that `TypedTxOut` is implicitly constrained by its smart
-- | constructor.
newtype TypedTxOutRef (a :: Type) (b :: Type) = TypedTxOutRef
  { txOutRef :: TransactionInput, typedTxOut :: TypedTxOut a b }

-- `DatumType v d` not needed but this replicates Plutus and provides extra
-- type safety.
derive newtype instance (DatumType v d, Eq d) => Eq (TypedTxOutRef v d)

-- | Extract the `Address` of a `TypedTxOutRef`
typedTxOutRefAddress
  :: forall (v :: Type) (d :: Type)
   . DatumType v d -- TODO: are there artificial constraints needed? (here and below)
  => TypedTxOutRef v d
  -> Address
typedTxOutRefAddress (TypedTxOutRef { typedTxOut }) =
  typedTxOutAddress typedTxOut

-- | Extract the `DataHash` of a `TypedTxOutRef`
typedTxOutRefDatumHash
  :: forall (v :: Type) (d :: Type)
   . DatumType v d
  => TypedTxOutRef v d
  -> Maybe DataHash
typedTxOutRefDatumHash (TypedTxOutRef { typedTxOut }) =
  typedTxOutDatumHash typedTxOut

-- | Extract the `Value` of a `TypedTxOutRef`
typedTxOutRefValue
  :: forall (v :: Type) (d :: Type)
   . DatumType v d
  => TypedTxOutRef v d
  -> Value
typedTxOutRefValue (TypedTxOutRef { typedTxOut }) = typedTxOutValue typedTxOut

-- | Extract the `TransactionInput` of a `TypedTxOutRef`
typedTxOutRefInput
  :: forall (v :: Type) (d :: Type)
   . DatumType v d
  => TypedTxOutRef v d
  -> TransactionInput
typedTxOutRefInput (TypedTxOutRef { txOutRef }) = txOutRef

-- A `TransactionOutput` tagged by a phantom type: and the connection type of
-- the output. DO NOT import as extra constraints are required so only import
-- the smart constructor `mkTypedTxOut`
newtype TypedTxOut (v :: Type) (d :: Type) = TypedTxOut
  { txOut :: TransactionOutput, data :: d }

-- `DatumType a b` not needed but this replicates Plutus and provides extra
-- type safety.
derive newtype instance (DatumType v d, Eq d) => Eq (TypedTxOut v d)

-- | Extract the `Address` of a `TypedTxOut`
typedTxOutAddress
  :: forall (v :: Type) (d :: Type)
   . DatumType v d
  => TypedTxOut v d
  -> Address
typedTxOutAddress (TypedTxOut { txOut }) = (unwrap txOut).address

-- | Extract the `DataHash` of a `TypedTxOut`
typedTxOutDatumHash
  :: forall (v :: Type) (d :: Type)
   . DatumType v d
  => TypedTxOut v d
  -> Maybe DataHash
typedTxOutDatumHash (TypedTxOut { txOut }) = (unwrap txOut).dataHash

-- | Extract the `Value` of a `TypedTxOut`
typedTxOutValue
  :: forall (v :: Type) (d :: Type)
   . DatumType v d
  => TypedTxOut v d
  -> Value
typedTxOutValue (TypedTxOut { txOut }) = (unwrap txOut).amount

-- | Extract the `TxOut` ~ `TransactionOutput` of a `TypedTxOut`
typedTxOutTxOut
  :: forall (v :: Type) (d :: Type)
   . DatumType v d
  => TypedTxOut v d
  -> TransactionOutput
typedTxOutTxOut (TypedTxOut { txOut }) = txOut

-- Purescript's lack of dependent types requires this smart constructor as we
-- cannot constrain the `TypedTxOut` datatype.
-- | Smart constructor to create a `TypedTxOut` from a network ID,
-- | a correctly-typed data, script, an address, and a value. A smart
-- | constructor is required because extra constraints are needed.
-- | `TransactionOutput` is tagged by a phantom type.
mkTypedTxOut
  :: forall (v :: Type) (d :: Type)
   . DatumType v d
  => ToData d
  => NetworkId
  -> TypedValidator v
  -> d
  -> Value
  -> Maybe (TypedTxOut v d)
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
          wrap { address, amount, dataHash: pure dHash }
  where
  mkTypedTxOut'
    :: d -- Data
    -> TransactionOutput
    -> TypedTxOut v d
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
  :: forall (a :: Type) (m :: Type -> Type)
   . Monad m
  => NetworkId
  -> TypedValidator a
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
  :: forall (v :: Type) (d :: Type) (m :: Type -> Type)
   . Monad m
  => DatumType v d
  => FromData d
  => TypedValidator v
  -> Datum
  -> m (Either TypeCheckError d)
checkDatum _ (Datum pd) =
  runExceptT $ liftM (WrongDatumType pd) (fromData pd :: Maybe d)

-- | Create a `TypedTxOut` from an existing `TransactionInput` by
-- | checking the types of its parts.
typeTxOut
  :: forall (v :: Type) (d :: Type)
   . DatumType v d
  => FromData d
  => ToData d
  => NetworkId
  -> TypedValidator v
  -> TransactionOutput
  -> QueryM (Either TypeCheckError (TypedTxOut v d))
typeTxOut
  networkId
  typedVal
  (TransactionOutput { address, amount, dataHash }) =
  runExceptT do
    -- Assume `Nothing` is a public key.
    dHash <- liftM ExpectedScriptGotPubkey dataHash
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
  :: forall (v :: Type) (d :: Type) (m :: Type -> Type)
   . DatumType v d
  => FromData d
  => ToData d
  => NetworkId
  -> (TransactionInput -> Maybe TransactionOutput)
  -> TypedValidator v
  -> TransactionInput
  -> QueryM (Either TypeCheckError (TypedTxOutRef v d))
typeTxOutRef networkId lookupRef typedVal txOutRef = runExceptT do
  out <- liftM UnknownRef (lookupRef txOutRef)
  typedTxOut <- ExceptT $ typeTxOut networkId typedVal out
  pure $ TypedTxOutRef { txOutRef, typedTxOut }
