module Examples.Nami.WithDatums (main) where

import Contract.Prelude

import Contract.Monad (Contract, runContract)
import Contract.PlutusData
  ( class FromData
  , class ToData
  , unitDatum
  )
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction
  ( TransactionHash
  , UnbalancedTx
  , balanceTx
  , submitTransaction
  )
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf)
import Data.BigInt as BigInt
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Types.TypedValidator (class DatumType, class RedeemerType)

main :: Effect Unit
main = launchAff_ $ runContract undefined $ do
  payToAlwaysSucceeds

payToAlwaysSucceeds :: Contract (Maybe TransactionHash)
payToAlwaysSucceeds = do
  valHash <- throwOnNothing "Got `Nothing` for validator hash"
    $ validatorHash alwaysSucceedsValidator
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToOtherScript valHash unitDatum
      $ lovelaceValueOf
      $ BigInt.fromInt 1000

    lookups :: Maybe (Lookups.ScriptLookups Void)
    lookups = Lookups.otherScriptM alwaysSucceedsValidator
  unbalancedTx <- throwOnLeft
    =<< flip mkUnbalancedTx' constraints
    =<< throwOnNothing "Lookups were `Nothing`" lookups
  balancedTx <- throwOnLeft =<< balanceTx unbalancedTx
  submitTransaction balancedTx

alwaysSucceedsValidator :: Validator
alwaysSucceedsValidator = wrap
  $ wrap
  $ hexToByteArrayUnsafe "4d01000033222220051200120011"

-- All of these can probably go once PR #158 is merged

throwOnLeft :: forall (a :: Type) (e :: Type). Show e => Either e a -> Contract a
throwOnLeft = either (throwError <<< error <<< show) pure

throwOnNothing :: forall (a :: Type). String -> Maybe a -> Contract a
throwOnNothing msg = maybe (throwError $ error msg) pure

mkUnbalancedTx'
  :: forall (a :: Type) (b :: Type)
   . DatumType a b
  => RedeemerType a b
  => FromData b
  => ToData b
  => Lookups.ScriptLookups a
  -> Constraints.TxConstraints b b
  -> Contract (Either Lookups.MkUnbalancedTxError UnbalancedTx)
mkUnbalancedTx' ls = wrap <<< Lookups.mkUnbalancedTx ls
