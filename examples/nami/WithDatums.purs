module Examples.Nami.WithDatums (main) where

import Contract.Prelude

import Contract.Monad (Contract(Contract), QueryConfig)
import Contract.PlutusData (unitDatum)
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction (TransactionHash, UnbalancedTx, balanceTx, submitTransaction)
import Contract.Value (lovelaceValueOf)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (runReaderT)
import Data.BigInt as BigInt
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import FromData (class FromData)
import ToData (class ToData)
import Types.ByteArray (hexToByteArrayUnsafe)
import Types.ScriptLookups (MkUnbalancedTxError, mkUnbalancedTx)
import Types.ScriptLookups as Lookups
import Types.TxConstraints as Constraints
import Types.TypedValidator (class DatumType, class RedeemerType)

main :: Effect Unit
main = launchAff_ $ runContract undefined $ do
  payToAlwaysSucceeds

payToAlwaysSucceeds :: Contract (Maybe TransactionHash)
payToAlwaysSucceeds = do
  valHash <- liftJust "Got `Nothing` for validator hash"
    $ validatorHash alwaysSucceedsValidator
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToOtherScript valHash unitDatum
      $ lovelaceValueOf
      $ BigInt.fromInt 1000

    lookups :: Maybe (Lookups.ScriptLookups Void)
    lookups = Lookups.otherScript alwaysSucceedsValidator
  unbalancedTx <- liftRight
    =<< flip mkUnbalancedTx' constraints
    =<< liftJust "Lookups were `Nothing`" lookups
  balancedTx <- liftRight =<< balanceTx unbalancedTx
  submitTransaction balancedTx

alwaysSucceedsValidator :: Validator
alwaysSucceedsValidator = wrap
  $ wrap
  $ hexToByteArrayUnsafe "4d01000033222220051200120011"

-- All of these can probably go once PR #158 is merged

runContract :: forall (a :: Type). QueryConfig -> Contract a -> Aff a
runContract qcfg (Contract x) = runReaderT x qcfg

liftRight :: forall (a :: Type) (e :: Type). Show e => Either e a -> Contract a
liftRight = either (throwError <<< error <<< show) pure

liftJust :: forall (a :: Type). String -> Maybe a -> Contract a
liftJust msg = maybe (throwError $ error msg) pure

mkUnbalancedTx'
  :: forall (a :: Type) (b :: Type)
   . DatumType a b
  => RedeemerType a b
  => FromData b
  => ToData b
  => Lookups.ScriptLookups a
  -> Constraints.TxConstraints b b
  -> Contract (Either MkUnbalancedTxError UnbalancedTx)
mkUnbalancedTx' ls = wrap <<< mkUnbalancedTx ls
