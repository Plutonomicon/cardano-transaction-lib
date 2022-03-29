module Examples.Nami.WithDatums (main) where

import Contract.Prelude

import Contract.Address (getWalletAddress)
import Contract.Monad (Contract(..), defaultContractConfig, runContract)
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction (FinalizedTransaction(..), balanceTx, finalizeTx)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf)
import Data.Argonaut (decodeJson, parseJson)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Console as Console
import QueryM.Submit (submit)
import Types.Datum (unitDatum)
import Types.ScriptLookups (ScriptLookups, mkUnbalancedTx', otherScript) as Lookups

main :: Effect Unit
main = launchAff_ $ do
  cfg <- defaultContractConfig
  runContract cfg $ do
    payToAlwaysSucceeds

payToAlwaysSucceeds :: Contract String -- returns TxId
payToAlwaysSucceeds = do
  validator <- throwOnNothing "Got `Nothing` for validator"
    alwaysSucceedsValidator
  valHash <-
    throwOnNothing "Got `Nothing` for validator hash"
      =<< validatorHash validator
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToOtherScript valHash unitDatum
      $ lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.otherScript validator
  ownAddress <- throwOnNothing "Failed to get wallet address" =<< getWalletAddress
  inputs :: Array _ <-
    map fst
      <<<
        Map.toUnfoldable
      <<<
        unwrap
      <$> (throwOnNothing "Failed to get utxos" =<< utxosAt ownAddress)
  liftEffect $ Console.log $ show inputs
  { unbalancedTx, datums, redeemers } <- throwOnLeft
    =<< (Contract $ Lookups.mkUnbalancedTx' lookups constraints)
  liftEffect $ Console.log $ show $ unbalancedTx
  balancedTx <- throwOnLeft =<< balanceTx unbalancedTx
  liftEffect $ Console.log $ show balancedTx
  FinalizedTransaction byteArray <-
    throwOnNothing "Failed to get finalized Tx" =<<
      finalizeTx balancedTx datums redeemers
  txId <- Contract $ submit byteArray
  liftEffect $ Console.log $ show txId
  pure txId

alwaysSucceedsValidator :: Maybe Validator
alwaysSucceedsValidator = hush
  $ decodeJson
  =<< parseJson "\"4d01000033222220051200120011\""

throwOnLeft
  :: forall (a :: Type) (e :: Type). Show e => Either e a -> Contract a
throwOnLeft = either (throwError <<< error <<< show) pure

throwOnNothing :: forall (a :: Type). String -> Maybe a -> Contract a
throwOnNothing msg = maybe (throwError $ error msg) pure
