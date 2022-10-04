module Ctl.Examples.Demo (main) where

import Prelude

import Contract.Address (scriptHashAddress)
import Contract.Config (testnetLodeConfig)
import Contract.Monad (ConfigParams, Contract, liftedE, liftedM, runContract)
import Contract.PlutusData (PlutusData, unitDatum, unitRedeemer)
import Contract.Prelude (fromMaybe, unwrap, wrap)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , balanceTx
  , getTxFinalFee
  , lookupTxHash
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletBalance, utxosAt)
import Contract.Value (lovelaceValueOf)
import Ctl.Examples.Demo.HTML (OnSendHandler, onSend, setBalance)
import Ctl.Examples.PlutusV2.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput (_input)
import Data.Array (head)
import Data.BigInt (BigInt, fromInt, toNumber)
import Data.Int (floor)
import Data.Lens (view)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple.Nested (type (/\), (/\))
-- import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , delay
  , error
  , launchAff_
  , parallel
  , sequential
  , throwError
  )
import Effect.Aff.Class (liftAff)

main :: Effect Unit
main = launchAff_ $ runContract config do
  validator <- alwaysSucceedsScriptV2
  let
    vhash = validatorHash validator

    sendHandler :: OnSendHandler
    sendHandler amount setStatus setBuilt = do
      txId /\ fee <- payToAlwaysSucceeds setStatus
        (floor $ amount * 1_000_000.0)
        vhash
      setConfirmedAndAwaitRetrieve <- setBuilt (txIdToHex txId)
        (lovelaceToAda fee)
      setRetrieved <- setConfirmedAndAwaitRetrieve
      spendTxId /\ spendFee <- spendFromAlwaysSucceeds setStatus vhash validator
        txId
      setRetrieved (txIdToHex spendTxId) (lovelaceToAda spendFee)

  sequential ado
    _ <- parallel updateBalance
    _ <- parallel $ onSend sendHandler
    in unit
  where
  lovelaceToAda :: BigInt -> Number
  lovelaceToAda ll = toNumber ll / 1_000_000.0

  txIdToHex :: TransactionHash -> String
  txIdToHex = byteArrayToHex <<< unwrap

updateBalance :: Contract () Unit
updateBalance = do
  balance <- liftedM "could not get wallet balance" getWalletBalance
  setBalance balance
  liftAff $ delay (Milliseconds 2500.0)
  updateBalance

config :: ConfigParams ()
config = testnetLodeConfig
  { ctlServerConfig = Nothing
  -- To run inside the Lode mobile app, secure connections must be used.
  -- The easiest way to secure is to use ngrok.
  -- The following `~/.ngrok2/ngrok.yml` config will create the necessary
  -- tunnels after running `ngrok start --all`:
  -- authtoken: ...
  -- tunnels:
  --   webserver:
  --     addr: 4008
  --     proto: http
  --     bind_tls: true
  --     host_header: rewrite
  --   ogmios:
  --     addr: 1337
  --     proto: http
  --     bind_tls: true
  --     host_header: rewrite
  --   odc:
  --     addr: 9999
  --     proto: http
  --     bind_tls: true
  --     host_header: rewrite

  -- Note that you must first visit each of the ngrok URLs manually in the
  -- browser.

  -- Then you can uncomment the following lines and update the `host` fields to
  -- the ones printed by `ngrok`:
  -- , ogmiosConfig = 
  --     { port: UInt.fromInt 443
  --     , host: "857f-86-6-49-94.eu.ngrok.io"
  --     , secure: true
  --     , path: Nothing
  --     }
  -- , datumCacheConfig =
  --     { port: UInt.fromInt 443
  --     , host: "bea2-86-6-49-94.eu.ngrok.io"
  --     , secure: true
  --     , path: Nothing
  --     }
  }

payToAlwaysSucceeds
  :: (String -> Contract () Unit)
  -> Int
  -> ValidatorHash
  -> Contract () (TransactionHash /\ BigInt)
payToAlwaysSucceeds log amount vhash = do
  log "building"
  let
    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript vhash unitDatum
        Constraints.DatumWitness
        $ lovelaceValueOf
        $ fromInt amount

    lookups :: ScriptLookups PlutusData
    lookups = mempty

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  log "balancing"
  balanced <- liftedE $ balanceTx unbalancedTx
  log "signing"
  balancedSignedTx <- wrap <$>
    (liftedM "fail" $ signTransaction $ unwrap balanced)
  log "submitting"
  txHash <- submit balancedSignedTx
  log "pending"
  awaitTxConfirmed txHash
  pure (txHash /\ getTxFinalFee balancedSignedTx)

spendFromAlwaysSucceeds
  :: (String -> Contract () Unit)
  -> ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract () (TransactionHash /\ BigInt)
spendFromAlwaysSucceeds log vhash validator txId = do
  log "building"
  let scriptAddress = scriptHashAddress vhash
  utxos <- fromMaybe Map.empty <$> utxosAt scriptAddress
  case view _input <$> head (lookupTxHash txId utxos) of
    Just txInput -> do
      let
        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.validator validator
          <> Lookups.unspentOutputs utxos

        constraints :: TxConstraints Unit Unit
        constraints =
          Constraints.mustSpendScriptOutput txInput unitRedeemer

      unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
      log "balancing"
      balanced <- liftedE $ balanceTx unbalancedTx
      log "signing"
      balancedSignedTx <- wrap <$>
        (liftedM "fail" $ signTransaction $ unwrap balanced)
      log "submitting"
      spendTxId <- submit balancedSignedTx
      log "pending"
      awaitTxConfirmed spendTxId
      pure (spendTxId /\ getTxFinalFee balancedSignedTx)

    _ -> throwError $ error "oops"
