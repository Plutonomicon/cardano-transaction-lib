-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit transactions. It creates two transactions: one that
-- | mints a token and one that sends that token to the owner's address.

module Ctl.Examples.SendsToken (main, example, contract) where

import Contract.Prelude

import Cardano.Types (PlutusScript)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint (Mint)
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (Value)
import Contract.Value as Value
import Contract.Wallet (ownPaymentPubKeyHashes, ownStakePubKeyHashes)
import Ctl.Examples.AlwaysMints (alwaysMintsPolicy)
import Ctl.Examples.Helpers (mkAssetName, mustPayToPubKeyStakeAddress) as Helpers
import Data.Array (head)

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.SendsToken"

  mintToken >>= awaitTxConfirmed
  logInfo' "Tx submitted successfully, Sending token to own address"

  sendToken >>= awaitTxConfirmed
  logInfo' "Tx submitted successfully!"

mintToken :: Contract TransactionHash
mintToken = do
  mp /\ mint /\ _value <- tokenValue
  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustMintValue mint

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.plutusMintingPolicy mp

  submitTxFromConstraints lookups constraints

sendToken :: Contract TransactionHash
sendToken = do
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  skh <- join <<< head <$> ownStakePubKeyHashes
  _ /\ _mint /\ value <- tokenValue
  let
    constraints :: Constraints.TxConstraints
    constraints = Helpers.mustPayToPubKeyStakeAddress pkh skh value

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  submitTxFromConstraints lookups constraints

tokenValue :: Contract (PlutusScript /\ Mint /\ Value)
tokenValue = do
  mp <- alwaysMintsPolicy
  let cs = PlutusScript.hash mp
  tn <- Helpers.mkAssetName "TheToken"
  pure $ mp /\ Mint.singleton cs tn (Int.fromInt 1) /\ Value.singleton cs tn
    (BigNum.fromInt 1)
