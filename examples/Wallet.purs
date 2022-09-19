module Examples.Wallet (example, contract) where

import Contract.Prelude

import Cardano.Types.Transaction (PublicKey(..))
import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Config (ConfigParams)
import Contract.Log (logDebug')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Utxos (getWalletBalance, getWalletUtxos)
import Data.Newtype (unwrap)
import Deserialization.Keys (publicKeyFromBech32)
import Effect.Aff (error)
import FromData (fromData)
import Serialization.Keys (bech32FromPublicKey, bytesFromPublicKey)
import ToData (toData)
import Types.PlutusData (PlutusData)

customToData :: PublicKey -> Maybe PlutusData
customToData = map toData <<< bytesFromPublicKey <=< publicKeyFromBech32 <<<
  unwrap

contract :: Contract () Unit
contract = do
  let
    pk = PublicKey
      "ed25519_pk1eamrnx3pph58yr5l4z2wghjpu2dt2f0rp0zq9qquqa39p52ct0xsudjp4e"

  let
    cslImpl :: Maybe PlutusData
    cslImpl = customToData pk
    myImpl = toData pk

  (pk' :: PublicKey) <- liftM (error "failllll") $ (cslImpl >>= fromData)

  logDebug' $ "pk: " <> show pk'
  logDebug' $ "cslImpl: " <> show cslImpl
  logDebug' $ "myImpl: " <> show myImpl
  logDebug' <<< show $ cslImpl == Just myImpl

  log "Address:"
  log <<< show =<< getWalletAddress
  log "Collateral:"
  log <<< show =<< getWalletCollateral
  log "Balance:"
  log <<< show =<< getWalletBalance
  log "UTxOs:"
  log <<< show =<< getWalletUtxos
  liftAff $ publishTestFeedback true

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract
