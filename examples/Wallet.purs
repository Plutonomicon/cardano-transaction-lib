module Ctl.Examples.Wallet (example, contract) where

import Contract.Prelude

import Aeson (JsonDecodeError, decodeAeson, encodeAeson)
import Contract.Address
  ( getWalletAddress
  , getWalletCollateral
  , ownPaymentPubKeyHash
  )
import Contract.Config (ConfigParams)
import Contract.Log (logAeson, logDebug')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.Scripts
  ( NativeScript
      ( ScriptAny
      , ScriptAll
      , ScriptPubkey
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  )
import Contract.Test.E2E (publishTestFeedback)
import Contract.Utxos (getWalletBalance, getWalletUtxos)
import Ctl.Internal.Types.BigNum (fromStringUnsafe)

contract :: Contract () Unit
contract = do
  pkh <- liftedM "couldn't get pkh" ownPaymentPubKeyHash

  let
    ns = ScriptNOfK 1
      [ ScriptAll
          [ ScriptAny
              [ ScriptPubkey (unwrap $ unwrap pkh)
              , ScriptPubkey (unwrap $ unwrap pkh)
              ]
          , TimelockExpiry (wrap $ fromStringUnsafe "5200000")
          ]
      , TimelockStart (wrap $ fromStringUnsafe "6000")
      ]

  logAeson logDebug' ns

  logDebug' $ show
    (decodeAeson (encodeAeson ns) :: Either JsonDecodeError NativeScript)

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
