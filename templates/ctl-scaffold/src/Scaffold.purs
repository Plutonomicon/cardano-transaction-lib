-- | This module contains a minimal contract that only prints public key
-- | info to the console.
module Scaffold (contract) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract)

contract :: Contract () Unit
contract = do
  logInfo' "Welcome to CTL! Your wallet's payment PubKey hash is:"
  logInfo' <<< show =<< ownPaymentPubKeyHash
