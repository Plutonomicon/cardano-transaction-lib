module Scaffold (contract) where

import Contract.Prelude

import Contract.Address as Contract.Address
import Contract.Log as Contract.Log
import Contract.Monad (Contract)

contract :: Contract () Unit
contract = Contract.Log.logInfo' <<< show =<<
  Contract.Address.ownPaymentPubKeyHash
