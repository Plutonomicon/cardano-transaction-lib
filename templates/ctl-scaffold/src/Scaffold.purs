module Scaffold (contract) where

import CTL.Contract.Prelude

import CTL.Contract.Address as Contract.Address
import CTL.Contract.Log as Contract.Log
import CTL.Contract.Monad (Contract)

contract :: Contract () Unit
contract = Contract.Log.logInfo' <<< show =<<
  Contract.Address.ownPaymentPubKeyHash
