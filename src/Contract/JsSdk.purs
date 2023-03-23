-- | This module contains helper functions for CTL-based JS SDK developers.
-- | See `doc/using-from-js.md`.
module Contract.JsSdk
  ( runContractJS
  , runContractInEnvJS
  , mkContractEnvJS
  , stopContractEnvJS
  , withContractEnvJS
  ) where

import Prelude

import Contract.Monad
  ( Contract
  , ContractEnv
  , ContractParams
  , mkContractEnv
  , runContract
  , runContractInEnv
  , stopContractEnv
  , withContractEnv
  )
import Control.Promise (Promise, fromAff, toAff)
import Data.Function.Uncurried (Fn1, Fn2, mkFn1, mkFn2, runFn1)
import Effect.Unsafe (unsafePerformEffect)

runContractJS :: forall (a :: Type). Fn2 ContractParams (Contract a) (Promise a)
runContractJS = mkFn2 \params contract ->
  unsafePerformEffect $ fromAff $ runContract params contract

runContractInEnvJS
  :: forall (a :: Type). Fn2 ContractEnv (Contract a) (Promise a)
runContractInEnvJS = mkFn2 \env contract ->
  unsafePerformEffect $ fromAff $ runContractInEnv env contract

mkContractEnvJS
  :: Fn1 ContractParams (Promise ContractEnv)
mkContractEnvJS = mkFn1 $ unsafePerformEffect <<< fromAff <<< mkContractEnv

stopContractEnvJS :: Fn1 ContractEnv (Promise Unit)
stopContractEnvJS = mkFn1 $ unsafePerformEffect <<< fromAff <<< stopContractEnv

withContractEnvJS
  :: forall a. Fn2 ContractParams (Fn1 ContractEnv (Promise a)) (Promise a)
withContractEnvJS = mkFn2 \params callback ->
  unsafePerformEffect $ fromAff $ withContractEnv params
    (toAff <<< runFn1 callback)
