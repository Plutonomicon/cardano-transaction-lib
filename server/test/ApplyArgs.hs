{-# LANGUAGE TemplateHaskell #-}

module ApplyArgs (
  unappliedScript,
  partiallyAppliedScript,
  fullyAppliedScript
) where

import Prelude ()
import PlutusTx.Prelude
import PlutusTx qualified

import Plutus.V1.Ledger.Api (unsafeFromBuiltinData, fromCompiledCode)
import Plutus.V1.Ledger.Value (CurrencySymbol(..))
import Plutus.V1.Ledger.Scripts (Script, mkValidatorScript, getValidator)


-- Very basic validator but uses its arguments
mkTestValidator :: Integer -> CurrencySymbol -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkTestValidator i (CurrencySymbol cs) _ _ _ = 
  if i == 1 && cs == "" then () else error ()

mkTestValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkTestValidatorUntyped p1 p2 =
  mkTestValidator
    (unsafeFromBuiltinData p1)
    (unsafeFromBuiltinData p2)

unappliedScript :: Script
unappliedScript =
  fromCompiledCode
    $$(PlutusTx.compile [|| mkTestValidatorUntyped ||])

partiallyAppliedScript :: Integer -> Script
partiallyAppliedScript i =
  fromCompiledCode
    ($$(PlutusTx.compile [|| mkTestValidatorUntyped ||])
    `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData i))

fullyAppliedScript :: Integer -> CurrencySymbol -> Script
fullyAppliedScript i b =
  getValidator $ mkValidatorScript
    ($$(PlutusTx.compile [|| mkTestValidatorUntyped ||]) 
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData i)
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData b))
