{-# LANGUAGE NoImplicitPrelude #-}

module Scripts (alwaysSucceeds) where

import Plutus.V1.Ledger.Api (Validator, mkValidatorScript)
import PlutusTx (compile)
import PlutusTx.Prelude (BuiltinData, ($))

-- import Prelude (undefined)

{-# INLINE mkAlwaysSucceeds #-}
mkAlwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkAlwaysSucceeds _ _ _ = ()

alwaysSucceeds :: Validator
alwaysSucceeds = mkValidatorScript $ $$(compile [||mkAlwaysSucceeds||])
