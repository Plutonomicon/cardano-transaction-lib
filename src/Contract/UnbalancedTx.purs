-- | A module for creating off-chain script lookups, and an unbalanced
-- | transaction.
module Contract.UnbalancedTx
  ( mkUnbalancedTx
  , mkUnbalancedTxE
  ) where

import Prelude

import Cardano.Types (Transaction, UtxoMap)
import Contract.Monad (Contract)
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.ProcessConstraints as PC
import Ctl.Internal.ProcessConstraints.Error
  ( MkUnbalancedTxError
  , explainMkUnbalancedTxError
  )
import Ctl.Internal.Types.ScriptLookups (ScriptLookups)
import Ctl.Internal.Types.TxConstraints (TxConstraints)
import Data.Either (Either(Left, Right))
import Data.Tuple.Nested (type (/\))
import Effect.Exception (error)

-- | Create an `UnbalancedTx` given `ScriptLookups` and
-- | `TxConstraints`. This should be called in conjuction with
-- | `balanceTx` and  `signTransaction`.
-- |
-- | This is a 'non-throwing' variant; if you need the 'throwing' variant, use
-- | `mkUnbalancedTx` instead.
mkUnbalancedTxE
  :: ScriptLookups
  -> TxConstraints
  -> Contract (Either MkUnbalancedTxError (Transaction /\ UtxoMap))
mkUnbalancedTxE = PC.mkUnbalancedTxImpl

-- | As `mkUnbalancedTxE`, but 'throwing'.
mkUnbalancedTx
  :: ScriptLookups
  -> TxConstraints
  -> Contract (Transaction /\ UtxoMap)
mkUnbalancedTx lookups constraints =
  mkUnbalancedTxE lookups constraints >>= case _ of
    Left err -> throwError $ error $ explainMkUnbalancedTxError err
    Right res -> pure res
