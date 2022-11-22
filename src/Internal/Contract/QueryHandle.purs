module Ctl.Internal.Contract.QueryHandle where
{-
import


type QueryHandle (m :: Type -> Type) =
  { utxosAt :: Address -> m UtxoMap
  , getDatumByHash :: DataHash -> m (Maybe Datum)
  , getDatumByHashes :: Array DataHash -> m (Map DataHash Datum)
  , getScriptByHash :: ScriptHash -> m (Maybe ScriptRef)
  , getScriptsByHashes :: Array ScriptHash -> m (Map ScriptHash ScriptRef)
  }
-}
