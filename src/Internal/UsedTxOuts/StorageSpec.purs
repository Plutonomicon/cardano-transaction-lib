module Ctl.Internal.UsedTxOuts.StorageSpec where

import Ctl.Internal.UsedTxOut.LocalStorage (localStorage)
import Ctl.Internal.UsedTxOuts.Storage (Storage)
import Node.Path (FilePath)
import Undefined (undefined)

data StorageSpec
  = UseLocalStorage
  | UseFileStorage FilePath
  | UseGlobalVariable String
  | UseStorage Storage

mkStorage :: StorageSpec -> Storage
mkStorage UseLocalStorage = localStorage
mkStorage (UseFileStorage file) = undefined
mkStorage (UseGlobalVariable var) = undefined
mkStorage (UseStorage storage) = storage

defaultStorageSpec :: StorageSpec
defaultStorageSpec = _defaultStorageSpec UseLocalStorage
  (UseFileStorage ".ctl_locked_utxos")

foreign import _defaultStorageSpec :: StorageSpec -> StorageSpec -> StorageSpec
