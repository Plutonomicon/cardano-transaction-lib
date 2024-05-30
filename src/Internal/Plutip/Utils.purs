module Ctl.Internal.Plutip.Utils
  ( tmpdir
  , mkDirIfNotExists
  ) where

import Prelude
import Effect (Effect)
import Node.FS.Sync as FS
import Node.Path (FilePath)

-- TODO: remove this function when PS bindings for os.tmpdir are available.
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/726
foreign import tmpdir :: Effect String

mkDirIfNotExists :: FilePath -> Effect Unit
mkDirIfNotExists dirName = do
  exists <- FS.exists dirName
  unless exists $ FS.mkdir dirName