module CTL.Internal.Plutip.Utils
  ( tmpdir
  ) where

import Effect (Effect)

-- TODO: remove this function when PS bindings for os.tmpdir are available.
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/726
foreign import tmpdir :: Effect String
