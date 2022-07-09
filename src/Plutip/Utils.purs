module Plutip.Utils
  ( tmpdir
  ) where

import Effect (Effect)

-- TODO: remove this function when PS bindings for os.tmpdir are available.
foreign import tmpdir :: Effect String
