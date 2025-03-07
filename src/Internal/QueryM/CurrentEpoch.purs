-- | A module to get "currentEpoch" via an Ogmios request.
module Ctl.Internal.QueryM.CurrentEpoch
  ( getCurrentEpoch
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM.Ogmios (currentEpoch) as Ogmios
import Ctl.Internal.QueryM.Ogmios.Types (CurrentEpoch, pprintOgmiosDecodeError)
import Data.Either (either)
import Effect.Exception (error)

-- | Get the current Epoch. Details can be found https://ogmios.dev/api/ under
-- | "currentEpoch" query
getCurrentEpoch :: QueryM CurrentEpoch
getCurrentEpoch = Ogmios.currentEpoch
  >>= either
    (throwError <<< error <<< pprintOgmiosDecodeError)
    pure
