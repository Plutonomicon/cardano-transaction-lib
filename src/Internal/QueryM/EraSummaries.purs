-- | A module to get "eraSummaries" via an Ogmios request.
module Ctl.Internal.QueryM.EraSummaries
  ( getEraSummaries
  ) where

import Prelude

import Cardano.Types.EraSummaries (EraSummaries)
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM.Ogmios (eraSummaries) as Ogmios
import Ctl.Internal.QueryM.Ogmios.Types (pprintOgmiosDecodeError)
import Data.Either (either)
import Data.Newtype (unwrap)
import Effect.Exception (error)

-- | Get `EraSummaries` as used for Slot arithemetic. Details can be found
-- | https://ogmios.dev/api/ under "eraSummaries" query
getEraSummaries :: QueryM EraSummaries
getEraSummaries = Ogmios.eraSummaries
  >>= either (throwError <<< error <<< pprintOgmiosDecodeError)
    (pure <<< unwrap)
