module Ctl.Internal.QueryM.MinFee (calculateMinFee) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction (Transaction)
import Ctl.Internal.Cardano.Types.Value (Coin)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM.ProtocolParameters (getProtocolParameters)
import Ctl.Internal.Serialization.MinFee (calculateMinFeeCsl)

-- | Calculate `min_fee` using CSL with protocol parameters from Ogmios.
calculateMinFee :: Transaction -> QueryM Coin
calculateMinFee tx = do
  pparams <- getProtocolParameters
  calculateMinFeeCsl pparams tx
