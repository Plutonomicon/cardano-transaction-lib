module CTL.Internal.QueryM.MinFee (calculateMinFee) where

import Prelude

import CTL.Internal.Cardano.Types.Transaction (Transaction)
import CTL.Internal.Cardano.Types.Value (Coin)
import CTL.Internal.QueryM (QueryM)
import CTL.Internal.QueryM.ProtocolParameters (getProtocolParameters)
import CTL.Internal.Serialization.MinFee (calculateMinFeeCsl)

-- | Calculate `min_fee` using CSL with protocol parameters from Ogmios.
calculateMinFee :: Transaction -> QueryM Coin
calculateMinFee tx = do
  pparams <- getProtocolParameters
  calculateMinFeeCsl pparams tx
