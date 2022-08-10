module QueryM.MinFee (calculateMinFee) where

import Prelude

import Cardano.Types.Transaction (Transaction)
import Cardano.Types.Value (Coin)
import QueryM (QueryM)
import QueryM.ProtocolParameters (getProtocolParameters)
import Serialization.MinFee (calculateMinFeeCsl)

-- | Calculate `min_fee` using CSL with protocol parameters from Ogmios.
calculateMinFee :: Transaction -> QueryM Coin
calculateMinFee tx = do
  pparams <- getProtocolParameters
  calculateMinFeeCsl pparams tx
