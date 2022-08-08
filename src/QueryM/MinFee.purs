module QueryM.MinFee where

import Prelude

import Cardano.Types.Transaction (Transaction)
import Cardano.Types.Value (Coin)
import QueryM (QueryM)
import QueryM.ProtocolParameters (getProtocolParameters)
import Serialization.MinFee (calculateMinFeeCsl)

calculateMinFee :: Transaction -> QueryM Coin
calculateMinFee tx = do
  pparams <- getProtocolParameters
  calculateMinFeeCsl pparams tx
