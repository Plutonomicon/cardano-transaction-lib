module Ctl.Internal.QueryM.Blockfrost where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.QueryM (ClientError, QueryM)
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Types.Datum (DataHash, Datum)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Undefined (undefined)

utxosAt :: Address -> QueryM (Either ClientError UtxoMap)
utxosAt = undefined

getDatumByHash :: DataHash -> QueryM (Either ClientError (Maybe Datum))
getDatumByHash = undefined
