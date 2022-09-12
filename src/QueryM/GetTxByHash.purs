module QueryM.GetTxByHash where

import Prelude

import Base64 (toByteArray)
import Cardano.Types.Transaction (Transaction)
import Data.Either (hush)
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Deserialization.Transaction (deserializeTransaction)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import QueryM (QueryM, mkDatumCacheRequest)
import QueryM.DatumCacheWsp as QueryM
import QueryM.Ogmios (TxHash)
import Types.CborBytes (CborBytes(CborBytes))

getTxByHash :: TxHash -> QueryM (Maybe Transaction)
getTxByHash txHash = do
  unwrap <$> mkDatumCacheRequest QueryM.getTxByHash _.getTxByHash txHash >>=
    maybe
      (pure Nothing)
      \txBase64 -> do
        let
          txBytes = CborBytes $ toByteArray txBase64
        maybe (liftEffect $ throw "Unable to decode transaction")
          (pure <<< Just)
          $ hush
          $ deserializeTransaction txBytes
