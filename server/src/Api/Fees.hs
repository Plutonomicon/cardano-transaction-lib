module Api.Fees (estimateTxFees) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as Cbor
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (ask)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Encoding qualified as Text
import Types

estimateTxFees :: Cbor -> AppM Fee
estimateTxFees cbor = do
  decoded <- either throwM pure $ decodeCborTx cbor
  Env {..} <- ask
  pure . Fee $ estimateFee protocolParams decoded

estimateFee :: C.ProtocolParameters -> C.Tx C.AlonzoEra -> C.Lovelace
estimateFee pparams (C.Tx txBody keyWits) =
  C.evaluateTransactionFee
    pparams
    txBody
    0 -- No. of Byron key witnesses; there shouldn't be any of these and
    -- 'evaluateTransactionFee' won't work with these anyway
    . fromIntegral
    $ length keyWits -- No. of Shelley key witnesses

decodeCborTx :: Cbor -> Either Cbor.DecoderError (C.Tx C.AlonzoEra)
decodeCborTx (Cbor txt) =
  C.deserialiseFromCBOR
    (C.proxyToAsType (Proxy @(C.Tx C.AlonzoEra)))
    $ Text.encodeUtf8 txt
