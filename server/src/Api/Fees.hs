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
estimateFee pparams tx =
  C.estimateTransactionFee
    undefined -- TODO network ID
    (C.protocolParamTxFeeFixed pparams)
    (C.protocolParamTxFeePerByte pparams)
    tx
    undefined -- TODO num. inputs
    undefined -- TODO num. outputs
    undefined -- TODO num. byron key wits
    undefined -- TODO num. shelley key wits

decodeCborTx :: Cbor -> Either Cbor.DecoderError (C.Tx C.AlonzoEra)
decodeCborTx (Cbor txt) =
  C.deserialiseFromCBOR
    (C.proxyToAsType (Proxy @(C.Tx C.AlonzoEra)))
    $ Text.encodeUtf8 txt
