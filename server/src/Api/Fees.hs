module Api.Fees (estimateTxFees) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (ask)
import Data.Bifunctor (first)
import Data.ByteString.Base16 qualified as Base16
import Data.Proxy (Proxy (Proxy))
import Data.Text.Encoding qualified as Text
import Types (
  AppM,
  CardanoBrowserServerError (..),
  Cbor (..),
  Env (..),
  Fee (..),
  FeeEstimateError (..),
 )
import Data.Text (Text)
import Data.ByteString (ByteString)

estimateTxFees :: Cbor -> AppM Fee
estimateTxFees cbor = do
  decoded <- either (throwM . FeeEstimate) pure $ decodeCborTx cbor
  Env {..} <- ask
  pure . Fee $ estimateFee protocolParams decoded

estimateFee :: C.ProtocolParameters -> C.Tx C.AlonzoEra -> Integer
estimateFee pparams (C.Tx txBody keyWits) = estimate
  where
    estimate :: Integer
    C.Lovelace estimate =
      C.evaluateTransactionFee
        pparams
        txBody
        0 -- No. of Byron key witnesses; there shouldn't be any of these and
        -- 'evaluateTransactionFee' won't work with them anyway
        . fromIntegral
        $ length keyWits -- No. of Shelley key witnesses

decodeCborTx :: Cbor -> Either FeeEstimateError (C.Tx C.AlonzoEra)
decodeCborTx (Cbor txt) =
  first InvalidCbor
    . C.deserialiseFromCBOR (C.proxyToAsType Proxy)
    =<< decode txt
  where
    decode :: Text -> Either FeeEstimateError ByteString
    decode = first InvalidHex . Base16.decode . Text.encodeUtf8
