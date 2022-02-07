module Test.AffInterface where

import Prelude
import Control.Monad.Reader.Trans (runReaderT)
import TestM (TestPlanM)
import Mote (group, test)
import Ogmios (mkOgmiosWebSocketAff, utxosAt')

testnet_addr :: String
testnet_addr =
  "addr_test1qr7g8nrv76fc7k4ueqwecljxx9jfwvsgawhl55hck3n8uwaz26mpcwu58zdkhpdnc6nuq3fa8vylc8ak9qvns7r2dsysp7ll4d"

-- note: currently this suite relies on Ogmios being open and running against the
-- testnet, and does not directly test outputs, as this suite is intended to
-- help verify that the Aff interface for websockets itself works,
-- not that the data represents expected values, as that would depend on chain
-- state, and ogmios itself.
suite :: TestPlanM Unit
suite = do
  group "Aff Int"
    $ test "UtxosAt"
    $ do
        ws <- mkOgmiosWebSocketAff "ws:127.0.0.1:1337"
        ( runReaderT
            ( do
                _utxoqr <- utxosAt' testnet_addr
                pure unit
            )
        )
          { ws }
