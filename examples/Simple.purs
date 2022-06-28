module Examples.Simple where

import Effect
import Effect.Aff
import Effect.Class.Console
import Prelude
import QueryM

import Aeson (class DecodeAeson, class EncodeAeson, Aeson, encodeAeson, stringifyAeson)
import Data.Either (Either)
import Data.Log.Level (LogLevel(..))
import QueryM.JsonWsp (JsonWspCall)
import QueryM.Ogmios (mkOgmiosCallType)
import Type.Prelude (Proxy(..))

-- SOmething odd is happening
-- WS is regularly disconnecting, and when it reconnects, multiple (old?) messages are recieved
-- TODO Look at what vladmir found

queryCall :: forall a b. EncodeAeson a => JsonWspCall a b
queryCall = mkOgmiosCallType
  { methodname: "Query"
  , args: identity
  }
  Proxy

main :: Effect Unit
main =
  launchAff_ $ do
     --(traceQueryConfig >>= flip runQueryM (getChainTip)) >>= logShow
     WebSocket ws listeners <- mkWebSocketAff Trace defaultOgmiosWsConfig

     let go = do
           resp <- mkRequest ws Trace queryCall listeners ({ query: "chainTip" })
           log $ stringifyAeson resp
           delay (Milliseconds 1000.0)
           go
     go
     -- mkReque

