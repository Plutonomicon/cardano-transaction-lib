module Test.Ctl.Ogmios.GenerateFixtures
  ( main
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, Aeson, stringifyAeson)
import Control.Parallel (parTraverse)
import Ctl.Internal.Hashing (md5HashHex)
import Ctl.Internal.Helpers (logString)
import Ctl.Internal.JsWebSocket
  ( _mkWebSocket
  , _onWsConnect
  , _onWsError
  , _onWsMessage
  , _wsClose
  , _wsSend
  )
import Ctl.Internal.QueryM
  ( ListenerSet
  , WebSocket(WebSocket)
  , WebsocketDispatch
  , defaultMessageListener
  , defaultOgmiosWsConfig
  , mkListenerSet
  , mkRequestAff
  , mkWebsocketDispatch
  )
import Ctl.Internal.QueryM.JsonWsp (JsonWspCall)
import Ctl.Internal.QueryM.Ogmios (mkOgmiosCallType)
import Ctl.Internal.ServerConfig (ServerConfig, mkWsUrl)
import Data.Either (Either(Left, Right))
import Data.Log.Level (LogLevel(Trace, Debug))
import Data.Map as Map
import Data.Traversable (for_, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Ref as Ref
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (writeTextFile)
import Node.Path (concat)

-- A simple websocket for testing
mkWebSocket
  :: forall (a :: Type) (b :: Type)
   . DecodeAeson b
  => Show b
  => LogLevel
  -> ServerConfig
  -> (Either Error (WebSocket (ListenerSet a b)) -> Effect Unit)
  -> Effect (Error -> Effect Unit)
mkWebSocket lvl serverCfg cb = do
  dispatcher <- Ref.new Map.empty
  pendingRequests <- Ref.new Map.empty
  let
    (messageDispatch :: WebsocketDispatch) =
      mkWebsocketDispatch dispatcher
  ws <- _mkWebSocket (logger Debug) $ mkWsUrl serverCfg
  let
    sendRequest :: String -> Effect Unit
    sendRequest = _wsSend ws (logString lvl Debug)
    onError = do
      logString lvl Debug "WS error occured, resending requests"
      Ref.read pendingRequests >>= traverse_ sendRequest
  _onWsConnect ws do
    void $ _onWsError ws \_ -> onError
    _onWsMessage ws (logger Debug) $ defaultMessageListener (\_ _ -> pure unit)
      [ messageDispatch ]
    void $ _onWsError ws $ const onError
    cb $ Right $ WebSocket ws
      (mkListenerSet dispatcher pendingRequests)
  pure $ \err -> cb $ Left $ err
  where
  logger :: LogLevel -> String -> Effect Unit
  logger = logString lvl

mkWebSocketAff
  :: forall (a :: Type) (b :: Type)
   . DecodeAeson b
  => Show b
  => LogLevel
  -> ServerConfig
  -> Aff (WebSocket (ListenerSet a b))
mkWebSocketAff lvl = makeAff <<< map (map (Canceler <<< map liftEffect)) <<<
  mkWebSocket lvl

data Query = Query (JsonWspCall Unit Aeson) String

mkQuery :: forall (query :: Type). EncodeAeson query => query -> String -> Query
mkQuery query shown = Query queryCall shown
  where
  queryCall = mkOgmiosCallType
    { methodname: "Query"
    , args: const { query }
    }

mkQuery' :: String -> Query
mkQuery' query = mkQuery query query

main :: Effect Unit
main =
  launchAff_ do
    let logLevel = Trace
    WebSocket ws listeners <- mkWebSocketAff logLevel defaultOgmiosWsConfig

    let
      addresses =
        [ "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc"
        , "addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla"
        , "addr_test1vz5rd5hsead7gcgn6zx6nalqxz6zlvdmg89kswl935dfh8cqn5kcy"
        , "addr_test1vrx0w7gndt6gk9svrksmpg23lmwmlcx2w2fre7rk27r8gdcyazwxm"
        , "addr_test1vp842vatdp6qxqnhcfhh6w83t6c8c5udhua999slgzwcq2gvgpvm9"
        , "addr_test1vrmet2lzexpmw78jpyqkuqs8ktg80x457h6wcnkp3z63etsx3pg70"
        , "addr_test1qpsfwsr4eqjfe49md9wpnyp3ws5emf4z3k6xqagvm880zgnk2wgk4"
            <> "wl2rz04eaqmq9fnxhyn56az0c4d3unvcvg2yw4qmkmv4t"
        , "addr1q9d34spgg2kdy47n82e7x9pdd6vql6d2engxmpj20jmhuc2047yqd4xnh7"
            <> "u6u5jp4t0q3fkxzckph4tgnzvamlu7k5psuahzcp"
        ]
    let
      queries =
        [ mkQuery' "currentProtocolParameters"
        , mkQuery' "eraSummaries"
        , mkQuery' "currentEpoch"
        , mkQuery' "systemStart"
        , mkQuery' "chainTip"
        ] <> flip map addresses \addr -> mkQuery { utxo: [ addr ] } "utxo"
    resps <- flip parTraverse queries \(Query qc shown) -> do
      resp <- mkRequestAff listeners ws (\_ _ -> pure unit) qc identity unit
      pure { resp, query: shown }

    for_ resps \{ resp, query } -> do
      let resp' = stringifyAeson resp
      respMd5 <- liftEffect $ md5HashHex resp'
      let
        fp = concat
          [ "fixtures"
          , "test"
          , "ogmios"
          , query <> "-" <> respMd5 <> ".json"
          ]
      writeTextFile UTF8 fp resp'
      log ("Written " <> fp)
    liftEffect $ _wsClose ws
