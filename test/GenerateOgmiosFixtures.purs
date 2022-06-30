module Test.GenerateOgmiosFixtures where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, Aeson, stringifyAeson)
import Contract.Monad (ListenerSet)
import Control.Parallel (parTraverse)
import Data.Either (Either(..))
import Data.Log.Level (LogLevel(..))
import Data.Traversable (for_, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throw)
import Effect.Ref as Ref
import Helpers (logString)
import JsWebSocket
  ( _mkWebSocket
  , _onWsConnect
  , _onWsError
  , _onWsMessage
  , _wsSend
  , _wsWatch
  )
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Path (concat)
import QueryM
  ( WebSocket(..)
  , defaultMessageListener
  , defaultOgmiosWsConfig
  , mkListenerSet
  , mkRequest
  , queryDispatch
  )
import QueryM.JsonWsp (JsonWspCall)
import QueryM.Ogmios (mkOgmiosCallType)
import QueryM.ServerConfig (ServerConfig, mkWsUrl)
import Type.Prelude (Proxy(..))
import Types.MultiMap as MultiMap
import Data.Map as Map

-- A simple websocket for testing
-- TODO Generalize websocket constructors using type classes traversing rows
-- to factor mk{Ogmios{DatumCache,},}WebSocket into a single implementation
mkWebSocket
  :: forall a b
   . DecodeAeson b
  => Show b
  => LogLevel
  -> ServerConfig
  -> (Either Error (WebSocket (ListenerSet a b)) -> Effect Unit)
  -> Effect (Error -> Effect Unit)
mkWebSocket lvl serverCfg cb = do
  dispatchMap <- Ref.new MultiMap.empty
  pendingRequests <- Ref.new Map.empty
  let
    md = [ queryDispatch dispatchMap ]
  ws <- _mkWebSocket (logger Debug) $ mkWsUrl serverCfg
  let
    sendRequest = _wsSend ws (logString lvl Debug)
    onError = do
      logString lvl Debug "WS error occured, resending requests"
      Ref.read pendingRequests >>= traverse_ sendRequest
  _onWsConnect ws do
    _wsWatch ws (logger Debug) onError
    _onWsMessage ws (logger Debug) $ defaultMessageListener lvl md
    _ <- _onWsError ws (logger Error) $ const onError
    cb $ Right $ WebSocket ws
      (mkListenerSet dispatchMap pendingRequests)
  pure $ \err -> cb $ Left $ err
  where
  logger :: LogLevel -> String -> Effect Unit
  logger = logString lvl

mkWebSocketAff
  :: forall a b
   . DecodeAeson b
  => Show b
  => LogLevel
  -> ServerConfig
  -> Aff (WebSocket (ListenerSet a b))
mkWebSocketAff lvl = makeAff <<< map (map (Canceler <<< (map liftEffect))) <<<
  mkWebSocket lvl

foreign import md5 :: String -> String

data Query = Query (JsonWspCall Unit Aeson) String

mkQuery :: forall query. EncodeAeson query => query -> String -> Query
mkQuery query shown = Query queryCall shown
  where
  queryCall = mkOgmiosCallType
    { methodname: "Query"
    , args: const { query }
    }
    Proxy

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
      resp <- mkRequest ws logLevel qc listeners unit
      pure { resp, query: shown }

    for_ resps \{ resp, query } -> do
      let
        resp' = stringifyAeson resp
        respMd5 = md5 resp'
        fp = concat
          [ "fixtures"
          , "test"
          , "ogmios"
          , query <> "-" <> respMd5 <> ".json"
          ]
      writeTextFile UTF8 fp resp'
    -- TODO Remove by properly shutting down the websocket
    liftEffect $ throw "terminating"
