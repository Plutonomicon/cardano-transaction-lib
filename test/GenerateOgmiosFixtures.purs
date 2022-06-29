module Test.GenerateOgmiosFixtures where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import QueryM (WebSocket(..), defaultOgmiosWsConfig, mkRequest, mkWebSocketAff)


import Aeson
  ( class EncodeAeson
  , Aeson
  , stringifyAeson
  )
import Control.Parallel (parTraverse)
import Data.Log.Level (LogLevel(..))
import Data.Traversable (for_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Path (concat)
import QueryM.JsonWsp (JsonWspCall)
import QueryM.Ogmios (mkOgmiosCallType)
import Type.Prelude (Proxy(..))

foreign import md5 :: String -> String

-- Something odd is happening
-- WS is regularly disconnecting, and when it reconnects, multiple (old?) messages are recieved
-- TODO Look at what vladmir found

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
      go = do
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
            ] <>
              (flip map addresses \addr -> mkQuery { utxo: [ addr ] } "utxosAt")
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
        liftEffect $ throw "terminating"
    -- todo try liftEffect throw again
    -- delay (Milliseconds (60.0*1000.0))
    -- go
    go
