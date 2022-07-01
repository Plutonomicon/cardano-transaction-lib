module Test.OgmiosAeson
  ( suite
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeJsonString)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (catMaybes, elem, filter, groupAllBy, notElem, nubBy)
import Data.Array.NonEmpty (head, length, tail, unzip)
import Data.Bifunctor (bimap)
import Data.Either (hush)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Profunctor.Strong (first)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (for_)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, skip, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath, basename, concat)
import Node.Process (lookupEnv)
import QueryM.Ogmios as O
import TestM (TestPlanM)
import Type.Proxy (Proxy(Proxy))

supported :: Array String
supported =
  [ "chainTip"
  , "utxo"
  , "currentEpoch"
  , "systemStart"
  , "eraSummaries"
  , "currentProtocolParameters"
  ]

-- Some fixtures using features we don't yet support
blacklist :: Array String
blacklist =
  [ -- EraMismatch
    "currentProtocolParameters-a9df54ac55c8156d849806393b2c490d.json"
  , "currentProtocolParameters-f7482203ea6d735521aaa13d3bdbcc8c.json"
  , "utxo-a9df54ac55c8156d849806393b2c490d.json"
  , "utxo-f7482203ea6d735521aaa13d3bdbcc8c.json"

  -- plutus:v1 parameters are required
  -- ```bash
  -- $ find $OGMIOS_FIXTURES -type f -name "currentProtocolParameters-*" \
  --   -printf "%f," \
  --   -exec jq 'type=="object" and has("costModels") and (.costModels | has("plutus:v1") | not)' {} \; \
  --   | grep ",true" | cut -d, -f1
  -- ```
  -- TODO Support plutus:v2 parameters
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/567
  , "currentProtocolParameters-c049b0e3a9fbd2fb9d03c94d4bda5592.json"
  , "currentProtocolParameters-2894b0c38840381075e4128bb3486ccb.json"
  , "currentProtocolParameters-5f5a6f750e60d05b6b74fb5a9b1af79e.json"
  , "currentProtocolParameters-e9347658bbf614f89a653a1033487b91.json"
  , "currentProtocolParameters-55fb137fb35bedd06e5eac9291f9a5f5.json"
  , "currentProtocolParameters-22fda9b587630ce78e554e7335ed457b.json"
  , "currentProtocolParameters-332eceb06bd8fd58ab8decd28071a7e1.json"
  , "currentProtocolParameters-1c86bb69adac17e04268ac3319dd9771.json"
  , "currentProtocolParameters-681ece14f9cbad322e917072d7d7ce47.json"
  , "currentProtocolParameters-d5442fd138a753910fee125d3968430b.json"
  , "currentProtocolParameters-a1d50f5eb616d2d591bb5ae724c480e5.json"
  , "currentProtocolParameters-c3f98175f347fb9285a3eb5abf0516e7.json"
  , "currentProtocolParameters-05493b86700847c3e2a85225db3d7524.json"
  , "currentProtocolParameters-c393b8281d5a28debec9c6237335732f.json"
  , "currentProtocolParameters-75e017cedc41e1651aac57b110eff62f.json"
  , "currentProtocolParameters-1f7ac8a75143ad8e7cb31798ac31ce4d.json"
  , "currentProtocolParameters-94cdda40a8754ec01cae92b04c14419e.json"
  , "currentProtocolParameters-caeded00736c10636e793e858ae3db77.json"
  , "currentProtocolParameters-510cbba0ff0073645e50833663685005.json"
  , "currentProtocolParameters-a036ce190522d9179b8973e859b6b1fb.json"
  , "currentProtocolParameters-07543223311a4833b28efb3cfaae5dfe.json"
  , "currentProtocolParameters-0e3a22aec366b248438218aaa8b6aca3.json"
  , "currentProtocolParameters-dea4789b97acd3d93093602ff20dd894.json"
  , "currentProtocolParameters-38bb1d48d5ffe9fec6a8142daf0b97b8.json"
  , "currentProtocolParameters-74617e35f22ba640e2795fcd7bb06eb6.json"
  , "currentProtocolParameters-8695321309b9585733f909146f3569c8.json"
  , "currentProtocolParameters-0b21b1266d18c38b0de438aec9ff35e9.json"
  , "currentProtocolParameters-21d28e2ff2c1444881981691275a03c5.json"
  , "currentProtocolParameters-a71219b654ce7845008e57694a191596.json"
  , "currentProtocolParameters-dbedacfa70e52c3cf988da6b934a7045.json"
  , "currentProtocolParameters-93f665615e66d4a9e3c43f2e2397bba8.json"
  , "currentProtocolParameters-f8a16da4ad392ade0beeb88649dc919c.json"
  , "currentProtocolParameters-ad039396e5ab412e13993a61c21d3dde.json"
  , "currentProtocolParameters-927e2bb59d0605c010a8b91357004752.json"
  , "currentProtocolParameters-25a918f027658bdcefd691506a922285.json"
  , "currentProtocolParameters-b3636cfb4c44e4ddef68f8a15bb8fee4.json"
  , "currentProtocolParameters-57b2c7958a5ccb7c32f6b38cf55d0ca8.json"
  , "currentProtocolParameters-dba89714c6ffd4cdd274d64ddb9a2b83.json"
  , "currentProtocolParameters-613017944d62e2adc9f821e8f7ba74ab.json"
  , "currentProtocolParameters-1efe99bb5dccb120344ff408bd4bc1a9.json"
  , "currentProtocolParameters-cdf9f4e82498431f5c711197d167a068.json"
  , "currentProtocolParameters-8ac5279b95f265ad1e8f90ad26c998f2.json"
  , "currentProtocolParameters-486e68919a7e68698d06ec71d99d99a6.json"
  , "currentProtocolParameters-99f4669e15504e97186bcb46ccce4262.json"
  , "currentProtocolParameters-b45fa5461c7dc52d68cf65450fdf4360.json"
  , "currentProtocolParameters-f8f2f251841fb31621bf64f2bf7a62d3.json"
  , "currentProtocolParameters-fe202d18562ac1cd3f2c7984e18f00cd.json"
  , "currentProtocolParameters-d16b60e5b621fe3eebd3a0cc5b8867e9.json"
  , "currentProtocolParameters-8ae2d7d796d2b6c3178e30791c3c72a6.json"
  , "currentProtocolParameters-814d0d901045a0b1fcfaba14b38a35d5.json"
  ]

readdir' :: FilePath -> Aff (Array FilePath)
readdir' fp = (map <<< map) (\fn -> concat [ fp, fn ]) (readdir fp)

suite :: TestPlanM Unit
suite = group "Ogmios Aeson tests" do
  let
    path = concat [ "fixtures", "test", "ogmios" ]
    pattern = hush $ regex "^([a-zA-Z]+)-[0-9a-fA-F]+\\.json$" noFlags

  files <- lift do
    ourFixtures <- readdir' path
    ogmiosFixtures <- (liftEffect $ lookupEnv "OGMIOS_FIXTURES") >>= maybe
      (liftEffect $ throw $ "OGMIOS_FIXTURES environment variable not set")
      readdir'
    pure
      ( filter (\fp -> basename fp `notElem` blacklist) $ ourFixtures <>
          ogmiosFixtures
      )

  let
    groupedFiles = map (first head <<< unzip)
      $ groupAllBy (comparing fst)
      $ nubBy (comparing (basename <<< snd))
      $ catMaybes
      $ flip map files \fp ->
          case pattern >>= flip match (basename fp) >>> map tail of
            Just [ Just query ] -> Just (query /\ fp)
            _ -> Nothing

  for_ groupedFiles \(query /\ fps) ->
    (if query `elem` supported then identity else skip)
      $ test (query <> " (" <> show (length fps) <> ")")
      $
        for_ fps \fp -> do
          file <- readTextFile UTF8 fp
          let
            handle :: forall (a :: Type). DecodeAeson a => Proxy a -> Aff Unit
            handle _ = liftEither $ bimap
              ( error <<< ((basename fp <> "\n  ") <> _) <<<
                  printJsonDecodeError
              )
              (const unit)
              (decodeJsonString file :: _ a)
          case query of
            "chainTip" -> handle (Proxy :: _ O.ChainTipQR)
            "utxo" -> handle (Proxy :: _ O.UtxoQR)
            "currentEpoch" -> handle (Proxy :: _ O.CurrentEpoch)
            "systemStart" -> handle (Proxy :: _ O.SystemStart)
            "eraSummaries" -> handle (Proxy :: _ O.EraSummaries)
            "currentProtocolParameters" -> handle
              (Proxy :: _ O.ProtocolParameters)
            _ -> liftEffect $ throw $ "Unknown case " <> basename fp
