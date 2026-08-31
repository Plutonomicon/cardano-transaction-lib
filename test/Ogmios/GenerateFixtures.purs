module Test.Ctl.Ogmios.GenerateFixtures
  ( main
  ) where

import Prelude

import Aeson (Aeson, stringifyAeson)
import Cardano.Kupmios
  ( KupmiosConfig
  , KupmiosEnv
  , KupmiosM
  , ogmiosQueryNoParams
  )
import Cardano.Kupmios.Ogmios.Types (class DecodeOgmios)
import Cardano.Provider (pprintOgmiosDecodeError)
import Contract.Config (defaultKupoServerConfig)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (runReaderT)
import Control.Parallel (parTraverse)
import Ctl.Internal.ServerConfig (defaultOgmiosServerConfig)
import Data.Either (Either(Left, Right))
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.Common (replace)
import Data.String.Pattern (Pattern(Pattern), Replacement(Replacement))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (writeTextFile)
import Node.Path (concat)
import Test.Ctl.Internal.Hashing (md5HashHex)

network :: String
network = "preview"

kupmiosConfig :: KupmiosConfig
kupmiosConfig =
  { ogmios:
      { serverConfig: defaultOgmiosServerConfig
      , requestRateLimiterCooldown: Nothing
      }
  , kupo:
      { serverConfig: defaultKupoServerConfig
      }
  , logLevel: Trace
  , customLogger: Nothing
  , suppressLogs: false
  }

kupmiosEnv :: KupmiosEnv
kupmiosEnv =
  { config: kupmiosConfig
  , ogmiosRequestRateLimiter: Nothing
  }

runKupmiosM :: forall (a :: Type). KupmiosM a -> Aff a
runKupmiosM =
  flip runReaderT kupmiosEnv
    <<< unwrap

newtype AesonResponse = AesonResponse Aeson

derive instance Newtype AesonResponse _

instance Show AesonResponse where
  show = show <<< unwrap

instance DecodeOgmios AesonResponse where
  decodeOgmios = pure <<< wrap

-- | To avoid creating directories, replace slashes with dashes
sanitiseMethod :: String -> String
sanitiseMethod = replace (Pattern "/") (Replacement "-")

queries :: Array String
queries =
  [ "queryNetwork/tip"
  , "queryNetwork/startTime"
  , "queryLedgerState/epoch"
  , "queryLedgerState/eraSummaries"
  , "queryLedgerState/protocolParameters"
  , "queryLedgerState/stakePools"
  ]

main :: Effect Unit
main =
  launchAff_ do
    resps <-
      parTraverse
        ( \method ->
            runKupmiosM (ogmiosQueryNoParams method) >>=
              case _ of
                Left ogmiosErr ->
                  throwError $ error $ "Ogmios request " <> method
                    <> " failed with error: "
                    <> pprintOgmiosDecodeError ogmiosErr
                Right (AesonResponse resp) ->
                  pure { resp, method }
        )
        queries
    for_ resps \{ resp, method } -> do
      let respStr = stringifyAeson resp
      respMd5 <- liftEffect $ md5HashHex respStr
      let
        fp = concat
          [ "fixtures"
          , "test"
          , "ogmios"
          , sanitiseMethod method <> "-" <> network <> "-" <> respMd5 <> ".json"
          ]
      writeTextFile UTF8 fp respStr
      log ("Written " <> fp)
