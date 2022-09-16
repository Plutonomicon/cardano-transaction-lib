module Test.WebSocket (main)
  where

import Prelude

import Contract.Config (LogLevel(..), testnetConfig)
import Contract.Monad (launchAff_, mkContractEnv, runContractInEnv, stopContractEnv, wrapContract)
import Data.Either (Either(..))
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Aff (attempt, delay, message)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import JsWebSocket (_onWsError, _removeOnWsError)
import QueryM (mkLogger, underlyingWebSocket)
import QueryM.CurrentEpoch (getCurrentEpoch)

-- import QueryM.Ogmios (CurrentEpoch)

main ∷ Effect Unit
main = launchAff_ do
    cenv <- mkContractEnv (testnetConfig { suppressLogs = false })
    let ws = underlyingWebSocket (unwrap cenv).runtime.ogmiosWs
    result <- attempt $ runContractInEnv cenv $ wrapContract getCurrentEpoch
    case result of
        Left e -> log $ message e
        Right epoch -> log $ show epoch
    -- liftEffect $ _wsClose ws
    liftEffect $ stopContractEnv cenv
    delay (wrap 500.0)
    -- delay (wrap 500.0)
    wse <- liftEffect $ _onWsError ws \err -> do
        logger Debug $
          "Ogmios WebSocket error: (" <> err <> ")."
    liftEffect $ _removeOnWsError ws wse
    result2 <- attempt $ runContractInEnv cenv $ wrapContract getCurrentEpoch
    case result2 of
        Left e -> log $ message e
        Right epoch -> log $ show epoch

logger ∷ LogLevel → String → Effect Unit
logger = mkLogger testnetConfig.logLevel testnetConfig.customLogger