module QueryM.Submit
  ( submit
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (asks)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Canceler(Canceler), makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import QueryM (QueryM, _wsSend, allowError, listeners, underlyingWebSocket)
import Types.JsonWsp as JsonWsp
import Data.Argonaut as Json
import Types.ByteArray (ByteArray)

submit :: ByteArray -> QueryM String
submit txCbor = do
  body <- liftEffect $ JsonWsp.mkJsonWspSubmit txCbor
  let id = body.mirror.id
  ogmiosWs <- asks _.ogmiosWs
  let sBody = Json.stringify $ Json.encodeJson body
  -- TODO: find a way to factor out this and similar code in utxosAt'
  let
    affFunc :: (Either Error String -> Effect Unit) -> Effect Canceler
    affFunc cont = do
      let
        ls = listeners ogmiosWs
        ws = underlyingWebSocket ogmiosWs
      ls.submit.addMessageListener id
        ( \result -> do
            ls.submit.removeMessageListener id
            allowError cont $ result
        )
      _wsSend ws sBody
      pure $ Canceler $ \err -> do
        liftEffect $ ls.submit.removeMessageListener id
        liftEffect $ throwError $ err
  liftAff $ makeAff $ affFunc
