module QueryM.ChainInfo (getChainTip) where

import Contract.Prelude
  ( Effect
  , Either
  , Unit
  , liftAff
  , liftEffect
  , pure
  )
import Control.Bind (bind, discard)
import Control.Monad.Error.Class (throwError)
import Control.Monad.RWS (ask)
import Data.Function (($))
import Effect.Aff (Canceler(..), makeAff)
import QueryM
  ( QueryM
  , _stringify
  , _wsSend
  , allowError
  , listeners
  , underlyingWebSocket
  )
import Effect.Exception (Error)
import Types.JsonWsp as JsonWsp

-- | Get information about the current blockchain tip.
getChainTip :: QueryM JsonWsp.ChainTipQR
getChainTip = do
  body <- liftEffect $ JsonWsp.mkChainTipQuery
  let id = body.mirror.id
  sBody <- liftEffect $ _stringify body
  config <- ask
  let
    affFunc :: (Either Error JsonWsp.ChainTipQR -> Effect Unit) -> Effect Canceler
    affFunc cont = do
      let
        ls = listeners config.ogmiosWs
        ws = underlyingWebSocket config.ogmiosWs
      ls.chainTip.addMessageListener id
        ( \result -> do
            ls.chainTip.removeMessageListener id
            allowError cont $ result
        )
      _wsSend ws sBody
      pure $ Canceler $ \err -> do
        liftEffect $ ls.chainTip.removeMessageListener id
        liftEffect $ throwError $ err
  liftAff $ makeAff $ affFunc
