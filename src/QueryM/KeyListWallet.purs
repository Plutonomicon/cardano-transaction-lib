module QueryM.KeyListWallet where

import Control.Monad.Reader (ask, local)
import Data.Array ((!!), length)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Prelude
import QueryM (QueryM)
import Wallet (Wallet(KeyListWallet))

withWallet :: forall a. Int -> QueryM a -> QueryM a
withWallet idx action = do
  cfg <- ask
  case cfg.wallet of
    Just (KeyListWallet klw@{ available }) ->
      case available !! idx of
        Nothing -> liftEffect $ throw $
          "withWallet: Wallet index " <> show idx <> " is not in the list. "
            <> "List length: "
            <> show (length available)
        Just selected -> do
          let
            setUpdatedWallet _ = cfg
              { wallet = Just $ KeyListWallet klw { selected = Just selected } }
          local setUpdatedWallet action
    _ -> liftEffect $ throw
      "withWallet: unable to use, wallet is not KeyListWallet"
