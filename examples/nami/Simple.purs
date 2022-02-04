module Examples.Nami.Simple (main) where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Maybe (Maybe(Just))
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import QueryM (getWalletAddress, QueryM)
import Wallet (mkNamiWalletAff)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  runReaderT logWalletAddress { ws: undefined, wallet }

logWalletAddress :: QueryM Unit
logWalletAddress = do
  address <- getWalletAddress
  liftEffect $ Console.log $ show address
