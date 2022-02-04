-- Examples of how to use the Nami wallet interface through `QueryM`
--
-- To run: `npm run dev` and visit `localhost:4008` in your browser. Make sure
-- that you have the Nami browser extension installed. Allow the page to access
-- your wallet when prompted by Nami
--
-- NOTE: due to Nami's limitations, only chromium-based browsers are supported

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
  runReaderT logWalletAddress { ws: {-TODO-} undefined, wallet }

logWalletAddress :: QueryM Unit
logWalletAddress = liftEffect <<< Console.log <<< show =<< getWalletAddress
