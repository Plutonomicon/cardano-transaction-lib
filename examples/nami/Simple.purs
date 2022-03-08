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
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(Just))
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import QueryM
  ( QueryM
  , getWalletAddress
  , getWalletCollateral
  , defaultServerConfig
  )
import Wallet (mkNamiWalletAff)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  runReaderT
    walletActions
    { ws: {-TODO-}  undefined
    , wallet
    , serverConfig: defaultServerConfig
    }
  where
  walletActions :: QueryM Unit
  walletActions = sequence_ [ logWalletAddress, logWalletCollateral ]

logWalletAddress :: QueryM Unit
logWalletAddress = logWallet getWalletAddress

logWalletCollateral :: QueryM Unit
logWalletCollateral = logWallet getWalletCollateral

logWallet :: forall (a :: Type). Show a => QueryM a -> QueryM Unit
logWallet act = liftEffect <<< Console.log <<< show =<< act
