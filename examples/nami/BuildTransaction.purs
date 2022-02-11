module Examples.Nami.BuildTransaction (main) where

import Prelude

import BalanceTx (UnbalancedTransaction)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.Class (liftAff)
import QueryM (QueryM, defaultServerConfig, getWalletAddress)
import Undefined (undefined)
import Wallet (mkNamiWalletAff)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  tx <- runReaderT
    buildTransaction
    { ws: {-TODO-}  undefined
    , wallet
    , serverConfig: defaultServerConfig
    }
  undefined

buildTransaction :: QueryM UnbalancedTransaction
buildTransaction = do
  ownAddress <-
    maybe
      (throw "Failed to get wallet address")
      pure
      =<< getWalletAddress
  undefined

throw :: forall (a :: Type). String -> QueryM a
throw = liftAff <<< throwError <<< error
