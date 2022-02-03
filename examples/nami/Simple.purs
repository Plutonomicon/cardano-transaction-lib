module Examples.Nami.Simple (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Wallet (Wallet(..), mkNamiWalletAff)


main :: Effect Unit
main = launchAff_ $ logWalletAddress

logWalletAddress :: Aff Unit
logWalletAddress = do
  Nami nami <- mkNamiWalletAff
  conn <- liftEffect $ Ref.read nami.connection
  address <- nami.getWalletAddress conn
  liftEffect $ Console.log $ show address
