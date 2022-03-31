module Seabug.CallContract (callMarketPlaceBuyTest, callMarketPlaceBuy) where

import Data.Unit (Unit)
import Data.Function (($))
import Data.Semigroup ((<>))
import Control.Bind (bind)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Aff (launchAff_)

import Contract.Monad (runContract_, defaultContractConfig)

import Seabug.Types (NftData)
import Seabug.Contract.MarketPlaceBuy (marketplaceBuy)

import Undefined (undefined)

callMarketPlaceBuyTest :: {name :: String } -> Effect Unit
callMarketPlaceBuyTest {name} = log $ "Aloha" <> name

callMarketPlaceBuy :: BuyNftArgs -> Effect Unit
callMarketPlaceBuy args = launchAff_ do
  contractConfig <- defaultContractConfig
  nftData <- liftEffect $ buildNftData args
  runContract_ contractConfig (marketplaceBuy nftData)

-- callMarketPlaceListNft :: Effect Unit
-- callMarketPlaceListNft = do
--   contractConfig <- defaultContractConfig
--   nftData <- liftEffect $ buildNftData args
--   utxos <- runContract contractConfig callMarketPlaceListNft
--   pututxosToNftList
                          -- UtxoM

type BuyNftArgs =
  { nftCollection ::
    { collectionNftCs :: String -- CurrencySymbol
    , lockLockup :: String -- BigInt
    , lockLockupEnd :: Number -- Slot
    , lockingScript :: String -- ValidatorHash
    , author :: String -- PaymentPubKeyHash
    , authorShare :: Number -- Natural
    , daoScript :: String -- ValidatorHash
    , daoShare :: Number -- Natural
    }
  , nftId ::
    { collectionNftTn :: String -- TokenName
    , price :: Number -- Natural
    , owner :: String -- PaymentPubKeyHash
    }
}

buildNftData :: BuyNftArgs -> Effect NftData
buildNftData = undefined

