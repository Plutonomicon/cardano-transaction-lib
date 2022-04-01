module Seabug.CallContract (callMarketPlaceBuy, callMarketPlaceListNft) where

import Contract.Monad (ContractConfig, runContract, runContract_)
import Contract.Prelude (Aff, pure)
import Contract.Transaction (UtxoM)
import Control.Bind (bind)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.BigInt (BigInt)
import Data.Function (($))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class (liftEffect)
import Seabug.Contract.MarketPlaceBuy (marketplaceBuy)
import Seabug.Contract.MarketPlaceListNft (marketPlaceListNft)
import Seabug.Types (NftData)
import Undefined (undefined)

callMarketPlaceBuy :: ContractConfiguration -> BuyNftArgs -> Effect (Promise Unit)
callMarketPlaceBuy cfg args = Promise.fromAff do
  contractConfig <- buildContractConfig cfg
  nftData <- liftEffect $ buildNftData args
  runContract_ contractConfig (marketplaceBuy nftData)

callMarketPlaceListNft :: ContractConfiguration -> Effect (Promise NftListing)
callMarketPlaceListNft cfg = Promise.fromAff do
  contractConfig <- buildContractConfig cfg
  utxos <- runContract contractConfig marketPlaceListNft
  pure $ buildNftList utxos

type ContractConfiguration =
  { server_host :: String
  , server_port :: Int
  , server_secure_conn :: Boolean
  , ogmios_host :: String
  , ogmios_port :: Int
  , ogmios_secure_conn :: Boolean
  , datum_cache_host :: String
  , datum_cache_port :: Int
  , datum_cache_secure_conn :: Boolean
  , networkId :: Int
  }

type NftListing = Array {currencySymbol :: String, tokenName :: String, amount :: BigInt}

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

buildContractConfig :: ContractConfiguration -> Aff ContractConfig
buildContractConfig = undefined

buildNftList :: UtxoM -> NftListing
buildNftList _ = []

buildNftData :: BuyNftArgs -> Effect NftData
buildNftData = undefined
