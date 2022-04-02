module Seabug.CallContract (callMarketPlaceBuy, callMarketPlaceListNft) where

import Contract.Address (Slot(..))
import Contract.Monad (ContractConfig(..), defaultSlotConfig, mkDatumCacheWebSocketAff, mkOgmiosWebSocketAff, runContract, runContract_)
import Contract.Prelude (Aff, Either, Maybe(..), liftEither, liftM, note, pure, wrap, (<$>), (<<<), (<>), (=<<))
import Contract.Prim.ByteArray (byteArrayFromString, hexToByteArray)
import Contract.Scripts (ed25519KeyHashFromBech32, scriptHashFromBech32)
import Contract.Transaction (UtxoM)
import Contract.Value (mkCurrencySymbol, mkTokenName)
import Control.Bind (bind)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Function (($))
import Data.UInt as UInt
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (error)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Seabug.Contract.MarketPlaceBuy (marketplaceBuy)
import Seabug.Contract.MarketPlaceListNft (marketPlaceListNft)
import Seabug.Types (NftCollection(..), NftData(..), NftId(..))
import Serialization.Address (intToNetworkId)
import Types.Natural as Nat
import UsedTxOuts (newUsedTxOuts)
import Wallet (mkNamiWalletAff)

callMarketPlaceBuy :: ContractConfiguration -> BuyNftArgs -> Effect (Promise Unit)
callMarketPlaceBuy cfg args = Promise.fromAff do
  contractConfig <- buildContractConfig cfg
  nftData <- liftEffect $ liftEither $ buildNftData args
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
  { nftCollectionArgs ::
    { collectionNftCs :: String -- CurrencySymbol
    , lockLockup :: String -- BigInt
    , lockLockupEnd :: String -- Slot
    , lockingScript :: String -- ValidatorHash
    , author :: String -- PaymentPubKeyHash
    , authorShare :: String -- Natural
    , daoScript :: String -- ValidatorHash
    , daoShare :: String -- Natural
    }
  , nftIdArgs ::
    { collectionNftTn :: String -- TokenName
    , price :: String -- Natural
    , owner :: String -- PaymentPubKeyHash
    }
}

buildContractConfig :: ContractConfiguration -> Aff ContractConfig
buildContractConfig cfg = do
  server_port <- liftM (error "Invalid server port number")
    $ UInt.fromInt' cfg.server_port
  ogmios_port <- liftM (error "Invalid ogmios port number")
    $ UInt.fromInt' cfg.ogmios_port
  datum_cache_port <- liftM (error "Invalid datum cache port number")
    $ UInt.fromInt' cfg.datum_cache_port
  networkId <- liftM (error "Invalid network id")
    $ intToNetworkId cfg.networkId

  let serverConfig = { port: server_port, host: cfg.server_host, secure: cfg.server_secure_conn }
  wallet <- Just <$> mkNamiWalletAff

  ogmiosWs <- mkOgmiosWebSocketAff {port: ogmios_port, host: cfg.ogmios_host, secure: cfg.ogmios_secure_conn}
  datumCacheWs <- mkDatumCacheWebSocketAff {port: datum_cache_port, host: cfg.datum_cache_host, secure: cfg.datum_cache_secure_conn}
  usedTxOuts <- newUsedTxOuts
  pure $ ContractConfig
    { ogmiosWs
    , datumCacheWs
    , wallet
    , serverConfig
    , usedTxOuts
    , slotConfig: defaultSlotConfig
    , networkId
    , projectId: ""
    }

buildNftList :: UtxoM -> NftListing
buildNftList _ = []

buildNftData :: BuyNftArgs -> Either Error NftData
buildNftData {nftCollectionArgs, nftIdArgs} = do
  nftCollection <- mkCollection nftCollectionArgs
  nftId <- mkId nftIdArgs
  pure $ NftData {nftCollection, nftId}
  where
    mkId r = do
      tn <- note (error $ "Invalid collection token name: " <> r.collectionNftTn )
        $ mkTokenName =<< hexToByteArray r.collectionNftTn
      price <- note (error $ "Invalid price: " <> r.price )
        $ Nat.fromString r.price
      owner <- note (error $ "Invalid owner: " <> r.owner )
        $ wrap <<< wrap <$> ed25519KeyHashFromBech32 r.owner
      pure $ NftId
        { collectionNftTn: tn
        , price
        , owner
        }
    mkCollection r = do
      collectionNftCs <- note (error $ "Invalid collection currency symbol: " <> r.collectionNftCs)
        $ mkCurrencySymbol =<< byteArrayFromString r.collectionNftCs
      lockLockup <- note (error $ "Invalid nft lockLockup: " <> r.lockLockup)
        $ BigInt.fromString r.lockLockup
      lockLockupEnd <- note (error $ "Invalid nft lockLockupEnd: " <> r.lockLockupEnd)
        $ Slot <$> UInt.fromString r.lockLockupEnd
      lockingScript <- note (error $ "Invalid nft lockingScript: " <> r.lockingScript)
        $ wrap <$> scriptHashFromBech32 r.lockingScript
      author <- note (error $ "Invalid author: " <> r.author )
        $ wrap <<< wrap <$> ed25519KeyHashFromBech32 r.author
      authorShare <- note (error $ "Invalid authorShare: " <> r.authorShare )
        $ Nat.fromString r.authorShare
      daoScript <- note (error $ "Invalid nft daoScript: " <> r.daoScript)
        $ wrap <$> scriptHashFromBech32 r.daoScript
      daoShare <- note (error $ "Invalid daoShare: " <> r.daoShare )
        $ Nat.fromString r.daoShare
      pure $ NftCollection
        { collectionNftCs
        , lockLockup
        , lockLockupEnd
        , lockingScript
        , author
        , authorShare
        , daoScript
        , daoShare
        }
