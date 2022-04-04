module Seabug.CallContract (callMarketPlaceBuy, callMarketPlaceListNft, callMarketPlaceBuyTest) where

import Contract.Address (Slot(Slot))
import Contract.Monad
  ( ContractConfig(ContractConfig)
  , defaultSlotConfig
  , mkDatumCacheWebSocketAff
  , mkOgmiosWebSocketAff
  , runContract
  , runContract_
  )
import Contract.Numeric.Natural (toBigInt)
import Contract.Prelude
  ( Aff
  , Either
  , Maybe(Just)
  , fromMaybe
  , liftEither
  , liftM
  , note
  , pure
  , unwrap
  , wrap
  , (<$>)
  , (<<<)
  , (<>)
  , (=<<)
  , (>>>)
  , show
  )
import Contract.Prim.ByteArray
  ( byteArrayFromString
  , byteArrayToHex
  , hexToByteArray
  )
import Contract.Scripts
  ( ed25519KeyHashFromBech32
  , ed25519KeyHashToBech32Unsafe
  , scriptHashFromBech32
  , scriptHashToBech32Unsafe
  )
import Contract.Transaction
  ( TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  )
import Contract.Value
  ( Value
  , getCurrencySymbol
  , getNonAdaAsset
  , getTokenName
  , mkCurrencySymbol
  , mkTokenName
  , valueToCoin
  )
import Control.Bind (bind)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (singleton)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Function (($), (#))
import Data.List (toUnfoldable)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (error)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Metadata.Seabug (SeabugMetadata(SeabugMetadata))
import Metadata.Seabug.Share (unShare)
import Seabug.Contract.MarketPlaceBuy (marketplaceBuy)
import Seabug.Contract.MarketPlaceListNft (ListNftResult, marketPlaceListNft)
import Seabug.Types (NftCollection(NftCollection), NftData(NftData), NftId(NftId))
import Serialization.Address (addressBech32, intToNetworkId)
import Types.Natural as Nat
import Types.Value (flattenNonAdaValue)
import UsedTxOuts (newUsedTxOuts)
import Wallet (mkNamiWalletAff)

-- | Exists temporarily for testing purposes
callMarketPlaceBuyTest :: String -> Effect (Promise String)
callMarketPlaceBuyTest str = Promise.fromAff do
  pure str

-- | Calls Seabugs marketplaceBuy and takes care of converting data types.
--   Returns a JS promise holding no data.
callMarketPlaceBuy :: ContractConfiguration -> BuyNftArgs -> Effect (Promise Unit)
callMarketPlaceBuy cfg args = Promise.fromAff do
  contractConfig <- buildContractConfig cfg
  nftData <- liftEffect $ liftEither $ buildNftData args
  runContract_ contractConfig (marketplaceBuy nftData)

-- | Calls Seabugs marketPlaceListNft and takes care of converting data types.
--   Returns a JS promise holding nft listings.
callMarketPlaceListNft :: ContractConfiguration -> Effect (Promise (Array ListNftResultOut))
callMarketPlaceListNft cfg = Promise.fromAff do
  contractConfig <- buildContractConfig cfg
  listnft <- runContract contractConfig marketPlaceListNft
  pure $ buildNftList <$> listnft

-- | Configuation needed to call contracts from JS.
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

type BuyNftArgs =
  { nftCollectionArgs ::
      { collectionNftCs :: String -- CurrencySymbol
      , lockLockup :: BigInt -- BigInt
      , lockLockupEnd :: BigInt -- Slot
      , lockingScript :: String -- ValidatorHash
      , author :: String -- PaymentPubKeyHash
      , authorShare :: BigInt -- Natural
      , daoScript :: String -- ValidatorHash
      , daoShare :: BigInt -- Natural
      }
  , nftIdArgs ::
      { collectionNftTn :: String -- TokenName
      , price :: BigInt -- Natural
      , owner :: String -- PaymentPubKeyHash
      }
  }

-- Placeholder for types I'm not sure how should we represent on frontend.
type ValueOut = Array { currencySymbol :: String, tokenName :: String, amount :: BigInt }

type ListNftResultOut =
  { input :: { transaction_id :: String, input_index :: Int }
  , output :: { address :: String, value :: ValueOut, data_hash :: String }
  , metadata ::
      { seabugMetadata ::
          { policyId :: String --MintingPolicyHash
          , mintPolicy :: String --ByteArray
          , collectionNftCS :: String -- CurrencySymbol
          , collectionNftTN :: String -- TokenName
          , lockingScript :: String --ValidatorHash
          , authorPkh :: String -- PubKeyHash
          , authorShare :: BigInt -- Share
          , marketplaceScript :: String -- ValidatorHash
          , marketplaceShare :: BigInt -- share
          , ownerPkh :: String -- PubKeyHash
          , ownerPrice :: BigInt --Natural
          }
      , ipfsHash :: String
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

  ogmiosWs <- mkOgmiosWebSocketAff { port: ogmios_port, host: cfg.ogmios_host, secure: cfg.ogmios_secure_conn }
  datumCacheWs <- mkDatumCacheWebSocketAff { port: datum_cache_port, host: cfg.datum_cache_host, secure: cfg.datum_cache_secure_conn }
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

buildNftList :: ListNftResult -> ListNftResultOut
buildNftList { input: TransactionInput input, output: TransactionOutput output, metadata } =
  let
    transaction_id = byteArrayToHex $ unwrap input.transaction_id
    input_index = UInt.toInt input.index
    address = addressBech32 output.address
    data_hash = fromMaybe "" $ byteArrayToHex <<< unwrap <$> output.data_hash
    ipfsHash = metadata.ipfsHash
    seabugMetadata = convertSeabugMetaData metadata.seabugMetadata
  in
    { input: { transaction_id, input_index }
    , output: { address, data_hash, value: convertValue output.amount }
    , metadata: { ipfsHash, seabugMetadata }
    }

  where

  convertValue :: Value -> ValueOut
  convertValue v =
    let
      nonAda = singleValueRecord <$> (toUnfoldable $ flattenNonAdaValue $ getNonAdaAsset v)
      ada = valueToCoin v # unwrap >>> { amount: _, currencySymbol: "", tokenName: "" }
    in
      singleton ada <> nonAda

    where
    singleValueRecord (cursym /\ tokname /\ amount) =
      { amount
      , currencySymbol: byteArrayToHex $ getCurrencySymbol cursym
      , tokenName: byteArrayToHex $ getTokenName tokname
      }

  convertSeabugMetaData :: SeabugMetadata -> _
  convertSeabugMetaData (SeabugMetadata m) =
    { policyId: scriptHashToBech32Unsafe "policy_vkh" $ unwrap m.policyId -- or the prefix should just be 'script'
    , mintPolicy: byteArrayToHex m.mintPolicy
    , collectionNftCS: byteArrayToHex $ getCurrencySymbol m.collectionNftCS
    , collectionNftTN: byteArrayToHex $ getTokenName m.collectionNftTN
    , lockingScript: scriptHashToBech32Unsafe "script" $ unwrap m.lockingScript
    , authorPkh: ed25519KeyHashToBech32Unsafe "addr_vkh" $ unwrap m.authorPkh
    , authorShare: unShare m.authorShare
    , marketplaceScript: scriptHashToBech32Unsafe "script" $ unwrap m.marketplaceScript
    , marketplaceShare: unShare m.marketplaceShare
    , ownerPkh: ed25519KeyHashToBech32Unsafe "addr_vkh" $ unwrap m.ownerPkh
    , ownerPrice: toBigInt m.ownerPrice
    }

buildNftData :: BuyNftArgs -> Either Error NftData
buildNftData { nftCollectionArgs, nftIdArgs } = do
  nftCollection <- mkCollection nftCollectionArgs
  nftId <- mkId nftIdArgs
  pure $ NftData { nftCollection, nftId }
  where
  mkId r = do
    tn <- note (error $ "Invalid collection token name: " <> r.collectionNftTn)
      $ mkTokenName =<< hexToByteArray r.collectionNftTn
    price <- note (error $ "Invalid price: " <> show r.price)
      $ Nat.fromBigInt r.price
    owner <- note (error $ "Invalid owner: " <> r.owner)
      $ wrap <<< wrap <$> ed25519KeyHashFromBech32 r.owner
    pure $ NftId
      { collectionNftTn: tn
      , price
      , owner
      }
  mkCollection r = do
    collectionNftCs <- note (error $ "Invalid collection currency symbol: " <> r.collectionNftCs)
      $ mkCurrencySymbol =<< byteArrayFromString r.collectionNftCs
    lockLockupEnd <- note (error $ "Invalid nft lockLockupEnd: " <> show r.lockLockupEnd)
      $ Slot <$> (UInt.fromString $ BigInt.toString r.lockLockupEnd)
    lockingScript <- note (error $ "Invalid nft lockingScript: " <> r.lockingScript)
      $ wrap <$> scriptHashFromBech32 r.lockingScript
    author <- note (error $ "Invalid author: " <> r.author)
      $ wrap <<< wrap <$> ed25519KeyHashFromBech32 r.author
    authorShare <- note (error $ "Invalid authorShare: " <> show r.authorShare)
      $ Nat.fromBigInt r.authorShare
    daoScript <- note (error $ "Invalid nft daoScript: " <> r.daoScript)
      $ wrap <$> scriptHashFromBech32 r.daoScript
    daoShare <- note (error $ "Invalid daoShare: " <> show r.daoShare)
      $ Nat.fromBigInt r.daoShare
    pure $ NftCollection
      { collectionNftCs
      , lockLockup: r.lockLockup
      , lockLockupEnd
      , lockingScript
      , author
      , authorShare
      , daoScript
      , daoShare
      }
