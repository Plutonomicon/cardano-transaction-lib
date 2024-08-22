module Test.Ctl.Blockfrost.GenerateFixtures.Helpers
  ( blockfrostBackend
  , contractParams
  , getBlockfrostApiKeyFromEnv
  , getSkeyFilepathFromEnv
  , lookupEnv'
  , storeBlockfrostFixture
  ) where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , ServerConfig
  , WalletSpec(UseKeys)
  , blockfrostPublicMainnetServerConfig
  , blockfrostPublicPreprodServerConfig
  , blockfrostPublicPreviewServerConfig
  , testnetConfig
  )
import Ctl.Internal.Contract.QueryBackend
  ( BlockfrostBackend
  , defaultConfirmTxDelay
  , mkBlockfrostBackendParams
  )
import Ctl.Internal.ServerConfig (blockfrostPublicSanchonetServerConfig)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (take) as String
import Effect.Exception (throw)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (writeTextFile)
import Node.FS.Sync (exists)
import Node.Path (concat)
import Node.Process (lookupEnv)
import Test.Ctl.Internal.Hashing (md5HashHex)

blockfrostBackend :: Effect BlockfrostBackend
blockfrostBackend = do
  blockfrostApiKey <- getBlockfrostApiKeyFromEnv
  blockfrostConfig <- blockfrostConfigFromApiKey blockfrostApiKey
  pure
    { blockfrostConfig
    , blockfrostApiKey: Just blockfrostApiKey
    , confirmTxDelay: defaultConfirmTxDelay
    }

contractParams :: Effect ContractParams
contractParams = do
  blockfrostApiKey <- getBlockfrostApiKeyFromEnv
  skeyFilepath <- getSkeyFilepathFromEnv
  blockfrostConfig <- blockfrostConfigFromApiKey blockfrostApiKey
  pure $ testnetConfig
    { backendParams =
        mkBlockfrostBackendParams
          { blockfrostConfig
          , blockfrostApiKey: Just blockfrostApiKey
          , confirmTxDelay: defaultConfirmTxDelay
          }
    , logLevel = Info
    , walletSpec = Just $ UseKeys (PrivatePaymentKeyFile skeyFilepath) Nothing
        Nothing
    }

blockfrostConfigFromApiKey :: String -> Effect ServerConfig
blockfrostConfigFromApiKey = String.take networkPrefixLength >>> case _ of
  "mainnet" ->
    pure blockfrostPublicMainnetServerConfig
  "preview" ->
    pure blockfrostPublicPreviewServerConfig
  "preprod" ->
    pure blockfrostPublicPreprodServerConfig
  "sanchon" ->
    pure blockfrostPublicSanchonetServerConfig
  _ ->
    throw "Failed to derive server config from Blockfrost API key"
  where
  networkPrefixLength :: Int
  networkPrefixLength = 7

getBlockfrostApiKeyFromEnv :: Effect String
getBlockfrostApiKeyFromEnv = lookupEnv' "BLOCKFROST_API_KEY"

getSkeyFilepathFromEnv :: Effect String
getSkeyFilepathFromEnv = lookupEnv' "SKEY_FILEPATH"

lookupEnv' :: String -> Effect String
lookupEnv' var =
  lookupEnv var >>=
    maybe (throw $ var <> " environment variable not set") pure

storeBlockfrostFixture :: Int -> String -> String -> Aff Unit
storeBlockfrostFixture i query resp = do
  respHash <- liftEffect $ md5HashHex resp
  let
    filename = query <> "-" <> respHash <> ".json"
    fp = concat [ "fixtures", "test", "blockfrost", query, filename ]
  liftEffect (exists fp) >>= flip unless
    ( writeTextFile UTF8 fp resp
        *> log ("Successfully saved fixture #" <> show i <> " to: " <> fp)
    )
