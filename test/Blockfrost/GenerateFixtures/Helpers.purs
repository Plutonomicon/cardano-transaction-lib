module Test.Ctl.Blockfrost.GenerateFixtures.Helpers
  ( contractParams
  , lookupEnv'
  , md5
  , storeBlockfrostFixture
  ) where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , WalletSpec(UseKeys)
  , blockfrostPublicPreviewServerConfig
  , testnetConfig
  )
import Ctl.Internal.Contract.QueryBackend (mkBlockfrostBackendParams)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Effect.Exception (throw)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (writeTextFile)
import Node.Path (concat)
import Node.Process (lookupEnv)

foreign import md5 :: String -> String

contractParams :: Effect ContractParams
contractParams = do
  blockfrostApiKey <- lookupEnv' "BLOCKFROST_API_KEY"
  skeyFilepath <- lookupEnv' "SKEY_FILEPATH"
  pure $ testnetConfig
    { backendParams =
        mkBlockfrostBackendParams
          { blockfrostConfig: blockfrostPublicPreviewServerConfig
          , blockfrostApiKey: Just blockfrostApiKey
          }
    , logLevel = Info
    , walletSpec = Just $ UseKeys (PrivatePaymentKeyFile skeyFilepath) Nothing
    }

lookupEnv' :: String -> Effect String
lookupEnv' var =
  lookupEnv var >>=
    maybe (throw $ var <> " environment variable not set") pure

storeBlockfrostFixture :: Int -> String -> String -> Aff Unit
storeBlockfrostFixture i query resp =
  let
    respHash = md5 resp
    filename = query <> "-" <> respHash <> ".json"
    fp = concat [ "fixtures", "test", "blockfrost", query, filename ]
  in
    writeTextFile UTF8 fp resp
      *> log ("Successfully saved fixture #" <> show i <> " to: " <> fp)
