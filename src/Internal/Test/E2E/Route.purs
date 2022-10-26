module Ctl.Internal.E2E.Route
  ( E2ETestName
  , E2EConfigName
  , route
  , parseRoute
  , Route
  , addLinks
  ) where

import Prelude

import Contract.Config (ConfigParams)
import Contract.Monad (Contract, runContract)
import Contract.Test.Cip30Mock (WalletMock, withCip30Mock)
import Contract.Wallet
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeyFromBytes
  )
import Contract.Wallet.Key (privateKeysToKeyWallet)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Helpers (liftEither)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Test.E2E.Feedback.Hooks (e2eFeedbackHooks)
import Ctl.Internal.Types.ByteArray (hexToByteArray)
import Ctl.Internal.Types.RawBytes (RawBytes(RawBytes))
import Ctl.Internal.Wallet.Spec (WalletSpec(ConnectToEternl))
import Data.Array (last)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(Left), note)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Data.String (stripPrefix)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Exception (error)

-- | A name of some particular test. Used in the URL
type E2ETestName = String

-- | A name of some particular E2E test environment (`ConfigParams` and possible
-- | CIP-30 mock). Used in the URL for routing
type E2EConfigName = String

-- | Router state - parsed from URL by `parseRoute`
type Route =
  { configName :: E2EConfigName
  , testName :: E2ETestName
  , paymentKey :: Maybe PrivatePaymentKey
  , stakeKey :: Maybe PrivateStakeKey
  }

parseRoute :: String -> Either String Route
parseRoute queryString =
  case split (Pattern ":") queryString of
    [ configName, testName ] -> do
      pure { configName, testName, paymentKey: Nothing, stakeKey: Nothing }
    [ configName, testName, paymentKeyStr ] -> do
      paymentKey <- mkPaymentKey paymentKeyStr
      pure
        { configName
        , testName
        , paymentKey: Just paymentKey
        , stakeKey: Nothing
        }
    [ configName, testName, paymentKeyStr, stakeKeyStr ] -> do
      paymentKey <- mkPaymentKey paymentKeyStr
      stakeKey <- mkStakeKey stakeKeyStr
      pure
        { configName
        , testName
        , paymentKey: Just paymentKey
        , stakeKey: Just stakeKey
        }
    _ -> Left "Unable to decode URL"
  where
  mkPrivateKey' :: String -> Maybe PrivateKey
  mkPrivateKey' str = hexToByteArray str >>= RawBytes >>> privateKeyFromBytes

  mkPrivateKey :: String -> Maybe PrivateKey
  mkPrivateKey str =
    mkPrivateKey' str <|> (stripPrefix (Pattern "5820") str >>= mkPrivateKey)

  mkPaymentKey :: String -> Either String PrivatePaymentKey
  mkPaymentKey str = note ("Unable to decode a Payment Key from " <> show str) $
    mkPrivateKey str <#> PrivatePaymentKey

  mkStakeKey :: String -> Either String PrivateStakeKey
  mkStakeKey str = note ("Unable to decode a Stake Key from " <> show str) do
    mkPrivateKey str <#> PrivateStakeKey

addLinks
  :: Map E2EConfigName (ConfigParams () /\ Maybe WalletMock)
  -> Map E2ETestName (Contract () Unit)
  -> Effect Unit
addLinks configMaps testMaps = do
  let
    configs = Array.fromFoldable $ Map.keys configMaps
    tests = Array.fromFoldable $ Map.keys testMaps
  _addLinks configs tests

-- | Serves given examples conditionally, depending on the URL query part.
-- |
-- | The structure of the query string should be one of:
-- | ```
-- | E2EConfigName:E2ETestName
-- | E2EConfigName:E2ETestName:PrivatePaymentKeyCborHex
-- | E2EConfigName:E2ETestName:PrivatePaymentKeyCborHex:PrivateStakeKeyCborHex
-- | ```
-- |
-- | Examples:
-- | ```
-- | gero:MintsMultiple
-- | eternl:AlwaysSucceeds:58200b07c066ba037344acee5431e6df41f6034bf1c5ffd6f803751e356807c6a209
-- | nami-mock:MintsMultiple:58200b07c066ba037344acee5431e6df41f6034bf1c5ffd6f803751e356807c6a209:5820f0db841df6c7fbc4506c58fad6676db0354a02dfd26efca445715a8adeabc338
-- | ```
route
  :: Map E2EConfigName (ConfigParams () /\ Maybe WalletMock)
  -> Map E2ETestName (Contract () Unit)
  -> Effect Unit
route configs tests = do
  queryString <- fold <<< last <<< split (Pattern "?") <$> _queryString
  { configName, testName, paymentKey: mbPaymentKey, stakeKey } <-
    liftEither $ lmap error $ parseRoute queryString
  config /\ mbMock <-
    liftMaybe
      (error $ "Unable to look up the config parameters: " <> configName) $
      Map.lookup configName configs
  test <-
    liftMaybe (error $ "Unable to look up the `Contract` to run: " <> testName)
      $ Map.lookup testName tests
  launchAff_ do
    delayIfEternl config
    case mbMock of
      Nothing ->
        runContract config { hooks = e2eFeedbackHooks } test
      Just mock -> do
        -- Payment key is required for CIP-30 mocking
        paymentKey <-
          liftMaybe (error "Payment key was not provided for CIP-30 mocking!")
            mbPaymentKey
        runContract config
          { walletSpec = Nothing, hooks = e2eFeedbackHooks }
          $ withCip30Mock (privateKeysToKeyWallet paymentKey stakeKey) mock test
  where
  -- | Eternl does not initialize instantly
  delayIfEternl config =
    case config.walletSpec of
      Just ConnectToEternl ->
        delay $ wrap 3000.0
      _ -> pure unit

foreign import _queryString :: Effect String

foreign import _writeExampleHTML :: String -> Array String -> Effect Unit

foreign import _addLinks
  :: Array E2EConfigName -> Array E2ETestName -> Effect Unit
