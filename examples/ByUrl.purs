module Ctl.Examples.ByUrl (main) where

import Prelude

import Contract.Config
  ( ConfigParams
  , testnetFlintConfig
  , testnetGeroConfig
  , testnetLodeConfig
  , testnetNamiConfig
  )
import Contract.Monad (Contract, runContract)
import Contract.Prelude (fst, traverse_, uncurry)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Wallet.Cip30Mock
  ( WalletMock(MockFlint, MockGero, MockNami)
  , withCip30Mock
  )
import Contract.Wallet.Key (privateKeysToKeyWallet)
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromString
  , privateStakeKeyFromString
  )
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.AlwaysMints as AlwaysMints
import Ctl.Examples.AlwaysSucceeds as AlwaysSucceeds
import Ctl.Examples.Datums as Datums
import Ctl.Examples.MintsMultipleTokens as MintsMultipleTokens
import Ctl.Examples.OneShotMinting as OneShotMinting
import Ctl.Examples.Pkh2Pkh as Pkh2Pkh
import Ctl.Examples.PlutusV2.AlwaysSucceeds as AlwaysSucceedsV2
import Ctl.Examples.PlutusV2.OneShotMinting as OneShotMintingV2
import Ctl.Examples.SendsToken as SendsToken
import Ctl.Examples.SignMultiple as SignMultiple
import Ctl.Examples.Wallet as Wallet
import Data.Array (last)
import Data.Foldable (lookup)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.Common (split)
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error)

foreign import _queryString :: Effect String

foreign import _writeExampleHTML :: String -> Array String -> Effect Unit

main :: Effect Unit
main = do
  traverse_ (uncurry _writeExampleHTML) $ map ((_ /\ map fst wallets) <<< fst)
    examples
  queryString <- last <<< split (Pattern "?") <$> _queryString
  case split (Pattern ":") <$> queryString of
    Just [ exampleName, walletName ] -> do
      exampleContract <-
        liftMaybe (error $ "unknown example name: " <> exampleName) $
          lookup exampleName examples
      config <- liftMaybe (error $ "unknown wallet name: " <> walletName) $
        lookup walletName wallets
      launchAff_ do
        paymentKey <- liftMaybe (error "Unable to load private key") $
          privatePaymentKeyFromString paymentKeyStr
        let
          mbStakeKey = privateStakeKeyFromString stakeKeyStr
          mbWalletMock = case walletName of
            "nami-mock" -> Just MockNami
            "gero-mock" -> Just MockGero
            "flint-mock" -> Just MockFlint
            _ -> Nothing
        case mbWalletMock of
          Just walletMock -> do
            runContract config { walletSpec = Nothing }
              $ withCip30Mock (privateKeysToKeyWallet paymentKey mbStakeKey)
                  walletMock
                  exampleContract
            publishTestFeedback true
          Nothing -> do
            runContract config exampleContract
            publishTestFeedback true
    _ -> liftEffect $ Console.error "Error parsing query string"

wallets :: Array (String /\ ConfigParams ())
wallets =
  [ "nami" /\ testnetNamiConfig
  , "gero" /\ testnetGeroConfig
  , "flint" /\ testnetFlintConfig
  , "lode" /\ testnetLodeConfig
  , "nami-mock" /\ testnetNamiConfig
  , "gero-mock" /\ testnetGeroConfig
  , "flint-mock" /\ testnetFlintConfig
  , "lode-mock" /\ testnetLodeConfig
  ]

examples :: Array (String /\ Contract () Unit)
examples =
  [ "AlwaysMints" /\ AlwaysMints.contract
  , "AlwaysSucceeds" /\ AlwaysSucceeds.contract
  , "AlwaysSucceedsV2" /\ AlwaysSucceedsV2.contract
  , "Datums" /\ Datums.contract
  , "Wallet" /\ Wallet.contract
  , "Pkh2Pkh" /\ Pkh2Pkh.contract
  , "SendsToken" /\ SendsToken.contract
  , "SignMultiple" /\ SignMultiple.contract
  , "MintsMultipleTokens" /\ MintsMultipleTokens.contract
  , "OneShotMinting" /\ OneShotMinting.contract
  , "OneShotMintingV2" /\ OneShotMintingV2.contract
  ]

-- Address is:
-- addr_test1qz4fcrdru8scms07fw5ztljq84jhmfv9eqf04rhx79kd66k264fzzl5qr7nr7rsz0mupy7wqgs8tg44tgx605mlmf4dss0agmx
-- Fund it when needed

paymentKeyStr :: String
paymentKeyStr =
  """
    {
      "type": "PaymentSigningKeyShelley_ed25519",
      "description": "Payment Signing Key",
      "cborHex": "58200b07c066ba037344acee5431e6df41f6034bf1c5ffd6f803751e356807c6a209"
    }
  """

stakeKeyStr :: String
stakeKeyStr =
  """
    {
      "type": "StakeSigningKeyShelley_ed25519",
      "description": "Stake Signing Key",
      "cborHex": "5820f0db841df6c7fbc4506c58fad6676db0354a02dfd26efca445715a8adeabc338"
    }
  """
