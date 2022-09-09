module Examples.ByUrl (main) where

import Prelude

import Contract.Config
  ( ConfigParams
  , testnetEternlConfig
  , testnetGeroConfig
  , testnetNamiConfig
  , testnetFlintConfig
  , testnetLodeConfig
  )
import Contract.Prelude (fst, traverse_, uncurry)
import Contract.Monad (Contract, runContract)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromString
  , privateStakeKeyFromString
  )
import Control.Monad.Error.Class (liftMaybe)
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
import Examples.AlwaysMints as AlwaysMints
import Examples.AlwaysSucceeds as AlwaysSucceeds
import Examples.Datums as Datums
import Examples.MintsMultipleTokens as MintsMultipleTokens
import Examples.Pkh2Pkh as Pkh2Pkh
import Examples.SendsToken as SendsToken
import Examples.SignMultiple as SignMultiple
import Examples.Wallet as Wallet
import Wallet.Cip30Mock
  ( WalletMock(MockFlint, MockGero, MockNami)
  , withCip30Mock
  )
import Wallet.Key (privateKeysToKeyWallet)

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
  , "eternl" /\ testnetEternlConfig
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
  , "Datums" /\ Datums.contract
  , "Wallet" /\ Wallet.contract
  , "Pkh2Pkh" /\ Pkh2Pkh.contract
  , "SendsToken" /\ SendsToken.contract
  , "SignMultiple" /\ SignMultiple.contract
  , "MintsMultipleTokens" /\ MintsMultipleTokens.contract
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
