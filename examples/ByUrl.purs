module Ctl.Examples.ByUrl (main) where

import Prelude

import Contract.Config
  ( ConfigParams
  , testnetEternlConfig
  , testnetFlintConfig
  , testnetGeroConfig
  , testnetLodeConfig
  , testnetNamiConfig
  )
import Contract.Monad (Contract)
import Contract.Test.E2E (E2EConfigName, E2ETestName, addLinks, route)
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
import Ctl.Internal.Wallet.Cip30Mock
  ( WalletMock(MockNami, MockGero, MockFlint, MockLode)
  )
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)

main :: Effect Unit
main = do
  addLinks wallets examples
  route wallets examples

wallets :: Map E2EConfigName (ConfigParams () /\ Maybe WalletMock)
wallets = Map.fromFoldable
  [ "nami" /\ testnetNamiConfig /\ Nothing
  , "gero" /\ testnetGeroConfig /\ Nothing
  , "flint" /\ testnetFlintConfig /\ Nothing
  , "eternl" /\ testnetEternlConfig /\ Nothing
  , "lode" /\ testnetLodeConfig /\ Nothing
  , "nami-mock" /\ testnetNamiConfig /\ Just MockNami
  , "gero-mock" /\ testnetGeroConfig /\ Just MockGero
  , "flint-mock" /\ testnetFlintConfig /\ Just MockFlint
  , "lode-mock" /\ testnetLodeConfig /\ Just MockLode
  ]

examples :: Map E2ETestName (Contract () Unit)
examples = Map.fromFoldable
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
