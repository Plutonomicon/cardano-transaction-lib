module Ctl.Examples.ByUrl (main) where

import Prelude

import Contract.Config
  ( ConfigParams
  , mainnetFlintConfig
  , mainnetGeroConfig
  , mainnetLodeConfig
  , mainnetNamiConfig
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
import Ctl.Examples.Cip30 as Cip30
import Ctl.Examples.Datums as Datums
import Ctl.Examples.MintsMultipleTokens as MintsMultipleTokens
import Ctl.Examples.NativeScriptMints as NativeScriptMints
import Ctl.Examples.OneShotMinting as OneShotMinting
import Ctl.Examples.Pkh2Pkh as Pkh2Pkh
import Ctl.Examples.PlutusV2.AlwaysSucceeds as AlwaysSucceedsV2
import Ctl.Examples.PlutusV2.OneShotMinting as OneShotMintingV2
import Ctl.Examples.PlutusV2.ReferenceInputs as ReferenceInputsV2
import Ctl.Examples.PlutusV2.ReferenceInputsAndScripts as ReferenceInputsAndScriptsV2
import Ctl.Examples.SendsToken as SendsToken
import Ctl.Examples.SignMultiple as SignMultiple
import Ctl.Examples.TxChaining as TxChaining
import Ctl.Examples.Utxos as Utxos
import Ctl.Examples.Wallet as Wallet
import Ctl.Internal.Wallet.Cip30Mock
  ( WalletMock(MockNami, MockGero, MockFlint, MockLode)
  )
import Test.Ctl.ApplyArgs as ApplyArgs
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
  -- Plutip cluster's network ID is set to mainnet:
  , "plutip-nami-mock" /\ mainnetNamiConfig /\ Just MockNami
  , "plutip-gero-mock" /\ mainnetGeroConfig /\ Just MockGero
  , "plutip-flint-mock" /\ mainnetFlintConfig /\ Just MockFlint
  , "plutip-lode-mock" /\ mainnetLodeConfig /\ Just MockLode
  ]

examples :: Map E2ETestName (Contract () Unit)
examples = Map.fromFoldable
  [ "AlwaysMints" /\ AlwaysMints.contract
  , "NativeScriptMints" /\ NativeScriptMints.contract
  , "AlwaysSucceeds" /\ AlwaysSucceeds.contract
  , "AlwaysSucceedsV2" /\ AlwaysSucceedsV2.contract
  , "Datums" /\ Datums.contract
  , "Wallet" /\ Wallet.contract
  , "Pkh2Pkh" /\ Pkh2Pkh.contract
  , "TxChaining" /\ TxChaining.contract
  , "SendsToken" /\ SendsToken.contract
  , "SignMultiple" /\ SignMultiple.contract
  , "MintsMultipleTokens" /\ MintsMultipleTokens.contract
  , "OneShotMinting" /\ OneShotMinting.contract
  , "OneShotMintingV2" /\ OneShotMintingV2.contract
  , "Cip30" /\ Cip30.contract
  , "ReferenceInputs" /\ ReferenceInputsV2.contract
  , "ReferenceInputsAndScripts" /\ ReferenceInputsAndScriptsV2.contract
  , "Utxos" /\ Utxos.contract
  , "ApplyArgs" /\ ApplyArgs.contract
  ]
