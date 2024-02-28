module Ctl.Examples.ByUrl (main) where

import Prelude

import Contract.Config
  ( ContractParams
  , WalletSpec
      ( ConnectToNami
      , ConnectToGero
      , ConnectToLode
      , ConnectToEternl
      , ConnectToFlint
      , ConnectToNuFi
      , ConnectToLace
      )
  , blockfrostPublicPreprodServerConfig
  , blockfrostPublicPreviewServerConfig
  , mainnetFlintConfig
  , mainnetGeroConfig
  , mainnetLodeConfig
  , mainnetNamiConfig
  , mainnetNuFiConfig
  , mkBlockfrostBackendParams
  , testnetConfig
  , testnetEternlConfig
  , testnetFlintConfig
  , testnetGeroConfig
  , testnetLaceConfig
  , testnetLodeConfig
  , testnetNamiConfig
  , testnetNuFiConfig
  )
import Contract.Monad (Contract)
import Contract.Test.E2E (E2EConfigName, E2ETestName, addLinks, route)
import Ctl.Examples.AdditionalUtxos as AdditionalUtxos
import Ctl.Examples.AlwaysMints as AlwaysMints
import Ctl.Examples.AlwaysSucceeds as AlwaysSucceeds
import Ctl.Examples.ChangeGeneration as ChangeGeneration
import Ctl.Examples.Cip30 as Cip30
import Ctl.Examples.Datums as Datums
import Ctl.Examples.DropTokens as DropTokens
import Ctl.Examples.ECDSA as ECDSA
import Ctl.Examples.MintsMultipleTokens as MintsMultipleTokens
import Ctl.Examples.NativeScriptMints as NativeScriptMints
import Ctl.Examples.OneShotMinting as OneShotMinting
import Ctl.Examples.PaysWithDatum as PaysWithDatum
import Ctl.Examples.Pkh2Pkh as Pkh2Pkh
import Ctl.Examples.PlutusV2.AlwaysSucceeds as AlwaysSucceedsV2
import Ctl.Examples.PlutusV2.OneShotMinting as OneShotMintingV2
import Ctl.Examples.PlutusV2.ReferenceInputs as ReferenceInputsV2
import Ctl.Examples.PlutusV2.ReferenceInputsAndScripts as ReferenceInputsAndScriptsV2
import Ctl.Examples.PlutusV3.AlwaysSucceeds as AlwaysSucceedsV3
import Ctl.Examples.Schnorr as Schnorr
import Ctl.Examples.SendsToken as SendsToken
import Ctl.Examples.SignData as SignData
import Ctl.Examples.SignMultiple as SignMultiple
import Ctl.Examples.TxChaining as TxChaining
import Ctl.Examples.Utxos as Utxos
import Ctl.Examples.Wallet as Wallet
import Ctl.Internal.Wallet.Cip30Mock
  ( WalletMock(MockNami, MockGero, MockFlint, MockLode, MockNuFi)
  )
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console as Console
import Test.Ctl.ApplyArgs as ApplyArgs
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

main :: Effect Unit
main = do
  -- Read Blockfrost API key from the browser storage.
  -- To set it up, run `npm run e2e-browser` and follow the instructions.
  mbApiKey <- getBlockfrostApiKey
  let
    walletsWithBlockfrost =
      wallets `Map.union`
        if isNothing mbApiKey then Map.empty
        else
          Map.fromFoldable
            [ "blockfrost-nami-preview"
                /\ (mkBlockfrostPreviewConfig mbApiKey)
                  { walletSpec = Just ConnectToNami }
                /\ Nothing
            , "blockfrost-gero-preview"
                /\ (mkBlockfrostPreviewConfig mbApiKey)
                  { walletSpec = Just ConnectToGero }
                /\ Nothing
            , "blockfrost-eternl-preview"
                /\ (mkBlockfrostPreviewConfig mbApiKey)
                  { walletSpec = Just ConnectToEternl }
                /\ Nothing
            , "blockfrost-lode-preview"
                /\ (mkBlockfrostPreviewConfig mbApiKey)
                  { walletSpec = Just ConnectToLode }
                /\ Nothing
            , "blockfrost-flint-preview"
                /\ (mkBlockfrostPreviewConfig mbApiKey)
                  { walletSpec = Just ConnectToFlint }
                /\ Nothing
            , "blockfrost-nufi-preview"
                /\ (mkBlockfrostPreviewConfig mbApiKey)
                  { walletSpec = Just ConnectToNuFi }
                /\ Nothing
            , "blockfrost-lace-preview"
                /\ (mkBlockfrostPreviewConfig mbApiKey)
                  { walletSpec = Just ConnectToLace }
                /\ Nothing
            , "blockfrost-nami-preprod"
                /\ (mkBlockfrostPreprodConfig mbApiKey)
                  { walletSpec = Just ConnectToNami }
                /\ Nothing
            , "blockfrost-gero-preprod"
                /\ (mkBlockfrostPreprodConfig mbApiKey)
                  { walletSpec = Just ConnectToGero }
                /\ Nothing
            , "blockfrost-eternl-preprod"
                /\ (mkBlockfrostPreprodConfig mbApiKey)
                  { walletSpec = Just ConnectToEternl }
                /\ Nothing
            , "blockfrost-lode-preprod"
                /\ (mkBlockfrostPreprodConfig mbApiKey)
                  { walletSpec = Just ConnectToLode }
                /\ Nothing
            , "blockfrost-flint-preprod"
                /\ (mkBlockfrostPreprodConfig mbApiKey)
                  { walletSpec = Just ConnectToFlint }
                /\ Nothing
            , "blockfrost-nufi-preprod"
                /\ (mkBlockfrostPreprodConfig mbApiKey)
                  { walletSpec = Just ConnectToNuFi }
                /\ Nothing
            , "blockfrost-lace-preprod"
                /\ (mkBlockfrostPreprodConfig mbApiKey)
                  { walletSpec = Just ConnectToLace }
                /\ Nothing
            ]
  addLinks walletsWithBlockfrost examples
  route walletsWithBlockfrost examples

getBlockfrostApiKey :: Effect (Maybe String)
getBlockfrostApiKey = do
  storage <- localStorage =<< window
  res <- getItem "BLOCKFROST_API_KEY" storage
  when (isNothing res) do
    Console.log
      "Set BLOCKFROST_API_KEY LocalStorage key to use Blockfrost services."
    Console.log "Run this in the browser console:"
    Console.log "  localStorage.setItem('BLOCKFROST_API_KEY', 'your-key-here');"
  pure res

wallets :: Map E2EConfigName (ContractParams /\ Maybe WalletMock)
wallets = Map.fromFoldable
  [ "nami" /\ testnetNamiConfig /\ Nothing
  , "gero" /\ testnetGeroConfig /\ Nothing
  , "flint" /\ testnetFlintConfig /\ Nothing
  , "eternl" /\ testnetEternlConfig /\ Nothing
  , "lode" /\ testnetLodeConfig /\ Nothing
  , "nufi" /\ testnetNuFiConfig /\ Nothing
  , "lace" /\ testnetLaceConfig /\ Nothing
  , "nami-mock" /\ testnetNamiConfig /\ Just MockNami
  , "gero-mock" /\ testnetGeroConfig /\ Just MockGero
  , "flint-mock" /\ testnetFlintConfig /\ Just MockFlint
  , "lode-mock" /\ testnetLodeConfig /\ Just MockLode
  -- Plutip cluster's network ID is set to mainnet:
  , "plutip-nami-mock" /\ mainnetNamiConfig /\ Just MockNami
  , "plutip-gero-mock" /\ mainnetGeroConfig /\ Just MockGero
  , "plutip-flint-mock" /\ mainnetFlintConfig /\ Just MockFlint
  , "plutip-lode-mock" /\ mainnetLodeConfig /\ Just MockLode
  , "plutip-nufi-mock" /\ mainnetNuFiConfig /\ Just MockNuFi
  ]

mkBlockfrostPreviewConfig :: Maybe String -> ContractParams
mkBlockfrostPreviewConfig apiKey =
  testnetConfig
    { backendParams = mkBlockfrostBackendParams
        { blockfrostConfig: blockfrostPublicPreviewServerConfig
        , blockfrostApiKey: apiKey
        , confirmTxDelay: Just (Seconds 30.0)
        }
    }

mkBlockfrostPreprodConfig :: Maybe String -> ContractParams
mkBlockfrostPreprodConfig apiKey =
  testnetConfig
    { backendParams = mkBlockfrostBackendParams
        { blockfrostConfig: blockfrostPublicPreprodServerConfig
        , blockfrostApiKey: apiKey
        , confirmTxDelay: Just (Seconds 30.0)
        }
    }

examples :: Map E2ETestName (Contract Unit)
examples = Map.fromFoldable
  [ "AdditionalUtxos" /\ AdditionalUtxos.contract false
  , "AlwaysMints" /\ AlwaysMints.contract
  , "NativeScriptMints" /\ NativeScriptMints.contract
  , "AlwaysSucceeds" /\ AlwaysSucceeds.contract
  , "AlwaysSucceedsV2" /\ AlwaysSucceedsV2.contract
  , "AlwaysSucceedsV3" /\ AlwaysSucceedsV3.contract
  , "Datums" /\ Datums.contract
  , "Wallet" /\ Wallet.contract
  , "Pkh2Pkh" /\ Pkh2Pkh.contract
  , "TxChaining" /\ TxChaining.contract
  , "SendsToken" /\ SendsToken.contract
  , "SignData" /\ SignData.contract
  , "SignMultiple" /\ SignMultiple.contract
  , "MintsMultipleTokens" /\ MintsMultipleTokens.contract
  , "OneShotMinting" /\ OneShotMinting.contract
  , "OneShotMintingV2" /\ OneShotMintingV2.contract
  , "Cip30" /\ Cip30.contract
  , "ReferenceInputs" /\ ReferenceInputsV2.contract
  , "ReferenceInputsAndScripts" /\ ReferenceInputsAndScriptsV2.contract
  , "Utxos" /\ Utxos.contract
  , "ApplyArgs" /\ ApplyArgs.contract
  , "Schnorr" /\ Schnorr.contract
  , "ECDSA" /\ ECDSA.contract
  , "PaysWithDatum" /\ PaysWithDatum.contract
  , "DropTokens" /\ DropTokens.contract
  , "ChangeGeneration1-1" /\
      ChangeGeneration.checkChangeOutputsDistribution 1 1 3
  , "ChangeGeneration3-1" /\
      ChangeGeneration.checkChangeOutputsDistribution 3 1 5
  , "ChangeGeneration1-3" /\
      ChangeGeneration.checkChangeOutputsDistribution 1 3 7
  ]
