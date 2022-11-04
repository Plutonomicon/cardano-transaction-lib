module Test.Ctl.TxOutput (suite, main) where

import Prelude

import Aeson (decodeJsonString, printJsonDecodeError)
import Control.Monad.Error.Class (liftEither, liftMaybe, throwError)
import Ctl.Internal.Cardano.Types.Transaction (TransactionOutput)
import Ctl.Internal.QueryM.Ogmios as O
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Ctl.Internal.TxOutput (ogmiosTxOutToTransactionOutput)
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  )
import Data.Bifunctor (bimap)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Mote (group, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.Path (concat)

-- Run with `spago test --main Test.Ctl.TxOutput`
main :: Effect Unit
main = launchAff_ do
  interpret suite

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "TxOutput ogmiosTxOutToTransactionOutput datums are correctly preserved"
    $ test "Fixture #1"
    $ liftEffect
    $ loadQueryResultFixture >>= traverseWithIndex_ \input output -> do
        check <- liftMaybe (error "Missing fixture check")
          $ Map.lookup input fixtureChecks
        output' <- liftMaybe (error "Failed to convert output")
          $ ogmiosTxOutToTransactionOutput output
        check output'

loadQueryResultFixture
  :: Effect O.UtxoQueryResult
loadQueryResultFixture = do
  contents <- readTextFile UTF8 path
  liftEither $ bimap
    (error <<< printJsonDecodeError)
    unwrap
    (decodeJsonString contents :: _ O.UtxoQR)
  where
  path :: String
  path = concat
    [ "fixtures"
    , "test"
    , "ogmios"
    , "utxo-681f7f01fe06ae75d83187cda28c376e.json"
    ]

expect :: TransactionOutput -> String -> String -> Effect Unit
expect to expected got = throwError $ error $
  "Expected "
    <> expected
    <> " but got "
    <> got
    <> ": "
    <> show to

hasInlineDatum :: TransactionOutput -> Effect Unit
hasInlineDatum to = to # unwrap >>> _.datum >>> case _ of
  OutputDatum _ -> pure unit
  OutputDatumHash _ -> expect to "inline datum" "datum"
  NoOutputDatum -> expect to "inline datum" "no datum"

hasDatum :: TransactionOutput -> Effect Unit
hasDatum to = to # unwrap >>> _.datum >>> case _ of
  OutputDatumHash _ -> pure unit
  NoOutputDatum -> expect to "datum" "no datum"
  OutputDatum _ -> expect to "datum" "inline datum"

hasNoDatum :: TransactionOutput -> Effect Unit
hasNoDatum to = to # unwrap >>> _.datum >>> case _ of
  NoOutputDatum -> pure unit
  OutputDatum _ -> expect to "no datum" "inline datum"
  OutputDatumHash _ -> expect to "no datum" "datum"

fixtureChecks :: Map.Map O.OgmiosTxOutRef (TransactionOutput -> Effect Unit)
fixtureChecks = Map.fromFoldable
  [ { "txId": "2208e439244a1d0ef238352e3693098aba9de9dd0154f9056551636c8ed15dc1"
    , "index": UInt.fromInt 6
    } /\ hasDatum
  , { "txId": "4f539156bfbefc070a3b61cad3d1cedab3050e2b2a62f0ffe16a43eb0edc1ce8"
    , "index": UInt.fromInt 2
    } /\ hasDatum
  , { "txId": "e88bd757ad5b9bedf372d8d3f0cf6c962a469db61a265f6418e1ffed86da29ec"
    , "index": UInt.fromInt 4
    } /\ hasDatum
  , { "txId": "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25"
    , "index": UInt.fromInt 7
    } /\ hasInlineDatum
  , { "txId": "e88bd757ad5b9bedf372d8d3f0cf6c962a469db61a265f6418e1ffed86da29ec"
    , "index": UInt.fromInt 5
    } /\ hasInlineDatum
  , { "txId": "0268be9dbd0446eaa217e1dec8f399249305e551d7fc1437dd84521f74aa621c"
    , "index": UInt.fromInt 6
    } /\ hasInlineDatum
  , { "txId": "bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"
    , "index": UInt.fromInt 5
    } /\ hasNoDatum
  , { "txId": "bfa726c3c149165b108e6ff550cb1a1c4f0fdc2e9f26a9a16f48babe73b600ce"
    , "index": UInt.fromInt 4
    } /\ hasNoDatum
  , { "txId": "0268be9dbd0446eaa217e1dec8f399249305e551d7fc1437dd84521f74aa621c"
    , "index": UInt.fromInt 7
    } /\ hasNoDatum
  ]
