module Test.Ctl.Blockfrost where

import Prelude

import Contract.Metadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  , TransactionMetadatum(Text)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  )
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Contract.Transaction (TransactionHash(TransactionHash))
import Ctl.Internal.Service.Blockfrost (getTxMetadata, isTxConfirmed)
import Data.BigInt as BigInt
import Data.Either (Either(Right), hush)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff, cancelWith, effectCanceler, launchAff)
import Effect.Class.Console (log)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (defaultConfig)

-- Run with `spago test --main Test.Ctl.Blockfrost`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 450_000.0, exit = true }
      testPlan

fixture1 :: { hash :: TransactionHash, metadata :: GeneralTransactionMetadata }
fixture1 =
  { hash: TransactionHash $ hexToByteArrayUnsafe
      "7a2aff2b7f92f6f8ec3fb2135301c7bfc36fea1489a3ca37fd6066f3155c46ff"
  , metadata:
      GeneralTransactionMetadata $ Map.fromFoldable $
        ( \(label /\ text) ->
            TransactionMetadatumLabel
              (unsafePartial $ fromJust $ BigInt.fromString label) /\ Text text
        )
          <$>
            [ "30" /\ "5"
            , "50" /\
                "d8799f581c11185d006a64f24e9cf6da987589cd0b166718d3680e63985db370"
            , "51" /\
                "3d9fd8799fd8799fd8799f581c70e60f3b5ea7153e0acc7a803e4401d44b8ed1"
            , "52" /\
                "bae1c7baaad1a62a72ffd8799fd8799fd8799f581c1e78aae7c90cc36d624f7b"
            , "53" /\
                "3bb6d86b52696dc84e490f343eba89005fffffffffa140d8799f00a1401a000f"
            , "54" /\
                "32a0ffffd8799fd8799fd8799f581c11185d006a64f24e9cf6da987589cd0b16"
            , "55" /\
                "6718d3680e63985db3703dffd8799fd8799fd8799f581cac7d463e54c43d994b"
            , "56" /\
                "66d1512d5fb315c7563ae43c37610cd07977c7ffffffffa1581c4e5205e5df36"
            , "57" /\
                "8030e9f814c2258c01f50c62375c55b07fbcc076c181d8799f01a0ffffffff,d"
            , "58" /\
                "8799f581c11185d006a64f24e9cf6da987589cd0b166718d3680e63985db3703"
            , "59" /\
                "d9fd8799fd8799fd8799f581c70e60f3b5ea7153e0acc7a803e4401d44b8ed1b"
            , "60" /\
                "ae1c7baaad1a62a72ffd8799fd8799fd8799f581c1e78aae7c90cc36d624f7b3"
            , "61" /\
                "bb6d86b52696dc84e490f343eba89005fffffffffa140d8799f00a1401a000f3"
            , "62" /\
                "2a0ffffd8799fd8799fd8799f581c11185d006a64f24e9cf6da987589cd0b166"
            , "63" /\
                "718d3680e63985db3703dffd8799fd8799fd8799f581cac7d463e54c43d994b6"
            , "64" /\
                "6d1512d5fb315c7563ae43c37610cd07977c7ffffffffa1581c4e5205e5df368"
            , "65" /\
                "030e9f814c2258c01f50c62375c55b07fbcc076c181d8799f01a0ffffffff,d8"
            , "66" /\
                "799f581c11185d006a64f24e9cf6da987589cd0b166718d3680e63985db3703d"
            , "67" /\
                "9fd8799fd8799fd8799f581c70e60f3b5ea7153e0acc7a803e4401d44b8ed1ba"
            , "68" /\
                "e1c7baaad1a62a72ffd8799fd8799fd8799f581c1e78aae7c90cc36d624f7b3b"
            , "69" /\
                "b6d86b52696dc84e490f343eba89005fffffffffa140d8799f00a1401a000f32"
            , "70" /\
                "a0ffffd8799fd8799fd8799f581c11185d006a64f24e9cf6da987589cd0b1667"
            , "71" /\
                "18d3680e63985db3703dffd8799fd8799fd8799f581cac7d463e54c43d994b66"
            , "72" /\
                "d1512d5fb315c7563ae43c37610cd07977c7ffffffffa1581c4e5205e5df3680"
            , "73" /\
                "30e9f814c2258c01f50c62375c55b07fbcc076c181d8799f01a0ffffffff,"
            , "75" /\
                "4e5205e5df368030e9f814c2258c01f50c62375c55b07fbcc076c181::00"
            , "76" /\
                "4e5205e5df368030e9f814c2258c01f50c62375c55b07fbcc076c181::01"
            , "77" /\
                "4e5205e5df368030e9f814c2258c01f50c62375c55b07fbcc076c181::02"
            ]
  }

config =
  { host: "cardano-preview.blockfrost.io"
  , port: UInt.fromInt 443
  , secure: true
  , path: Just "/api/v0"
  }

apiKey = Just ?help

testPlan :: TestPlanM (Aff Unit) Unit
testPlan = group "Blockfrost" do
  test "getTxMetadata success" do
    metadata <- getTxMetadata fixture1.hash config apiKey
    (hush metadata) `shouldEqual` (Just (Just fixture1.metadata))
  test "isTxConfirmed success" do
    confirmed <- isTxConfirmed fixture1.hash config apiKey
    (hush confirmed) `shouldEqual` (Just true)
