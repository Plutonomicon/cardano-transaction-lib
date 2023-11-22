module Test.Ctl.Blockfrost (main, testPlan) where

import Prelude

import Contract.Config (blockfrostPublicPreviewServerConfig)
import Contract.Metadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  , TransactionMetadatum(Text, MetadataMap)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  )
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Transaction
  ( DataHash
  , GetTxMetadataError
      ( GetTxMetadataTxNotFoundError
      , GetTxMetadataMetadataEmptyOrMissingError
      )
  , TransactionHash(TransactionHash)
  )
import Control.Monad.Error.Class (liftEither)
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.Helpers (liftedM)
import Ctl.Internal.Serialization.Hash (ScriptHash, scriptHashFromBytes)
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostServiceM
  , runBlockfrostServiceM
  )
import Ctl.Internal.Service.Blockfrost as Blockfrost
import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), fromRight, isRight)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class.Console (log)
import JS.BigInt as BigInt
import Mote (group, test)
import Node.Process (argv)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Runner (defaultConfig)

-- Run with `spago test --main Test.Ctl.Blockfrost --exec-args PREVIEW_API_KEY`
main :: Effect Unit
main = do
  apiKey <- liftedM (error "ApiKey not supplied") $ (_ !! 1) <$> argv
  launchAff_ do
    interpretWithConfig
      defaultConfig { exit = true }
      ( testPlan
          { blockfrostConfig: blockfrostPublicPreviewServerConfig
          , blockfrostApiKey: Just apiKey
          , confirmTxDelay: Nothing
          }
      )

testPlan :: BlockfrostBackend -> TestPlanM (Aff Unit) Unit
testPlan backend = group "Blockfrost" do
  let
    mkDatumHash :: String -> DataHash
    mkDatumHash = wrap <<< hexToByteArrayUnsafe

    mkStringHash :: String -> ScriptHash
    mkStringHash s = unsafePartial $ fromJust $ scriptHashFromBytes $
      hexToByteArrayUnsafe s

  test "getDatumByHash - not found" do
    runBlockfrost do
      datum <- Blockfrost.getDatumByHash $ mkDatumHash
        "e1457a0c47dfb7a2f6b8fbb059bdceab163c05d34f195b87b9f2b30e"
      datum `shouldSatisfy` isRight
      let result = fromRight Nothing datum
      result `shouldEqual` Nothing

  test "getScriptByHash - not found" do
    runBlockfrost do
      script <- Blockfrost.getScriptByHash $ mkStringHash
        "e1457a0c47dfb7a2f6b8fbb059bdceab163c05d34f195b87b9f2b30e"
      script `shouldSatisfy` isRight
      let result = fromRight Nothing script
      result `shouldEqual` Nothing

  forWithIndex_ [ fixture1, fixture2, fixture3, fixture4 ] \i fixture ->
    group ("fixture " <> show (i + 1)) do
      test "getTxMetadata" do
        eMetadata <- runBlockfrost $ Blockfrost.getTxMetadata $
          fixtureHash fixture
        eMetadata `shouldEqual` case fixture of
          TxWithMetadata { metadata } -> Right metadata
          TxWithNoMetadata _ -> Left GetTxMetadataMetadataEmptyOrMissingError
          UnconfirmedTx _ -> Left GetTxMetadataTxNotFoundError
      test "isTxConfirmed" do
        eExists <- runBlockfrost $ Blockfrost.doesTxExist $
          fixtureHash fixture
        exists <- liftEither $ lmap (error <<< show) eExists
        exists `shouldEqual` case fixture of
          TxWithMetadata _ -> true
          TxWithNoMetadata _ -> true
          UnconfirmedTx _ -> false
  where
  runBlockfrost :: forall (a :: Type). BlockfrostServiceM a -> Aff a
  runBlockfrost = runBlockfrostServiceM (prettyFormatter >=> log) backend

data Fixture
  = TxWithMetadata
      { hash :: TransactionHash
      , metadata :: GeneralTransactionMetadata
      }
  | TxWithNoMetadata
      { hash :: TransactionHash }
  | UnconfirmedTx
      { hash :: TransactionHash }

fixtureHash :: Fixture -> TransactionHash
fixtureHash = case _ of
  TxWithMetadata { hash } -> hash
  TxWithNoMetadata { hash } -> hash
  UnconfirmedTx { hash } -> hash

fixture1 :: Fixture
fixture1 = TxWithMetadata
  { hash: TransactionHash $ hexToByteArrayUnsafe
      "7a2aff2b7f92f6f8ec3fb2135301c7bfc36fea1489a3ca37fd6066f3155c46ff"
  , metadata:
      GeneralTransactionMetadata $ Map.fromFoldable $
        [ 30 /\ "5"
        , 50 /\
            "d8799f581c11185d006a64f24e9cf6da987589cd0b166718d3680e63985db370"
        , 51 /\
            "3d9fd8799fd8799fd8799f581c70e60f3b5ea7153e0acc7a803e4401d44b8ed1"
        , 52 /\
            "bae1c7baaad1a62a72ffd8799fd8799fd8799f581c1e78aae7c90cc36d624f7b"
        , 53 /\
            "3bb6d86b52696dc84e490f343eba89005fffffffffa140d8799f00a1401a000f"
        , 54 /\
            "32a0ffffd8799fd8799fd8799f581c11185d006a64f24e9cf6da987589cd0b16"
        , 55 /\
            "6718d3680e63985db3703dffd8799fd8799fd8799f581cac7d463e54c43d994b"
        , 56 /\
            "66d1512d5fb315c7563ae43c37610cd07977c7ffffffffa1581c4e5205e5df36"
        , 57 /\
            "8030e9f814c2258c01f50c62375c55b07fbcc076c181d8799f01a0ffffffff,d"
        , 58 /\
            "8799f581c11185d006a64f24e9cf6da987589cd0b166718d3680e63985db3703"
        , 59 /\
            "d9fd8799fd8799fd8799f581c70e60f3b5ea7153e0acc7a803e4401d44b8ed1b"
        , 60 /\
            "ae1c7baaad1a62a72ffd8799fd8799fd8799f581c1e78aae7c90cc36d624f7b3"
        , 61 /\
            "bb6d86b52696dc84e490f343eba89005fffffffffa140d8799f00a1401a000f3"
        , 62 /\
            "2a0ffffd8799fd8799fd8799f581c11185d006a64f24e9cf6da987589cd0b166"
        , 63 /\
            "718d3680e63985db3703dffd8799fd8799fd8799f581cac7d463e54c43d994b6"
        , 64 /\
            "6d1512d5fb315c7563ae43c37610cd07977c7ffffffffa1581c4e5205e5df368"
        , 65 /\
            "030e9f814c2258c01f50c62375c55b07fbcc076c181d8799f01a0ffffffff,d8"
        , 66 /\
            "799f581c11185d006a64f24e9cf6da987589cd0b166718d3680e63985db3703d"
        , 67 /\
            "9fd8799fd8799fd8799f581c70e60f3b5ea7153e0acc7a803e4401d44b8ed1ba"
        , 68 /\
            "e1c7baaad1a62a72ffd8799fd8799fd8799f581c1e78aae7c90cc36d624f7b3b"
        , 69 /\
            "b6d86b52696dc84e490f343eba89005fffffffffa140d8799f00a1401a000f32"
        , 70 /\
            "a0ffffd8799fd8799fd8799f581c11185d006a64f24e9cf6da987589cd0b1667"
        , 71 /\
            "18d3680e63985db3703dffd8799fd8799fd8799f581cac7d463e54c43d994b66"
        , 72 /\
            "d1512d5fb315c7563ae43c37610cd07977c7ffffffffa1581c4e5205e5df3680"
        , 73 /\
            "30e9f814c2258c01f50c62375c55b07fbcc076c181d8799f01a0ffffffff,"
        , 75 /\
            "4e5205e5df368030e9f814c2258c01f50c62375c55b07fbcc076c181::00"
        , 76 /\
            "4e5205e5df368030e9f814c2258c01f50c62375c55b07fbcc076c181::01"
        , 77 /\
            "4e5205e5df368030e9f814c2258c01f50c62375c55b07fbcc076c181::02"
        ] <#> \(label /\ text) ->
          TransactionMetadatumLabel (BigInt.fromInt label) /\ Text text
  }

fixture2 :: Fixture
fixture2 = TxWithMetadata
  { hash: TransactionHash $ hexToByteArrayUnsafe
      "d499729695be63b4c6affb2412899a7f16390d54d97f78f51d796a5cef424126"
  , metadata:
      GeneralTransactionMetadata $ Map.fromFoldable $
        [ 674 /\
            [ "City" /\ "Mumbai"
            , "Humidity" /\ "69.19999694824219 %"
            , "Sensor" /\ "DTH22 with Raspberry Pi"
            , "Temperature" /\ "27.299999237060547 C"
            , "Timestamp" /\ "1672173001"
            ]
        ] <#> \(label /\ metamap) ->
          TransactionMetadatumLabel
            (BigInt.fromInt label) /\ MetadataMap
            (Map.fromFoldable $ metamap <#> \(k /\ v) -> Text k /\ Text v)
  }

fixture3 :: Fixture
fixture3 = TxWithNoMetadata
  { hash: TransactionHash $ hexToByteArrayUnsafe
      "7b458500ef7783e16dab5d9f9f282505182c316ccf3ecf75d0472f95ab31eeaa"
  }

fixture4 :: Fixture
fixture4 = UnconfirmedTx
  { hash: TransactionHash $ hexToByteArrayUnsafe
      "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
  }
