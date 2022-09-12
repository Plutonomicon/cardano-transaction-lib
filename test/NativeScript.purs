module Test.NativeScript (suite) where

import Prelude

import Cardano.Types.NativeScript
  ( NativeScript(ScriptPubkey, ScriptAll, ScriptAny, ScriptNOfK)
  )
import Data.Maybe (fromJust)
import Data.Set as Set
import Effect.Aff (Aff)
import Mote (group, test)
import NativeScripts (getMaximumSigners)
import Partial.Unsafe (unsafePartial)
import Serialization.Hash
  ( Ed25519KeyHash
  , ed25519KeyHashFromBech32
  , ed25519KeyHashFromBytes
  )
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Types.RawBytes (hexToRawBytesUnsafe)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "NativeScript number of keys" do
    test "#1" do
      let
        script = ScriptAll [ ScriptPubkey pk1, ScriptPubkey pk2 ]
      getMaximumSigners Set.empty script `shouldEqual` 2
    test "#2" do
      let
        script = ScriptAll [ ScriptPubkey pk1, ScriptPubkey pk1 ]
      getMaximumSigners Set.empty script `shouldEqual` 1
    test "#3" do
      let
        script = ScriptAll
          [ ScriptPubkey pk1
          , ScriptPubkey pk1
          , ScriptPubkey pk1
          , ScriptPubkey pk1
          ]
      getMaximumSigners Set.empty script `shouldEqual` 1
    test "#4" do
      let
        script = ScriptAll
          [ ScriptPubkey pk1
          , ScriptPubkey pk1
          , ScriptPubkey pk1
          , ScriptPubkey pk2
          ]
      getMaximumSigners Set.empty script `shouldEqual` 2
    test "#5" do
      let
        script = ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
      getMaximumSigners Set.empty script `shouldEqual` 1
    test "#6" do
      let
        script = ScriptAll
          [ ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          , ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          ]
      getMaximumSigners Set.empty script `shouldEqual` 2
    test "#7" do
      let
        script = ScriptNOfK 1 [ ScriptPubkey pk1, ScriptPubkey pk2 ]
      getMaximumSigners Set.empty script `shouldEqual` 1
    test "#8" do
      let
        script = ScriptNOfK 2 [ ScriptPubkey pk1, ScriptPubkey pk2 ]
      getMaximumSigners Set.empty script `shouldEqual` 2
    test "#9" do
      let
        script = ScriptAny
          [ ScriptAll [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          , ScriptAll [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          ]
      getMaximumSigners Set.empty script `shouldEqual` 2
    test "#10" do
      let
        script = ScriptAny
          [ ScriptAll [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          , ScriptAll [ ScriptPubkey pk2, ScriptPubkey pk3 ]
          ]
      getMaximumSigners Set.empty script `shouldEqual` 2
    test "#11" do
      let
        script = ScriptNOfK 2
          [ ScriptAll [ ScriptPubkey pk1 ]
          , ScriptAll [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          , ScriptAll [ ScriptPubkey pk1, ScriptPubkey pk2, ScriptPubkey pk3 ]
          ]
      getMaximumSigners Set.empty script `shouldEqual` 3
    test "#12" do
      let
        script = ScriptNOfK 2
          [ ScriptAll [ ScriptPubkey pk3 ]
          , ScriptAll [ ScriptPubkey pk1 ]
          , ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk3 ]
          ]
      getMaximumSigners Set.empty script `shouldEqual` 2
    test "#13" do
      let
        script = ScriptAny
          [ ScriptAny [ ScriptPubkey pk3, ScriptPubkey pk1 ]
          , ScriptAny [ ScriptPubkey pk3, ScriptPubkey pk2 ]
          , ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          ]
      getMaximumSigners Set.empty script `shouldEqual` 1
    test "#14" do
      let
        script = ScriptAll
          [ ScriptAny [ ScriptPubkey pk3, ScriptPubkey pk1 ]
          , ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          , ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          ]
      getMaximumSigners Set.empty script `shouldEqual` 3
    test "#15" do
      let
        script = ScriptNOfK 2
          [ ScriptAny [ ScriptPubkey pk3, ScriptPubkey pk1 ]
          , ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          , ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          ]
      getMaximumSigners Set.empty script `shouldEqual` 2
    test "#16" do
      let
        script = ScriptNOfK 2
          [ ScriptAny [ ScriptPubkey pk3, ScriptPubkey pk1 ]
          , ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          , ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          ]
      getMaximumSigners Set.empty script `shouldEqual` 2
    test "#17" do
      let
        script = ScriptNOfK 3
          [ ScriptAny [ ScriptPubkey pk3, ScriptPubkey pk1 ]
          , ScriptAny [ ScriptPubkey pk2, ScriptPubkey pk3 ]
          , ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          ]
      getMaximumSigners Set.empty script `shouldEqual` 3
    test "#18" do
      let
        script = ScriptNOfK 3
          [ ScriptAny [ ScriptPubkey pk3, ScriptPubkey pk1 ]
          , ScriptAny [ ScriptPubkey pk2, ScriptPubkey pk3 ]
          , ScriptAny [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          ]
      getMaximumSigners (Set.fromFoldable [ pk1 ]) script `shouldEqual` 2
    test "#19" do
      let
        script = ScriptAll
          [ ScriptPubkey pk1, ScriptPubkey pk2, ScriptPubkey pk3 ]
      getMaximumSigners (Set.fromFoldable [ pk1, pk2, pk3 ]) script
        `shouldEqual` 0
    test "#20" do
      let
        script = ScriptAll
          [ ScriptNOfK 1 [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          , ScriptNOfK 1 [ ScriptPubkey pk3, ScriptPubkey pk4 ]
          ]
      getMaximumSigners Set.empty script
        `shouldEqual` 2
    test "#21" do
      let
        script = ScriptAll
          [ ScriptNOfK 1 [ ScriptPubkey pk1, ScriptPubkey pk2 ]
          , ScriptNOfK 1 [ ScriptPubkey pk3, ScriptPubkey pk1 ]
          ]
      getMaximumSigners Set.empty script
        `shouldEqual` 2
    test "#22" do
      let
        script = ScriptAll
          [ ScriptNOfK 2
              [ ScriptPubkey pk1, ScriptPubkey pk2, ScriptPubkey pk3 ]
          , ScriptNOfK 1 [ ScriptPubkey pk2, ScriptPubkey pk1 ]
          ]
      getMaximumSigners Set.empty script
        `shouldEqual` 3
    test "#23" do
      let
        script = ScriptAll
          [ ScriptNOfK 2
              [ ScriptPubkey pk1, ScriptPubkey pk2, ScriptPubkey pk3 ]
          , ScriptNOfK 2
              [ ScriptPubkey pk1, ScriptPubkey pk2, ScriptPubkey pk3 ]
          ]
      getMaximumSigners Set.empty script
        `shouldEqual` 3
    test "#24" do
      let
        -- worst case all 4 will be distinct
        script = ScriptNOfK 2
          [ ScriptNOfK 2
              [ ScriptPubkey pk1, ScriptPubkey pk4, ScriptPubkey pk2 ]
          , ScriptNOfK 2
              [ ScriptPubkey pk3, ScriptPubkey pk4, ScriptPubkey pk5 ]
          , ScriptNOfK 2
              [ ScriptPubkey pk1, ScriptPubkey pk2, ScriptPubkey pk3 ]
          ]
      getMaximumSigners Set.empty script
        `shouldEqual` 4
    test "#25" do
      let
        script = ScriptNOfK 2
          [ ScriptNOfK 2
              [ ScriptPubkey pk1, ScriptPubkey pk2, ScriptPubkey pk3 ]
          , ScriptNOfK 2
              [ ScriptPubkey pk2, ScriptPubkey pk3, ScriptPubkey pk1 ]
          , ScriptNOfK 2
              [ ScriptPubkey pk3, ScriptPubkey pk2, ScriptPubkey pk1 ]
          ]
      getMaximumSigners Set.empty script
        `shouldEqual` 3

pk1 :: Ed25519KeyHash
pk1 = unsafePartial $ fromJust $ ed25519KeyHashFromBytes $ hexToRawBytesUnsafe
  "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"

pk2 :: Ed25519KeyHash
pk2 = unsafePartial $ fromJust $ ed25519KeyHashFromBytes $ hexToRawBytesUnsafe
  "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"

pk3 :: Ed25519KeyHash
pk3 = unsafePartial $ fromJust do
  ed25519KeyHashFromBech32
    "addr_vkh1zuctrdcq6ctd29242w8g84nlz0q38t2lnv3zzfcrfqktx0c9tzp"

pk4 :: Ed25519KeyHash
pk4 = unsafePartial $ fromJust $ ed25519KeyHashFromBytes $ hexToRawBytesUnsafe
  "30fb3b8529951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"

pk5 :: Ed25519KeyHash
pk5 = unsafePartial $ fromJust $ ed25519KeyHashFromBytes $ hexToRawBytesUnsafe
  "30fb3b8529951e26f034919a5a37f22cb99d94d1d409f69ddbaea971"
