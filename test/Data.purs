-- | Tests for `ToData`/`FromData`
module Test.Data (suite) where

import Prelude

import ConstrIndex (class HasConstrIndex, defaultConstrIndex, fromConstr2Index)
import Contract.PlutusData (PlutusData(..))
import Contract.Prelude (fromJust, traverse_, uncurry)
import Control.Lazy (fix)
import Data.Array (zip, (..))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Generic.Rep as G
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import FromData (class FromData, fromData, genericFromData)
import Mote (group, skip, test)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, genericArbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import ToData (class ToData, genericToData, toData)
import Deserialization.FromBytes (fromBytes)
import Deserialization.PlutusData as PDD
import Serialization (toBytes)
import Serialization.PlutusData as PDS
import Types.ByteArray (hexToByteArrayUnsafe)
import Untagged.Union (asOneOf)

suite :: TestPlanM Unit
suite = do
  group "PlutusData representation tests: ToData/FromData" $ do
    group "Primitives" do
      test "Unit" do
        let
          input = unit
        fromData (toData input) `shouldEqual` Just input
      group "Boolean" do
        let
          inputs = [ true, false ]
        for_ inputs \input -> do
          test (show input) do
            fromData (toData input) `shouldEqual` Just input
      group "Maybe" do
        let
          inputs = [ Just true, Just false, Nothing ]
        for_ inputs \input -> do
          test (show input) do
            fromData (toData input) `shouldEqual` Just input
      group "BigInt" do
        let
          inputs =
            [ BigInt.fromInt 0
            , BigInt.fromInt 10000
            , BigInt.fromInt (-10000)
            ]
        for_ inputs \input -> do
          test (show input) do
            fromData (toData input) `shouldEqual` Just input
      group "Map" do
        test "Map #1" do
          let
            input =
              Map.fromFoldable [ unit /\ unit ]
          fromData (toData input) `shouldEqual` Just input
        test "Map #2" do
          let
            input = Map.fromFoldable
              [ Map.fromFoldable [ unit /\ unit ] /\ Map.fromFoldable
                  [ unit /\ unit ]
              ]
          fromData (toData input) `shouldEqual` Just input
    group "Generic" do
      -- TODO: Quickcheckify
      test "EType: from . to == id" do
        let
          input = E0 (D1 (D2 (C1 Nothing))) true
            (C2 (MyBigInt (BigInt.fromInt 123)) false)
        fromData (toData input) `shouldEqual` Just input
      test "CType: C1 constructor shouldn't accept empty arguments" do
        let
          pd = Constr (BigInt.fromInt 1) []
        fromData pd `shouldEqual` (Nothing :: Maybe CType)
      test "CType: C1 constructor shouldn't accept more than one argument" do
        let
          pd = Constr (BigInt.fromInt 1)
            [ (Constr (BigInt.fromInt 1) []), (Integer $ BigInt.fromInt 0) ]
        fromData pd `shouldEqual` (Nothing :: Maybe CType)
      test "CType: C0 constructor shouldn't accept any arguments" do
        let
          pd = Constr (BigInt.fromInt 0) [ (Constr (BigInt.fromInt 1) []) ]
        fromData pd `shouldEqual` (Nothing :: Maybe CType)
      test "FType and FType' toData/fromData the same: F0 == F0'" do
        let
          f0 = F0 { f0A: BigInt.fromInt 1337 }
          f0' = F0' (BigInt.fromInt 1337)
        fromData (toData f0) `shouldEqual` Just f0'
        fromData (toData f0') `shouldEqual` Just f0
      test "FType and FType' toData/fromData the same: F1 == F1'" do
        let
          f1 = F1 { f1A: true, f1B: false, f1C: true }
          f1' = F1' true false true
        fromData (toData f1) `shouldEqual` Just f1'
        fromData (toData f1') `shouldEqual` Just f1
      test "FType and FType' toData/fromData the same: F2 == F2'" do
        let
          f2 = F2
            { f2A: BigInt.fromInt 1337
            , f2B: F1 { f1A: true, f1B: false, f1C: true }
            }
          f2' = F2' (BigInt.fromInt 1337) (F1' true false true)
        fromData (toData f2) `shouldEqual` Just f2'
        fromData (toData f2') `shouldEqual` Just f2
  group "ToData and serialization - binary fixtures" do
    -- How to get binary fixtures:
    --
    -- ```
    -- import PlutusTx
    -- import PlutusTx.IsData
    -- import Codec.Serialise
    -- import Data.ByteString.Lazy as BL
    --
    -- showBinary = toHexDigest . BL.toStrict . serialise . toData
    -- ```
    test "Unit" do
      let
        binaryFixture = "d87980"
        expectedOutput = unit
      fromBytesFromData binaryFixture `shouldEqual` Just expectedOutput
    group "Boolean" do
      let
        fixtures =
          [ false /\ "d87980"
          , true /\ "d87a80"
          ]
      for_ fixtures \(value /\ binaryFixture) -> do
        test (show value) do
          fromBytesFromData binaryFixture `shouldEqual` Just value
    group "Maybe" do
      let
        fixtures =
          [ Nothing /\ "d87a80"
          , Just (BigInt.fromInt 1) /\ "d8799f01ff"
          ]
      for_ fixtures \(value /\ binaryFixture) -> do
        test (show value) do
          fromBytesFromData binaryFixture `shouldEqual` Just value
    group "BigInt" do
      traverse_ (uncurry testBinaryFixture)
        [ BigInt.fromInt (negate 1000) /\ "3903e7"
        , BigInt.fromInt 1000 /\ "1903e8"
        , BigInt.fromInt 1 /\ "01"
        , unsafePartial
            ( fromJust $ BigInt.fromString
                "999999999999999999999999999999999999999"
            )
            /\ "c25102f050fe938943acc45f65567fffffffff"
        ]
    skip $ group "BigInt - failing (problem with encoding of 0)" do
      traverse_ (uncurry testBinaryFixture)
        [ BigInt.fromInt 0 /\ "00"
        ]

-- | Newtype wrapper to avoid an orphan instance
newtype MyBigInt = MyBigInt BigInt

derive newtype instance ToData MyBigInt
derive newtype instance FromData MyBigInt
derive newtype instance Eq MyBigInt
derive newtype instance Show MyBigInt

instance Arbitrary MyBigInt where
  arbitrary = do
    i <- arbitrary :: Gen Int
    let bi = BigInt.fromInt i
    pure $ MyBigInt bi

-- | Types used to test generic fromData and toData
data CType
  = C0
  | C1 (Maybe MyBigInt)
  | C2 MyBigInt Boolean
  | C3 MyBigInt Boolean Boolean

data DType = D0 CType MyBigInt (Maybe Boolean) | D1 DType | D2 CType
data EType = E0 DType Boolean CType
data FType
  = F0
      { f0A :: BigInt
      }
  | F1
      { f1A :: Boolean
      , f1B :: Boolean
      , f1C :: Boolean
      }
  | F2
      { f2A :: BigInt
      , f2B :: FType
      }

data FType'
  = F0' BigInt
  | F1' Boolean Boolean Boolean
  | F2' BigInt FType'

derive instance G.Generic CType _
derive instance G.Generic DType _
derive instance G.Generic EType _
derive instance G.Generic FType _
derive instance G.Generic FType' _

derive instance Eq CType
derive instance Eq DType
derive instance Eq EType
derive instance Eq FType
derive instance Eq FType'

instance HasConstrIndex CType where
  constrIndex = defaultConstrIndex

instance HasConstrIndex DType where
  constrIndex = defaultConstrIndex

instance HasConstrIndex EType where
  constrIndex = defaultConstrIndex

instance HasConstrIndex FType where
  constrIndex = defaultConstrIndex

instance HasConstrIndex FType' where
  constrIndex = defaultConstrIndex

instance FromData DType where
  fromData x = genericFromData x

instance ToData DType where
  toData pd = genericToData pd

instance FromData EType where
  fromData = genericFromData

instance ToData EType where
  toData = genericToData

instance FromData CType where
  fromData pd = genericFromData pd -- NOTE: https://github.com/purescript/documentation/blob/master/errors/CycleInDeclaration.md

instance ToData CType where
  toData x = genericToData x -- NOTE: https://github.com/purescript/documentation/blob/master/errors/CycleInDeclaration.md

instance FromData FType where
  fromData pd = genericFromData pd

instance FromData FType' where
  fromData pd = genericFromData pd

instance ToData FType where
  toData x = genericToData x

instance ToData FType' where
  toData x = genericToData x

-- https://stackoverflow.com/questions/51215083/how-to-quickcheck-on-custom-adt-in-purescript
-- https://github.com/purescript/purescript/issues/2975
-- https://github.com/purescript/documentation/blob/master/errors/CycleInDeclaration.md
instance Arbitrary CType where
  arbitrary = fix $ \arb -> arb

instance Arbitrary EType where
  arbitrary = genericArbitrary

instance Arbitrary DType where
  arbitrary = fix $ \arb -> arb

instance Show CType where
  show x = genericShow x

instance Show DType where
  show x = genericShow x

instance Show EType where
  show = genericShow

instance Show FType where
  show x = genericShow x

instance Show FType' where
  show x = genericShow x

data Day = Mon | Tue | Wed | Thurs | Fri | Sat | Sun

derive instance G.Generic Day _

instance HasConstrIndex Day where
  constrIndex _ = fromConstr2Index
    (zip [ "Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun" ] (0 .. 7))

instance ToData Day where
  toData = genericToData

instance FromData Day where
  fromData = genericFromData

data AnotherDay = AMon | ATue | AWed | AThurs | AFri | ASat | ASun

derive instance G.Generic AnotherDay _

instance HasConstrIndex AnotherDay where
  constrIndex = defaultConstrIndex

instance ToData AnotherDay where
  toData = genericToData

instance FromData AnotherDay where
  fromData = genericFromData

data Tree a = Node a (Tuple (Tree a) (Tree a)) | Leaf a

derive instance G.Generic (Tree a) _

instance HasConstrIndex (Tree a) where
  constrIndex = defaultConstrIndex

instance (ToData a) => ToData (Tree a) where
  toData x = genericToData x -- https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md#avoiding-stack-overflow-errors-with-recursive-types

instance (FromData a) => FromData (Tree a) where
  fromData x = genericFromData x

fromBytesFromData :: forall a. FromData a => String -> Maybe a
fromBytesFromData binary = fromData =<< PDD.convertPlutusData =<< fromBytes
  (hexToByteArrayUnsafe binary)

testBinaryFixture
  :: forall a
   . Eq a
  => Show a
  => FromData a
  => ToData a
  => a
  -> String
  -> TestPlanM Unit
testBinaryFixture value binaryFixture = do
  test ("Deserialization: " <> show value) do
    fromBytesFromData binaryFixture `shouldEqual` Just value
  test ("Serialization: " <> show value) do
    map (toBytes <<< asOneOf) (PDS.convertPlutusData (toData value))
      `shouldEqual` Just
        (hexToByteArrayUnsafe binaryFixture)
