-- | Tests for `ToData`/`FromData`
module Test.Data (suite, tests, uniqueIndicesTests) where

import Prelude

import Aeson (decodeAeson, encodeAeson)
import Contract.PlutusData (PlutusData(Constr, Integer))
import Contract.Prelude (Either(..))
import Control.Lazy (fix)
import Data.Argonaut (JsonDecodeError(..))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Generic.Rep as G
import Data.Maybe (maybe, Maybe(Just, Nothing), fromJust)
import Data.Show.Generic (genericShow)
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple, uncurry)
import Data.Tuple.Nested ((/\))
import Deserialization.FromBytes (fromBytes)
import Deserialization.PlutusData as PDD
import FromData (class FromData, fromData, genericFromData)
import Mote (group, skip, test)
import Partial.Unsafe (unsafePartial)
import Plutus.Types.AssocMap (Map(..))
import Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  )
import Serialization (toBytes)
import Serialization.PlutusData as PDS
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, genericArbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import ToData (class ToData, genericToData, toData)
import Type.RowList (Cons, Nil)
import TypeLevel.Nat (Z, S)
import TypeLevel.RowList (class AllUniqueLabels)
import TypeLevel.RowList.Unordered.Indexed (NilI, ConsI, class UniqueIndices)
import Types.ByteArray (hexToByteArrayUnsafe)
import Untagged.Union (asOneOf)
import Plutus.Types.AssocMap as AssocMap

plutusDataAesonRoundTrip
  ∷ ∀ (a ∷ Type). ToData a ⇒ FromData a ⇒ a → Either JsonDecodeError a
plutusDataAesonRoundTrip x = do
  pd <- encodeAeson (toData x) # decodeAeson
  maybe (Left $ TypeMismatch "") (pure) $ fromData pd

suite :: TestPlanM Unit
suite = do
  group "PlutusData Aeson representation tests" $ do
    group "Primitives" do
      test "Unit" do
        let
          input = unit
        plutusDataAesonRoundTrip input `shouldEqual` Right input
      group "Boolean" do
        let
          inputs = [ true, false ]
        for_ inputs \input -> do
          test (show input) do
            plutusDataAesonRoundTrip input `shouldEqual` Right input
      group "Maybe" do
        let
          inputs = [ Just true, Just false, Nothing ]
        for_ inputs \input -> do
          test (show input) do
            plutusDataAesonRoundTrip input `shouldEqual` Right input
      group "BigInt" do
        let
          inputs =
            [ BigInt.fromInt 0
            , BigInt.fromInt 10000
            , BigInt.fromInt (-10000)
            ]
        for_ inputs \input -> do
          test (show input) do
            plutusDataAesonRoundTrip input `shouldEqual` Right input
      test "Array" do
        let
          input = [ Just true, Just false, Nothing ]
        plutusDataAesonRoundTrip input `shouldEqual` Right input
      group "Map" do
        let
          inputs =
            [ Map
                [ BigInt.fromInt 1 /\
                    [ Map
                        [ BigInt.fromInt 11 /\ false
                        , BigInt.fromInt 12 /\ true
                        ]
                    ]
                , BigInt.fromInt 2 /\
                    [ Map
                        [ BigInt.fromInt 21 /\ false
                        , BigInt.fromInt 22 /\ false
                        , BigInt.fromInt 23 /\ true
                        ]
                    ]
                , BigInt.fromInt 3 /\ []
                ]
            ]
        for_ inputs \input -> do
          test (show input) do
            plutusDataAesonRoundTrip input `shouldEqual` Right input
    group "Generic" do
      -- TODO: Quickcheckify
      test "CType: from . to == id" do
        let
          input = C4
            ( Map
                [ BigInt.fromInt 13 /\ [ Map [ BigInt.fromInt 17 /\ false ] ]
                ]
            )
        plutusDataAesonRoundTrip input `shouldEqual` Right input
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
      test "Array" do
        let
          input = [ Just true, Just false, Nothing ]
        fromData (toData input) `shouldEqual` Just input
      test "AssocMap" do
        let
          input = AssocMap.Map
            [ BigInt.fromInt 1 /\
                [ ( AssocMap.Map
                      [ BigInt.fromInt 17 /\ false, BigInt.fromInt 13 /\ true ]
                  )
                ]
            , BigInt.fromInt 3 /\ []
            ]
        fromData (toData input) `shouldEqual` Just input
      test "AssocMap #2" do
        let
          input = AssocMap.Map
            [ BigInt.fromInt 1 /\ [ true, false, false ]
            , BigInt.fromInt 3 /\ [ false, true, true ]
            , BigInt.fromInt 7 /\ [ true, false, true ]
            ]
        fromData (toData input) `shouldEqual` Just input
      test "AssocMap #3" do
        let
          input =
            [ AssocMap.Map
                [ BigInt.fromInt 17 /\ false
                , BigInt.fromInt 13 /\ true
                ]
            ]
        fromData (toData input) `shouldEqual` Just input
      test "AssocMap #4" do
        let
          input = AssocMap.Map
            [ BigInt.fromInt 1 /\ (AssocMap.Map [ true /\ false ])
            , BigInt.fromInt 3 /\ (AssocMap.Map [ false /\ false ])
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
          f1 = F1 { f1A: false, f1B: false, f1C: true }
          f1' = F1' false false true
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
  | C4 (AssocMap.Map BigInt (Array (AssocMap.Map BigInt Boolean)))

instance
  HasPlutusSchema CType
    ( "C0" := PNil @@ Z
        :+ "C1"
        := PNil
        @@ (S Z)
        :+ "C2"
        := PNil
        @@ (S (S Z))
        :+ "C3"
        := PNil
        @@ (S (S (S Z)))
        :+ "C4"
        := PNil
        @@ (S (S (S (S Z))))
        :+ PNil
    )

data DType = D0 CType MyBigInt (Maybe Boolean) | D1 DType | D2 CType

instance
  HasPlutusSchema DType
    ( "D0" := PNil @@ Z
        :+ "D1"
        := PNil
        @@ (S Z)
        :+ "D2"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

data EType = E0 DType Boolean CType

instance HasPlutusSchema EType ("E0" := PNil @@ Z :+ PNil)

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

instance
  HasPlutusSchema FType
    ( "F0"
        :=
          ( "f0A" := I BigInt
              :+ PNil
          )
        @@ (S Z)

        :+ "F1"
        :=
          ( "f1A" := I Boolean
              :+ "f1B"
              := I Boolean
              :+ "f1C"
              := I Boolean
              :+ PNil
          )
        @@ Z

        :+ "F2"
        :=
          ( "f2A" := I BigInt
              :+ "f2B"
              := I FType
              :+ PNil
          )
        @@ (S (S Z))

        :+ PNil
    )

data FType'
  = F0' BigInt
  | F1' Boolean Boolean Boolean
  | F2' BigInt FType'

instance
  HasPlutusSchema FType'
    ( "F0'" := PNil @@ (S Z)
        :+ "F1'"
        := PNil
        @@ (Z)
        :+ "F2'"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

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

instance
  HasPlutusSchema Day
    ( "Mon" := PNil @@ Z
        :+ "Tue"
        := PNil
        @@ (S Z)
        :+ "Wed"
        := PNil
        @@ (S (S Z))
        :+ "Thurs"
        := PNil
        @@ (S (S (S Z)))
        :+ "Fri"
        := PNil
        @@ (S (S (S (S (S Z)))))
        :+ "Sat"
        := PNil
        @@ (S (S (S (S (S (S Z))))))
        :+ "Sun"
        := PNil
        @@ (S (S (S (S (S (S (S Z)))))))
        :+ PNil
    )

instance ToData Day where
  toData = genericToData

instance FromData Day where
  fromData = genericFromData

data AnotherDay = AMon | ATue | AWed | AThurs | AFri | ASat | ASun

derive instance G.Generic AnotherDay _

instance
  HasPlutusSchema AnotherDay
    ( "AMon" := PNil @@ Z
        :+ "ATue"
        := PNil
        @@ (S Z)
        :+ "AWed"
        := PNil
        @@ (S (S Z))
        :+ "AThurs"
        := PNil
        @@ (S (S (S Z)))
        :+ "AFri"
        := PNil
        @@ (S (S (S (S (S Z)))))
        :+ "ASat"
        := PNil
        @@ (S (S (S (S (S (S Z))))))
        :+ "ASun"
        := PNil
        @@ (S (S (S (S (S (S (S Z)))))))
        :+ PNil
    )

instance ToData AnotherDay where
  toData = genericToData

instance FromData AnotherDay where
  fromData = genericFromData

data Tree a = Node a (Tuple (Tree a) (Tree a)) | Leaf a

derive instance G.Generic (Tree a) _

instance
  HasPlutusSchema (Tree a)
    ( "Node" := PNil @@ Z
        :+ "Leaf"
        := PNil
        @@ (S Z)
        :+ PNil
    )

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

-- | Poor man's type level tests
tests ∷ Array String
tests =
  [ testNil
  , testSingleton
  , testUniques
  ]
  where
  testNil :: AllUniqueLabels Nil => String
  testNil = "Empty list has all unique labels"

  testSingleton
    :: forall (a :: Type). AllUniqueLabels (Cons "A" a Nil) => String
  testSingleton = "Singleton list has all unique labels"

  testUniques
    :: forall (a :: Type)
     . AllUniqueLabels
         ( Cons "A" a
             (Cons "B" a (Cons "C" a Nil))
         )
    => String
  testUniques = "[A, B, C] is all unique and should compile"

uniqueIndicesTests ∷ Array String
uniqueIndicesTests =
  [ testNil
  , testSingletonZ
  , testSingletonSSZ
  , testUniques
  ]
  where
  testNil :: UniqueIndices NilI => String
  testNil = "Empty list has all unique indices"

  testSingletonZ
    :: forall (a :: Type). UniqueIndices (ConsI "A" a Z NilI) => String
  testSingletonZ = "Singleton list has all unique indices"

  testSingletonSSZ
    :: forall (a :: Type). UniqueIndices (ConsI "A" a (S (S Z)) NilI) => String
  testSingletonSSZ = "Singleton list has all unique indices"

  testUniques
    :: forall (a :: Type)
     . UniqueIndices
         ( ConsI "A" a Z
             (ConsI "B" a (S Z) (ConsI "C" a (S (S Z)) NilI))
         )
    => String
  testUniques = "[0, 1, 2] have all unique indices"
