-- | Tests for `ToData`/`FromData`
module Test.Ctl.Data (suite, tests, uniqueIndicesTests) where

import Prelude hiding (conj)

import Aeson (JsonDecodeError(TypeMismatch), decodeAeson, encodeAeson)
import Control.Lazy (fix)
import Control.Monad.Error.Class (class MonadThrow)
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.PlutusData as PDD
import Ctl.Internal.FromData (class FromData, fromData, genericFromData)
import Ctl.Internal.Helpers (showWithParens)
import Ctl.Internal.Plutus.Types.AssocMap (Map(Map))
import Ctl.Internal.Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  )
import Ctl.Internal.Serialization (toBytes)
import Ctl.Internal.Serialization.PlutusData as PDS
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.ToData (class ToData, genericToData, toData)
import Ctl.Internal.TypeLevel.Nat (S, Z)
import Ctl.Internal.TypeLevel.RowList (class AllUniqueLabels)
import Ctl.Internal.TypeLevel.RowList.Unordered.Indexed
  ( class UniqueIndices
  , ConsI
  , NilI
  )
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.ByteArray (hexToByteArrayUnsafe)
import Ctl.Internal.Types.PlutusData (PlutusData(Constr, Integer))
import Data.Array.NonEmpty (fromNonEmpty) as NEArray
import Data.Either (Either(Left, Right))
import Data.Generic.Rep as G
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Data.Show.Generic (genericShow)
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception (Error)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, genericArbitrary)
import Test.QuickCheck.Combinators (conj)
import Test.QuickCheck.Gen (frequency)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(Proxy))
import Type.RowList (Cons, Nil)

plutusDataAesonRoundTrip
  :: forall (a :: Type). ToData a => FromData a => a -> Either JsonDecodeError a
plutusDataAesonRoundTrip x = do
  maybe (Left $ TypeMismatch "") pure <<< fromData =<<
    (encodeAeson (toData x) # decodeAeson)

shouldEqualWith
  :: forall (a :: Type) (b :: Type) (m1 :: Type -> Type) (m2 :: Type -> Type)
   . ToData a
  => FromData a
  => Show (m2 a)
  => Eq (m2 a)
  => MonadThrow Error m1
  => (b -> m2 a)
  -> (b -> m2 a)
  -> b
  -> m1 Unit
shouldEqualWith functionToTest wrap value =
  functionToTest value `shouldEqual` wrap value

plutusDataRoundtripProperty
  :: forall (a :: Type)
   . FromData a
  => ToData a
  => Arbitrary a
  => Eq a
  => Show a
  => Proxy a
  -> Aff Unit
plutusDataRoundtripProperty (_ :: Proxy a) =
  quickCheck \(input :: a) -> fromData (toData input) === Just input

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "PlutusData Aeson representation tests" $ do
    group "Primitives" do
      test "Unit" do
        shouldEqualWith plutusDataAesonRoundTrip Right unit
      group "Boolean" do
        let
          inputs = [ true, false ]
        for_ inputs \input -> do
          test (show input) do
            shouldEqualWith plutusDataAesonRoundTrip Right input
      group "Maybe" do
        let
          inputs = [ Just true, Just false, Nothing ]
        for_ inputs \input -> do
          test (show input) do
            shouldEqualWith plutusDataAesonRoundTrip Right input
      test "BigInt" $
        quickCheck \(n :: Int) ->
          let
            input = BigInt.fromInt n
          in
            plutusDataAesonRoundTrip input === Right input
      test "Array" $
        let
          input = [ Just true, Just false, Nothing ]
        in
          shouldEqualWith plutusDataAesonRoundTrip Right input
      test "Map" $
        let
          input = Map
            [ BigInt.fromInt 13 /\
                [ Map
                    [ BigInt.fromInt 17 /\ false
                    ]
                ]
            ]
        in
          shouldEqualWith plutusDataAesonRoundTrip Right input
    group "Generic" do
      test "CType: from . to == id" $
        let
          input = C4
            ( Map
                [ BigInt.fromInt 13 /\ [ Map [ BigInt.fromInt 17 /\ false ] ]
                ]
            )
        in
          plutusDataAesonRoundTrip input `shouldEqual` Right input
  group "PlutusData representation tests: ToData/FromData" $ do
    group "Primitives" do
      test "Unit" do
        shouldEqualWith (fromData <<< toData) Just unit
      group "Boolean" do
        let
          inputs = [ true, false ]
        for_ inputs \input -> do
          test (show input) do
            shouldEqualWith (fromData <<< toData) Just input
      group "Maybe" do
        let
          inputs = [ Just true, Just false, Nothing ]
        for_ inputs \input -> do
          test (show input) do
            shouldEqualWith (fromData <<< toData) Just input
      test "BigInt" $ plutusDataRoundtripProperty (Proxy :: Proxy MyBigInt)
      test "Array" $
        shouldEqualWith
          (fromData <<< toData)
          Just
          [ Just true, Just false, Nothing ]
      test "Map" $
        let
          input = Map
            [ BigInt.fromInt 13 /\
                [ Map
                    [ BigInt.fromInt 17 /\ false
                    ]
                ]
            ]
        in
          shouldEqualWith (fromData <<< toData) Just input
    group "Generic" do
      test "EType: from . to == id" $
        plutusDataRoundtripProperty (Proxy :: Proxy EType)
      test "CType: C1 constructor shouldn't accept empty arguments" $
        let
          pd = Constr BigNum.one []
        in
          shouldEqualWith fromData (const (Nothing :: Maybe CType)) pd
      test "CType: C1 constructor shouldn't accept more than one argument" $
        let
          pd = Constr BigNum.one
            [ (Constr BigNum.one []), (Integer $ BigInt.fromInt 0) ]
        in
          shouldEqualWith fromData (const (Nothing :: Maybe CType)) pd
      test "CType: C0 constructor shouldn't accept any arguments" $
        let
          pd = Constr BigNum.zero [ (Constr BigNum.one []) ]
        in
          shouldEqualWith fromData (const (Nothing :: Maybe CType)) pd
      test "FType and FType' toData/fromData are the same" $
        quickCheck \(input :: FType) -> do
          let
            input' = fType2Ftype' input
          conj (fromData (toData input) === Just input')
            $ fromData (toData input') === Just input
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

-- | Newtype wrapper to avoid an orphan instance
newtype MyBigInt = MyBigInt BigInt

derive newtype instance ToData MyBigInt
derive newtype instance FromData MyBigInt
derive newtype instance Eq MyBigInt

instance Show MyBigInt where
  show (MyBigInt bi) = showWithParens "MyBigInt" bi

instance Arbitrary MyBigInt where
  arbitrary = MyBigInt <<< BigInt.fromInt <$> arbitrary

-- | Types used to test generic fromData and toData
data CType
  = C0
  | C1 (Maybe MyBigInt)
  | C2 MyBigInt Boolean
  | C3 MyBigInt Boolean Boolean
  | C4 (Map BigInt (Array (Map BigInt Boolean)))

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

instance Arbitrary CType where
  arbitrary =
    (frequency <<< NEArray.fromNonEmpty) $
      (0.25 /\ pure C0)
        :|
          [ 0.25 /\ (C1 <$> arbitrary)
          , 0.25 /\ (C2 <$> arbitrary <*> arbitrary)
          , 0.25 /\ (C3 <$> arbitrary <*> arbitrary <*> arbitrary)
          ]

instance Arbitrary EType where
  arbitrary = genericArbitrary

instance Arbitrary DType where
  arbitrary = fix \_ -> frequency $ NEArray.fromNonEmpty $
    0.4 /\ (D0 <$> arbitrary <*> arbitrary <*> arbitrary)
      :|
        [ 0.4 /\ (D2 <$> arbitrary)
        , 0.2 /\ (D1 <$> arbitrary)
        ]

instance Arbitrary FType where
  arbitrary = fix \_ -> frequency $ NEArray.fromNonEmpty $
    0.4 /\ (F0 <$> ({ f0A: _ } <<< unwrap <$> arbitrary))
      :|
        [ 0.4 /\
            ( F1 <$>
                ( { f1A: _, f1B: _, f1C: _ } <$> arbitrary <*> arbitrary <*>
                    arbitrary
                )
            )
        , 0.2 /\
            (F2 <$> ({ f2A: _, f2B: _ } <<< unwrap <$> arbitrary <*> arbitrary))
        ]
    where
    unwrap :: MyBigInt -> BigInt
    unwrap (MyBigInt x) = x

fType2Ftype' :: FType -> FType'
fType2Ftype' (F0 { f0A: i }) = F0' i
fType2Ftype' (F1 { f1A: b1, f1B: b2, f1C: b3 }) = F1' b1 b2 b3
fType2Ftype' (F2 { f2A: i, f2B: f }) = F2' i $ fType2Ftype' f

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

instance Functor Tree where
  map f (Leaf a) = Leaf (f a)
  map f (Node a (ltree /\ rtree)) = Node (f a) (map f ltree /\ map f rtree)

instance
  HasPlutusSchema (Tree a)
    ( "Node" := PNil @@ Z
        :+ "Leaf"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance (ToData a) => ToData (Tree a) where
  -- https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md#avoiding-stack-overflow-errors-with-recursive-types
  toData t = genericToData $ map toData t

instance (FromData a) => FromData (Tree a) where
  fromData pd = worker =<< (genericFromData pd :: _ (Tree PlutusData))
    where
    worker :: Tree PlutusData -> Maybe (Tree a)
    worker = case _ of
      Leaf a -> Leaf <$> fromData a
      Node a (ltree /\ rtree) ->
        Node <$> fromData a <*> (Tuple <$> worker ltree <*> worker rtree)

fromBytesFromData :: forall a. FromData a => String -> Maybe a
fromBytesFromData binary = fromData <<< PDD.convertPlutusData =<< fromBytes
  (wrap $ hexToByteArrayUnsafe binary)

testBinaryFixture
  :: forall a
   . Eq a
  => Show a
  => FromData a
  => ToData a
  => a
  -> String
  -> TestPlanM (Aff Unit) Unit
testBinaryFixture value binaryFixture = do
  test ("Deserialization: " <> show value) do
    fromBytesFromData binaryFixture `shouldEqual` Just value
  test ("Serialization: " <> show value) do
    toBytes (PDS.convertPlutusData $ toData value)
      `shouldEqual` wrap (hexToByteArrayUnsafe binaryFixture)

-- | Poor man's type level tests
tests :: Array String
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

uniqueIndicesTests :: Array String
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
