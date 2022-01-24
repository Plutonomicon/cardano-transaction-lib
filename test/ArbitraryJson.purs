module Test.ArbitraryJson where

import Data.Semiring ((+),(*))
import Data.Ring (negate, (-))
import Control.Alt ((<$>))
import Control.Alternative (pure)
import Control.Apply (lift2)
import Control.Bind (bind, (>>=))
import Control.Category ((>>>))
import Control.Lazy (fix)
import Control.Monad.Gen (oneOf)
import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Eq ((==))
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Array.NonEmpty (cons')
import Data.NonEmpty ((:|))
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements)


newtype ArbBigInt = ArbBigInt BigInt

instance Arbitrary ArbBigInt where
    arbitrary = do
      a <- BigInt.fromInt <$> chooseInt 1000000000 2000000000
      b <- BigInt.fromInt <$> arbitrary
      c <- BigInt.fromInt <$> elements (cons' (-1) [1])
      pure $ ArbBigInt $ c * (a * a * a * a + b)


data ArbJson
  = JNull
  | JBool    Boolean
  | JBigInt  ArbBigInt
  | JNum     Number
  | JStr     String
  | JList   (Array ArbJson)
  | JObject (Array (Tuple String ArbJson))

instance Arbitrary ArbJson where
  arbitrary = arbitraryRec 4
    where
      arbStr :: Gen String
      arbStr =
        let stringWithEscapes = fromCharArray <$> (arrayOf (oneOf (pure '\\' :| [arbitrary])))
        in oneOf ( stringWithEscapes :| [arbitrary])

      arbNum :: Gen Number
      arbNum = oneOf (arbitrary :| [elements selected])
        where
          selected = cons' 1.123123e200 [-2.12312342e200,1e-200,-2.2e-100]

      arbitraryRec :: Int -> Gen ArbJson
      arbitraryRec n = fix \_ -> let
        bool   =  JBool   <$> arbitrary
        int    =  JBigInt <$> arbitrary
        num    =  JNum    <$> arbNum
        str    =  JStr    <$> arbStr
        list   =  JList   <$> arrayOf (arbitraryRec (n-1))
        object =  JObject <$> (arrayOf $ lift2 Tuple arbStr (arbitraryRec (n-1)))
        in oneOf (pure JNull :| if n == 0
                    then [bool, int, num, str]
                    else [bool, int, num, str, list, object])


-- | Turn ArbJson into a JSON string.
stringifyArbJson :: ArbJson -> String
stringifyArbJson = case _ of
  JNull -> "null"
  JBool b -> Json.stringify (Json.fromBoolean b)
  JNum b -> Json.stringify (Json.fromNumber b)
  JStr b -> Json.stringify (Json.fromString b)
  JBigInt (ArbBigInt bi) -> BigInt.toString bi
  JList l -> "[" <> joinWith "," (stringifyArbJson <$> l) <> "]"
  JObject o -> "{" <> joinWith "," (stringifyKV <$> o) <> "}"
    where
      stringifyKV (Tuple k v) = Json.stringify (Json.fromString k) <> ": " <> stringifyArbJson v


-- | Turn ArbJson to Json if it does not contain BigInts
arbJsonToJson :: ArbJson -> Maybe Json
arbJsonToJson = case _ of
  JNull -> Just Json.jsonNull
  JBool b -> Just $ Json.fromBoolean b
  JBigInt _ -> Nothing
  JNum n -> Just $ Json.fromNumber n
  JStr s -> Just $ Json.fromString s
  JList a -> traverse arbJsonToJson a >>= Json.fromArray >>> pure
  JObject a -> traverse parseKV a >>= Object.fromFoldable >>> Json.fromObject >>> pure
    where
      parseKV :: Tuple String ArbJson -> Maybe (Tuple String Json)
      parseKV (Tuple k v) = Tuple k <$> arbJsonToJson v
