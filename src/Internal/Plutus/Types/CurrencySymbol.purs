module Ctl.Internal.Plutus.Types.CurrencySymbol
  ( CurrencySymbol(CurrencySymbol)
  , adaSymbol
  , currencyMPSHash
  , getCurrencySymbol
  , mkCurrencySymbol
  , mpsSymbol
  , scriptHashAsCurrencySymbol
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getField
  )
import Control.Monad.Gen as Gen
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata)
import Ctl.Internal.Serialization.Hash
  ( ScriptHash
  , scriptHashFromBytes
  , scriptHashToBytes
  )
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Ctl.Internal.Types.Scripts (MintingPolicyHash(MintingPolicyHash))
import Data.Array.NonEmpty (fromArray)
import Data.Either (Either(Left))
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype CurrencySymbol = CurrencySymbol ByteArray

derive newtype instance Eq CurrencySymbol
derive newtype instance Ord CurrencySymbol
derive newtype instance FromData CurrencySymbol
derive newtype instance FromMetadata CurrencySymbol
derive newtype instance ToData CurrencySymbol
derive newtype instance ToMetadata CurrencySymbol

instance DecodeAeson CurrencySymbol where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    (flip getField "unCurrencySymbol" >=> decodeAeson >>> map CurrencySymbol)

instance EncodeAeson CurrencySymbol where
  encodeAeson (CurrencySymbol mph) = encodeAeson { "unCurrencySymbol": mph }

instance Show CurrencySymbol where
  show (CurrencySymbol cs) = "(CurrencySymbol " <> show cs <> ")"

instance Arbitrary CurrencySymbol where
  arbitrary = Gen.elements $ map translate $ unsafeNonEmpty
    [ "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65"
    , "92c4f22371bd453aec9fe19ccebfbc88211ae854b5eab424bcd4c26d"
    , "c9e9c15d4f16a7948d3736c93aa79034621d51dccc4df5d31c7d34aa"
    ]
    where
    unsafeNonEmpty x = unsafePartial $ fromJust $ fromArray x

    translate :: String -> CurrencySymbol
    translate x =
      scriptHashAsCurrencySymbol $ unsafePartial $ fromJust
        $ scriptHashFromBytes
        $ hexToByteArrayUnsafe x

adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol mempty

scriptHashAsCurrencySymbol :: ScriptHash -> CurrencySymbol
scriptHashAsCurrencySymbol = CurrencySymbol <<< unwrap <<< scriptHashToBytes

-- | The minting policy hash of a currency symbol.
currencyMPSHash :: CurrencySymbol -> MintingPolicyHash
currencyMPSHash = MintingPolicyHash <<< currencyScriptHash

-- | The currency symbol of a monetary policy hash.
mpsSymbol :: MintingPolicyHash -> Maybe CurrencySymbol
mpsSymbol (MintingPolicyHash h) = mkCurrencySymbol $ unwrap $ scriptHashToBytes
  h

getCurrencySymbol :: CurrencySymbol -> ByteArray
getCurrencySymbol (CurrencySymbol curSymbol) = curSymbol

mkCurrencySymbol :: ByteArray -> Maybe CurrencySymbol
mkCurrencySymbol byteArr
  | byteArr == mempty =
      pure adaSymbol
  | otherwise =
      scriptHashFromBytes byteArr $> CurrencySymbol byteArr

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

-- This must be safe to use as long as we always construct a
-- `CurrencySymbol` with the smart-constructors.
currencyScriptHash :: CurrencySymbol -> ScriptHash
currencyScriptHash = unsafePartial $ fromJust <<< scriptHashFromBytes
  <<< getCurrencySymbol
