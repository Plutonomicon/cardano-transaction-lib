module Plutus.Types.CurrencySymbol
  ( CurrencySymbol
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
  , encodeAeson'
  , getField
  )
import Data.Either (Either(Left))
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap, wrap)
import FromData (class FromData)
import Partial.Unsafe (unsafePartial)
import Serialization.Hash (ScriptHash, scriptHashFromBytes, scriptHashToBytes)
import ToData (class ToData)
import Types.ByteArray (ByteArray)
import Types.Scripts (MintingPolicyHash(MintingPolicyHash))
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata)

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
  encodeAeson' (CurrencySymbol mph) = encodeAeson' { "unCurrencySymbol": mph }

instance Show CurrencySymbol where
  show (CurrencySymbol cs) = "(CurrencySymbol " <> show cs <> ")"

adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol mempty

scriptHashAsCurrencySymbol :: ScriptHash -> CurrencySymbol
scriptHashAsCurrencySymbol = CurrencySymbol <<< unwrap <<< scriptHashToBytes

-- | The minting policy hash of a currency symbol.
currencyMPSHash :: CurrencySymbol -> MintingPolicyHash
currencyMPSHash = MintingPolicyHash <<< currencyScriptHash

-- | The currency symbol of a monetary policy hash.
mpsSymbol :: MintingPolicyHash -> Maybe CurrencySymbol
mpsSymbol (MintingPolicyHash h) = mkCurrencySymbol <<< unwrap $
  scriptHashToBytes h

getCurrencySymbol :: CurrencySymbol -> ByteArray
getCurrencySymbol (CurrencySymbol curSymbol) = curSymbol

mkCurrencySymbol :: ByteArray -> Maybe CurrencySymbol
mkCurrencySymbol byteArr
  | byteArr == mempty =
      pure adaSymbol
  | otherwise =
      scriptHashFromBytes (wrap byteArr) $> CurrencySymbol byteArr

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

-- This must be safe to use as long as we always construct a
-- `CurrencySymbol` with the smart-constructors.
currencyScriptHash :: CurrencySymbol -> ScriptHash
currencyScriptHash = unsafePartial $ fromJust <<< scriptHashFromBytes <<< wrap
  <<< getCurrencySymbol
