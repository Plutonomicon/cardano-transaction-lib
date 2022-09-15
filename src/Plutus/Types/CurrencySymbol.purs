module CTL.Plutus.Types.CurrencySymbol
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
import CTL.Internal.FromData (class FromData)
import CTL.Internal.Metadata.FromMetadata (class FromMetadata)
import CTL.Internal.Metadata.ToMetadata (class ToMetadata)
import CTL.Internal.Serialization.Hash
  ( ScriptHash
  , scriptHashFromBytes
  , scriptHashToBytes
  )
import CTL.Internal.ToData (class ToData)
import CTL.Internal.Types.ByteArray (ByteArray)
import CTL.Internal.Types.Scripts (MintingPolicyHash(MintingPolicyHash))
import Data.Either (Either(Left))
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap, wrap)
import Partial.Unsafe (unsafePartial)

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
