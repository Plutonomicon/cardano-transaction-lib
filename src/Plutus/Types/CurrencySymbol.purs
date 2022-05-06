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

import Data.Maybe (Maybe)
import FromData (class FromData)
import ToData (class ToData)
import Serialization.Hash
  ( ScriptHash
  , scriptHashAsBytes
  , scriptHashFromBytes
  , scriptHashToBytes
  )
import Types.ByteArray (ByteArray)
import Types.CborBytes (CborBytes(CborBytes))
import Data.Newtype (unwrap)
import Types.Scripts (MintingPolicyHash(MintingPolicyHash))

newtype CurrencySymbol = CurrencySymbol ByteArray

derive newtype instance Eq CurrencySymbol
derive newtype instance Ord CurrencySymbol
derive newtype instance FromData CurrencySymbol
derive newtype instance ToData CurrencySymbol

instance Show CurrencySymbol where
  show (CurrencySymbol cs) = "(CurrencySymbol" <> show cs <> ")"

adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol mempty

scriptHashAsCurrencySymbol :: ScriptHash -> CurrencySymbol
scriptHashAsCurrencySymbol = CurrencySymbol <<< scriptHashAsBytes

-- | The minting policy hash of a currency symbol.
currencyMPSHash :: CurrencySymbol -> Maybe MintingPolicyHash
currencyMPSHash = map MintingPolicyHash <<< currencyScriptHash

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
      scriptHashFromBytes (CborBytes byteArr) $> CurrencySymbol byteArr

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

currencyScriptHash :: CurrencySymbol -> Maybe ScriptHash
currencyScriptHash = scriptHashFromBytes <<< CborBytes <<< getCurrencySymbol
