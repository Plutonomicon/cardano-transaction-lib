module Ctl.Internal.Service.Helpers
  ( aesonArray
  , aesonString
  , aesonObject
  , decodeAssetClass
  , decodePlutusAssetClass
  ) where

import Prelude

import Aeson
  ( Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonArray
  , caseAesonObject
  , caseAesonString
  )
import Control.Apply (lift2)
import Ctl.Internal.Cardano.Types.Value (CurrencySymbol, mkCurrencySymbol)
import Ctl.Internal.Plutus.Types.CurrencySymbol as Plutus
import Ctl.Internal.Types.ByteArray (hexToByteArray)
import Ctl.Internal.Types.TokenName (TokenName, mkTokenName)
import Data.Either (Either(Left), note)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Foreign.Object (Object)

aesonArray
  :: forall (a :: Type)
   . (Array Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonArray = caseAesonArray (Left (TypeMismatch "Array"))

aesonObject
  :: forall (a :: Type)
   . (Object Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonObject = caseAesonObject (Left (TypeMismatch "Object"))

aesonString
  :: forall (a :: Type)
   . (String -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonString = caseAesonString (Left (TypeMismatch "String"))

decodeAssetClass
  :: String
  -> String
  -> String
  -> Either JsonDecodeError (CurrencySymbol /\ TokenName)
decodeAssetClass assetString csString tnString =
  lift2 Tuple
    ( note (assetStringTypeMismatch "CurrencySymbol" csString)
        (mkCurrencySymbol =<< hexToByteArray csString)
    )
    ( note (assetStringTypeMismatch "TokenName" tnString)
        (mkTokenName =<< hexToByteArray tnString)
    )
  where
  assetStringTypeMismatch :: String -> String -> JsonDecodeError
  assetStringTypeMismatch t actual =
    TypeMismatch $
      ("In " <> assetString <> ": Expected hex-encoded " <> t)
        <> (", got: " <> actual)

decodePlutusAssetClass
  :: String
  -> String
  -> String
  -> Either JsonDecodeError (Plutus.CurrencySymbol /\ TokenName)
decodePlutusAssetClass assetString csString tnString =
  lift2 Tuple
    ( note (assetStringTypeMismatch "Plutus.CurrencySymbol" csString)
        (Plutus.mkCurrencySymbol =<< hexToByteArray csString)
    )
    ( note (assetStringTypeMismatch "TokenName" tnString)
        (mkTokenName =<< hexToByteArray tnString)
    )
  where
  assetStringTypeMismatch :: String -> String -> JsonDecodeError
  assetStringTypeMismatch t actual =
    TypeMismatch $
      ("In " <> assetString <> ": Expected hex-encoded " <> t)
        <> (", got: " <> actual)
