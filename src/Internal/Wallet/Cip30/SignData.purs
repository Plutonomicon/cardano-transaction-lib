module Ctl.Internal.Wallet.Cip30.SignData (signData) where

import Prelude

import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Serialization.Keys
  ( bytesFromPublicKey
  , publicKeyFromPrivateKey
  )
import Ctl.Internal.Serialization.ToBytes (toBytes)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes(CborBytes))
import Ctl.Internal.Types.RawBytes (RawBytes(RawBytes))
import Ctl.Internal.Wallet.Cip30 (DataSignature)
import Effect (Effect)

foreign import data COSESign1Builder :: Type
foreign import newCoseSign1Builder
  :: ByteArray -> Headers -> Effect COSESign1Builder

foreign import makeDataToSign :: COSESign1Builder -> ByteArray
foreign import sign :: PrivateKey -> ByteArray -> ByteArray
foreign import buildSignature :: COSESign1Builder -> ByteArray -> ByteArray

foreign import data Headers :: Type
foreign import newHeaders :: HeaderMap -> ProtectedHeaderMap -> Headers

foreign import data ProtectedHeaderMap :: Type
foreign import newProtectedHeaderMap :: HeaderMap -> ProtectedHeaderMap

foreign import data HeaderMap :: Type
foreign import newHeaderMap :: Effect HeaderMap
foreign import setAlgHeaderToEdDsa :: HeaderMap -> Effect Unit
foreign import setAddressHeader :: CborBytes -> HeaderMap -> Effect Unit

foreign import data COSEKey :: Type
foreign import newCoseKeyWithOkpType :: Effect COSEKey
foreign import setCoseKeyAlgHeaderToEdDsa :: COSEKey -> Effect Unit
foreign import setCoseKeyCrvHeaderToEd25519 :: COSEKey -> Effect Unit
foreign import setCoseKeyXHeader :: RawBytes -> COSEKey -> Effect Unit
foreign import bytesFromCoseKey :: COSEKey -> CborBytes

signData :: PrivateKey -> Address -> RawBytes -> Effect DataSignature
signData privatePaymentKey address (RawBytes payload) =
  { key: _, signature: _ } <$> key <*> signature
  where
  key :: Effect CborBytes
  key = do
    coseKey <- newCoseKeyWithOkpType
    setCoseKeyAlgHeaderToEdDsa coseKey
    setCoseKeyCrvHeaderToEd25519 coseKey
    setCoseKeyXHeader publicPaymentKeyBytes coseKey
    pure $ bytesFromCoseKey coseKey
    where
    publicPaymentKeyBytes :: RawBytes
    publicPaymentKeyBytes =
      bytesFromPublicKey (publicKeyFromPrivateKey privatePaymentKey)

  signature :: Effect CborBytes
  signature = CborBytes <$> (buildSignature <$> builder <*> signedSigStruct)
    where
    signedSigStruct :: Effect ByteArray
    signedSigStruct = sign privatePaymentKey <<< makeDataToSign <$> builder

    builder :: Effect COSESign1Builder
    builder = headers >>= newCoseSign1Builder payload
      where
      headers :: Effect Headers
      headers = newHeaders <$> newHeaderMap <*> protectedHeaders

      protectedHeaders :: Effect ProtectedHeaderMap
      protectedHeaders = do
        headerMap <- newHeaderMap
        setAlgHeaderToEdDsa headerMap
        setAddressHeader (toBytes address) headerMap
        pure $ newProtectedHeaderMap headerMap
