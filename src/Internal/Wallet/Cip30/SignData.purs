module Ctl.Internal.Wallet.Cip30.SignData (signData) where

import Prelude

import Ctl.Internal.Serialization.Address (Address, addressBytes)
import Ctl.Internal.Serialization.Keys
  ( bytesFromPublicKey
  , publicKeyFromPrivateKey
  )
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.CborBytes (CborBytes(CborBytes))
import Ctl.Internal.Types.RawBytes (RawBytes(RawBytes))
import Ctl.Internal.Wallet.Cip30 (DataSignature)

foreign import data COSESign1Builder :: Type
foreign import newCoseSign1Builder :: ByteArray -> Headers -> COSESign1Builder
foreign import makeDataToSign :: COSESign1Builder -> ByteArray
foreign import sign :: PrivateKey -> ByteArray -> ByteArray
foreign import buildSignature :: COSESign1Builder -> ByteArray -> ByteArray

foreign import data Headers :: Type
foreign import newHeaders :: HeaderMap -> ProtectedHeaderMap -> Headers

foreign import data HeaderMap :: Type
foreign import newHeaderMap :: HeaderMap
foreign import setAlgHeaderToEdDsa :: HeaderMap -> HeaderMap
foreign import setAddressHeader :: CborBytes -> HeaderMap -> HeaderMap

foreign import data ProtectedHeaderMap :: Type
foreign import newProtectedHeaderMap :: HeaderMap -> ProtectedHeaderMap

foreign import data COSEKey :: Type
foreign import newCoseKeyWithOkpType :: COSEKey
foreign import setCoseKeyAlgHeaderToEdDsa :: COSEKey -> COSEKey
foreign import setCoseKeyCrvHeaderToEd25519 :: COSEKey -> COSEKey
foreign import setCoseKeyXHeader :: RawBytes -> COSEKey -> COSEKey
foreign import bytesFromCoseKey :: COSEKey -> CborBytes

signData :: PrivateKey -> Address -> RawBytes -> DataSignature
signData privatePaymentKey address (RawBytes payload) = { key, signature }
  where
  key :: CborBytes
  key =
    bytesFromCoseKey
      ( newCoseKeyWithOkpType
          # setCoseKeyAlgHeaderToEdDsa
          # setCoseKeyCrvHeaderToEd25519
          # setCoseKeyXHeader publicPaymentKeyBytes
      )
    where
    publicPaymentKeyBytes :: RawBytes
    publicPaymentKeyBytes =
      bytesFromPublicKey (publicKeyFromPrivateKey privatePaymentKey)

  signature :: CborBytes
  signature = CborBytes (buildSignature builder signedSigStruct)
    where
    signedSigStruct :: ByteArray
    signedSigStruct = sign privatePaymentKey (makeDataToSign builder)

    builder :: COSESign1Builder
    builder =
      newCoseSign1Builder payload (newHeaders newHeaderMap protectedHeaders)
      where
      protectedHeaders :: ProtectedHeaderMap
      protectedHeaders =
        newProtectedHeaderMap
          ( newHeaderMap
              # setAlgHeaderToEdDsa
              # setAddressHeader (addressBytes address)
          )
