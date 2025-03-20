module Ctl.Examples.SignData (main, example, contract) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.MessageSigning (DataSignature)
import Cardano.Types (CborBytes, PublicKey, RawBytes)
import Cardano.Types.PublicKey as PublicKey
import Contract.Address (Address)
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.Wallet (getChangeAddress, getRewardAddresses, signData)
import Data.Array (head) as Array
import Data.ByteArray (byteArrayFromAscii)
import Data.Maybe (fromJust)
import Effect.Aff (error)
import Effect.Class (class MonadEffect)
import Effect.Exception (throw, throwException)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.SignData"

  changeAddress <- getChangeAddress
  logInfo' $ "change address: " <> show changeAddress
  testSignDataWithAddress "changeAddress" changeAddress

  rewardAddress <-
    liftedM "Could not get reward address" $ Array.head <$> getRewardAddresses
  logInfo' $ "reward address: " <> show rewardAddress
  testSignDataWithAddress "rewardAddress" rewardAddress
  where
  payload :: RawBytes
  payload = wrap $ unsafePartial fromJust $ byteArrayFromAscii "Hello world!"

  testSignDataWithAddress :: String -> Address -> Contract Unit
  testSignDataWithAddress addressLabel address = do
    dataSignature <- signData address payload
    logInfo' $ "signData " <> addressLabel <> ": " <> show dataSignature
    void $ liftAff $ checkCip30SignDataResponse address dataSignature

type DeserializedDataSignature =
  { coseKey :: COSEKey
  , coseSign1 :: COSESign1
  }

checkCip30SignDataResponse
  :: Address -> DataSignature -> Aff DeserializedDataSignature
checkCip30SignDataResponse address { key, signature } = do
  coseSign1 <- liftEffect $ fromBytesCoseSign1 signature
  coseKey <- liftEffect $ fromBytesCoseKey key

  checkCoseSign1ProtectedHeaders coseSign1
  checkCoseKeyHeaders coseKey
  checkKidHeaders coseSign1 coseKey
  liftEffect $ checkVerification coseSign1 coseKey
  pure { coseKey, coseSign1 }
  where
  checkCoseSign1ProtectedHeaders :: COSESign1 -> Aff Unit
  checkCoseSign1ProtectedHeaders coseSign1 = do
    assertTrue "COSE_Sign1's alg (1) header must be set to EdDSA (-8)"
      (getCoseSign1ProtectedHeaderAlg coseSign1 == Just (-8))

    assertTrue "COSE_Sign1's \"address\" header must be set to address bytes"
      ( getCoseSign1ProtectedHeaderAddress coseSign1
          == Just (encodeCbor address)
      )

  checkCoseKeyHeaders :: COSEKey -> Aff Unit
  checkCoseKeyHeaders coseKey = do
    assertTrue "COSE_Key's kty (1) header must be set to OKP (1)"
      (getCoseKeyHeaderKty coseKey == Just 1)

    assertTrue "COSE_Key's alg (3) header must be set to EdDSA (-8)"
      (getCoseKeyHeaderAlg coseKey == Just (-8))

    assertTrue "COSE_Key's crv (-1) header must be set to Ed25519 (6)"
      (getCoseKeyHeaderCrv coseKey == Just (6))

  checkKidHeaders :: COSESign1 -> COSEKey -> Aff Unit
  checkKidHeaders coseSign1 coseKey =
    assertTrue
      "COSE_Sign1's kid (4) and COSE_Key's kid (2) headers, if present, must \
      \be set to the same value"
      (getCoseSign1ProtectedHeaderKid coseSign1 == getCoseKeyHeaderKid coseKey)

  checkVerification :: COSESign1 -> COSEKey -> Effect Unit
  checkVerification coseSign1 coseKey = do
    publicKey <-
      errMaybe "COSE_Key's x (-2) header must be set to public key bytes"
        $ getCoseKeyHeaderX coseKey
        >>= PublicKey.fromRawBytes
    sigStructBytes <- getSignedData coseSign1
    assertTrue "Signature verification failed"
      =<< verifySignature coseSign1 publicKey sigStructBytes

getCoseSign1ProtectedHeaderAlg :: COSESign1 -> Maybe Int
getCoseSign1ProtectedHeaderAlg = _getCoseSign1ProtectedHeaderAlg maybeFfiHelper

getCoseSign1ProtectedHeaderAddress :: COSESign1 -> Maybe CborBytes
getCoseSign1ProtectedHeaderAddress =
  _getCoseSign1ProtectedHeaderAddress maybeFfiHelper

type MaybeFfiHelper =
  { nothing :: forall (x :: Type). Maybe x
  , just :: forall (x :: Type). x -> Maybe x
  , from :: forall (x :: Type). x -> Maybe x -> x
  }

maybeFfiHelper :: MaybeFfiHelper
maybeFfiHelper = { nothing: Nothing, just: Just, from: fromMaybe }

getCoseKeyHeaderKty :: COSEKey -> Maybe Int
getCoseKeyHeaderKty = _getCoseKeyHeaderKty maybeFfiHelper

getCoseKeyHeaderAlg :: COSEKey -> Maybe Int
getCoseKeyHeaderAlg = _getCoseKeyHeaderAlg maybeFfiHelper

getCoseKeyHeaderCrv :: COSEKey -> Maybe Int
getCoseKeyHeaderCrv = _getCoseKeyHeaderCrv maybeFfiHelper

getCoseSign1ProtectedHeaderKid :: COSESign1 -> Maybe RawBytes
getCoseSign1ProtectedHeaderKid = _getCoseSign1ProtectedHeaderKid maybeFfiHelper

getCoseKeyHeaderKid :: COSEKey -> Maybe RawBytes
getCoseKeyHeaderKid = _getCoseKeyHeaderKid maybeFfiHelper

assertTrue
  :: forall (m :: Type -> Type)
   . Applicative m
  => MonadEffect m
  => String
  -> Boolean
  -> m Unit
assertTrue msg b = unless b $ liftEffect $ throwException $ error msg

errMaybe
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadEffect m
  => String
  -> Maybe a
  -> m a
errMaybe msg = maybe (liftEffect $ throw msg) pure

getCoseKeyHeaderX :: COSEKey -> Maybe RawBytes
getCoseKeyHeaderX = _getCoseKeyHeaderX maybeFfiHelper

--------------------------------------------------------------------------------
-- Foreign functions
--------------------------------------------------------------------------------

foreign import _getCoseSign1ProtectedHeaderAlg
  :: MaybeFfiHelper -> COSESign1 -> Maybe Int

foreign import _getCoseSign1ProtectedHeaderAddress
  :: MaybeFfiHelper -> COSESign1 -> Maybe CborBytes

foreign import _getCoseKeyHeaderX :: MaybeFfiHelper -> COSEKey -> Maybe RawBytes
foreign import _getCoseKeyHeaderKty :: MaybeFfiHelper -> COSEKey -> Maybe Int
foreign import _getCoseKeyHeaderAlg :: MaybeFfiHelper -> COSEKey -> Maybe Int
foreign import _getCoseKeyHeaderCrv :: MaybeFfiHelper -> COSEKey -> Maybe Int
foreign import _getCoseSign1ProtectedHeaderKid
  :: MaybeFfiHelper -> COSESign1 -> Maybe RawBytes

foreign import _getCoseKeyHeaderKid
  :: MaybeFfiHelper -> COSEKey -> Maybe RawBytes

foreign import getSignedData :: COSESign1 -> Effect CborBytes

foreign import verifySignature
  :: COSESign1 -> PublicKey -> CborBytes -> Effect Boolean

foreign import fromBytesCoseSign1 :: CborBytes -> Effect COSESign1
foreign import fromBytesCoseKey :: CborBytes -> Effect COSEKey

foreign import data COSEKey :: Type
foreign import data COSESign1 :: Type
