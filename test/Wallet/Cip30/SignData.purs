module Test.Ctl.Wallet.Cip30.SignData (suite) where

import Prelude

import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId(MainnetId)
  , addressBytes
  , intToNetworkId
  )
import Ctl.Internal.Serialization.Keys
  ( bytesFromPublicKey
  , publicKeyFromPrivateKey
  )
import Ctl.Internal.Serialization.Types (PrivateKey, PublicKey)
import Ctl.Internal.Types.ByteArray (byteArrayFromIntArrayUnsafe)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.RawBytes (RawBytes)
import Ctl.Internal.Wallet.Cip30.SignData (signData)
import Ctl.Internal.Wallet.Key
  ( PrivatePaymentKey
  , PrivateStakeKey
  , privateKeysToAddress
  )
import Data.Maybe (Maybe(Just), fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.TestM (TestPlanM)
import Test.Ctl.Utils (assertTrue)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, randomSample, vectorOf)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "signData (CIP-30)" do
    test "generates a valid signature and key for a given payload" do
      traverse_ testCip30SignData =<< liftEffect (randomSample arbitrary)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

type TestInput =
  { privateKey :: ArbitraryPrivatePaymentKey
  , privateStakeKey :: Maybe ArbitraryPrivateStakeKey
  , payload :: RawBytes
  , networkId :: ArbitraryNetworkId
  }

testCip30SignData :: TestInput -> Aff Unit
testCip30SignData { privateKey, privateStakeKey, payload, networkId } = do
  address <-
    privateKeysToAddress (unwrap privateKey) (unwrap <$> privateStakeKey)
      (unwrap networkId)

  let { key, signature } = signData privatePaymentKey address payload

  coseSign1 <- liftEffect $ fromBytesCoseSign1 signature
  coseKey <- liftEffect $ fromBytesCoseKey key

  checkCoseSign1ProtectedHeaders coseSign1 address
  checkCoseKeyHeaders coseKey
  checkKidHeaders coseSign1 coseKey
  liftEffect $ checkVerification coseSign1
  where
  privatePaymentKey :: PrivateKey
  privatePaymentKey = unwrap $ unwrap $ privateKey

  publicPaymentKey :: PublicKey
  publicPaymentKey = publicKeyFromPrivateKey privatePaymentKey

  checkCoseSign1ProtectedHeaders :: COSESign1 -> Address -> Aff Unit
  checkCoseSign1ProtectedHeaders coseSign1 address = do
    assertTrue "COSE_Sign1's alg (1) header must be set to EdDSA (-8)"
      (getCoseSign1ProtectedHeaderAlg coseSign1 == Just (-8))

    assertTrue "COSE_Sign1's \"address\" header must be set to address bytes"
      ( getCoseSign1ProtectedHeaderAddress coseSign1
          == Just (addressBytes address)
      )

  checkCoseKeyHeaders :: COSEKey -> Aff Unit
  checkCoseKeyHeaders coseKey = do
    assertTrue "COSE_Key's kty (1) header must be set to OKP (1)"
      (getCoseKeyHeaderKty coseKey == Just 1)

    assertTrue "COSE_Key's alg (3) header must be set to EdDSA (-8)"
      (getCoseKeyHeaderAlg coseKey == Just (-8))

    assertTrue "COSE_Key's crv (-1) header must be set to Ed25519 (6)"
      (getCoseKeyHeaderCrv coseKey == Just (6))

    assertTrue "COSE_Key's x (-2) header must be set to public key bytes"
      (getCoseKeyHeaderX coseKey == Just (bytesFromPublicKey publicPaymentKey))

  checkKidHeaders :: COSESign1 -> COSEKey -> Aff Unit
  checkKidHeaders coseSign1 coseKey =
    assertTrue
      "COSE_Sign1's kid (4) and COSE_Key's kid (2) headers, if present, must \
      \be set to the same value"
      (getCoseSign1ProtectedHeaderKid coseSign1 == getCoseKeyHeaderKid coseKey)

  checkVerification :: COSESign1 -> Effect Unit
  checkVerification coseSign1 = do
    sigStructBytes <- getSignedData coseSign1
    assertTrue "Signature verification failed"
      =<< verifySignature coseSign1 publicPaymentKey sigStructBytes

--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

newtype ArbitraryPrivatePaymentKey =
  ArbitraryPrivatePaymentKey PrivatePaymentKey

derive instance Newtype ArbitraryPrivatePaymentKey _

instance Arbitrary ArbitraryPrivatePaymentKey where
  arbitrary =
    wrap <<< wrap <<< unwrap <$> (arbitrary :: Gen ArbitraryPrivateKey)

newtype ArbitraryPrivateStakeKey = ArbitraryPrivateStakeKey PrivateStakeKey

derive instance Newtype ArbitraryPrivateStakeKey _

instance Arbitrary ArbitraryPrivateStakeKey where
  arbitrary =
    wrap <<< wrap <<< unwrap <$> (arbitrary :: Gen ArbitraryPrivateKey)

newtype ArbitraryPrivateKey = ArbitraryPrivateKey PrivateKey

derive instance Newtype ArbitraryPrivateKey _

instance Arbitrary ArbitraryPrivateKey where
  arbitrary =
    wrap <<< unsafePartial fromJust <<< privateKeyFromBytes <$> privateKeyBytes
    where
    privateKeyBytes :: Gen RawBytes
    privateKeyBytes =
      wrap <<< byteArrayFromIntArrayUnsafe <$> vectorOf 32 (chooseInt 0 255)

newtype ArbitraryNetworkId = ArbitraryNetworkId NetworkId

derive instance Newtype ArbitraryNetworkId _

instance Arbitrary ArbitraryNetworkId where
  arbitrary =
    wrap <<< fromMaybe MainnetId <<< intToNetworkId <$> chooseInt 0 1

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

foreign import data COSESign1 :: Type
foreign import _getCoseSign1ProtectedHeaderAlg
  :: MaybeFfiHelper -> COSESign1 -> Maybe Int

foreign import _getCoseSign1ProtectedHeaderAddress
  :: MaybeFfiHelper -> COSESign1 -> Maybe CborBytes

foreign import _getCoseSign1ProtectedHeaderKid
  :: MaybeFfiHelper -> COSESign1 -> Maybe RawBytes

foreign import data COSEKey :: Type
foreign import _getCoseKeyHeaderKty :: MaybeFfiHelper -> COSEKey -> Maybe Int
foreign import _getCoseKeyHeaderAlg :: MaybeFfiHelper -> COSEKey -> Maybe Int
foreign import _getCoseKeyHeaderCrv :: MaybeFfiHelper -> COSEKey -> Maybe Int
foreign import _getCoseKeyHeaderX :: MaybeFfiHelper -> COSEKey -> Maybe RawBytes
foreign import _getCoseKeyHeaderKid
  :: MaybeFfiHelper -> COSEKey -> Maybe RawBytes

foreign import fromBytesCoseSign1 :: CborBytes -> Effect COSESign1
foreign import fromBytesCoseKey :: CborBytes -> Effect COSEKey

foreign import getSignedData :: COSESign1 -> Effect CborBytes
foreign import verifySignature
  :: COSESign1 -> PublicKey -> CborBytes -> Effect Boolean

getCoseSign1ProtectedHeaderAlg :: COSESign1 -> Maybe Int
getCoseSign1ProtectedHeaderAlg = _getCoseSign1ProtectedHeaderAlg maybeFfiHelper

getCoseSign1ProtectedHeaderAddress :: COSESign1 -> Maybe CborBytes
getCoseSign1ProtectedHeaderAddress =
  _getCoseSign1ProtectedHeaderAddress maybeFfiHelper

getCoseSign1ProtectedHeaderKid :: COSESign1 -> Maybe RawBytes
getCoseSign1ProtectedHeaderKid = _getCoseSign1ProtectedHeaderKid maybeFfiHelper

getCoseKeyHeaderKty :: COSEKey -> Maybe Int
getCoseKeyHeaderKty = _getCoseKeyHeaderKty maybeFfiHelper

getCoseKeyHeaderAlg :: COSEKey -> Maybe Int
getCoseKeyHeaderAlg = _getCoseKeyHeaderAlg maybeFfiHelper

getCoseKeyHeaderCrv :: COSEKey -> Maybe Int
getCoseKeyHeaderCrv = _getCoseKeyHeaderCrv maybeFfiHelper

getCoseKeyHeaderX :: COSEKey -> Maybe RawBytes
getCoseKeyHeaderX = _getCoseKeyHeaderX maybeFfiHelper

getCoseKeyHeaderKid :: COSEKey -> Maybe RawBytes
getCoseKeyHeaderKid = _getCoseKeyHeaderKid maybeFfiHelper
