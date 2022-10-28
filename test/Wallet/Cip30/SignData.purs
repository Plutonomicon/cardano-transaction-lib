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
import Ctl.Internal.Serialization.Types (PrivateKey)
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
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.TestM (TestPlanM)
import Test.Ctl.Utils (assertTrue, errMaybe)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, randomSample, vectorOf)

type TestInput =
  { privateKey :: ArbitraryPrivatePaymentKey
  , privateStakeKey :: Maybe ArbitraryPrivateStakeKey
  , payload :: RawBytes
  , networkId :: ArbitraryNetworkId
  }

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "signData (CIP-30)" do
    test "generates a valid signature and key for a given payload" do
      traverse_ testCip30SignData =<< liftEffect (randomSample arbitrary)

testCip30SignData :: TestInput -> Aff Unit
testCip30SignData { privateKey, privateStakeKey, payload, networkId } = do
  address <-
    privateKeysToAddress (unwrap privateKey) (unwrap <$> privateStakeKey)
      (unwrap networkId)
  let
    privatePaymentKey :: PrivateKey
    privatePaymentKey = unwrap $ unwrap $ privateKey

    { key, signature } = signData privatePaymentKey address payload

  coseSign1 <-
    errMaybe "fromBytesCoseSign1 failed" (fromBytesCoseSign1 signature)
  _ <-
    errMaybe "fromBytesCoseKey failed" (fromBytesCoseKey key)

  checkCoseSign1ProtectedHeaders coseSign1 address
  where
  checkCoseSign1ProtectedHeaders :: COSESign1 -> Address -> Aff Unit
  checkCoseSign1ProtectedHeaders coseSign1 address = do
    assertTrue "COSE_Sign1's alg (1) header must be set to EdDSA (-8)"
      (getCoseSign1ProtectedHeaderAlg coseSign1 == Just (-8))
    assertTrue "COSE_Sign1's \"address\" header must be set to address bytes"
      ( getCoseSign1ProtectedHeaderAddress coseSign1
          == Just (addressBytes address)
      )

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
foreign import _fromBytesCoseSign1
  :: MaybeFfiHelper -> CborBytes -> Maybe COSESign1

foreign import _getCoseSign1ProtectedHeaderAlg
  :: MaybeFfiHelper -> COSESign1 -> Maybe Int

foreign import _getCoseSign1ProtectedHeaderAddress
  :: MaybeFfiHelper -> COSESign1 -> Maybe CborBytes

foreign import data COSEKey :: Type
foreign import _fromBytesCoseKey
  :: MaybeFfiHelper -> CborBytes -> Maybe COSEKey

fromBytesCoseSign1 :: CborBytes -> Maybe COSESign1
fromBytesCoseSign1 = _fromBytesCoseSign1 maybeFfiHelper

fromBytesCoseKey :: CborBytes -> Maybe COSEKey
fromBytesCoseKey = _fromBytesCoseKey maybeFfiHelper

getCoseSign1ProtectedHeaderAlg :: COSESign1 -> Maybe Int
getCoseSign1ProtectedHeaderAlg = _getCoseSign1ProtectedHeaderAlg maybeFfiHelper

getCoseSign1ProtectedHeaderAddress :: COSESign1 -> Maybe CborBytes
getCoseSign1ProtectedHeaderAddress =
  _getCoseSign1ProtectedHeaderAddress maybeFfiHelper
