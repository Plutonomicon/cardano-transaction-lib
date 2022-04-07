module Test.ByteArray where

import Prelude
import Data.Maybe (Maybe(Just))
import Effect.Class (liftEffect)
import Mote (group, test)
import Test.QuickCheck (quickCheck, (===))
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import TestM (TestPlanM)
import Type.Proxy (Proxy(..))
import Types.ByteArray (ByteArray, byteArrayFromIntArray, byteArrayFromIntArrayUnsafe, byteArrayToHex, byteArrayToIntArray, hexToByteArray)

suite :: TestPlanM Unit
suite = do
  group "ByteArray" do
    test "Eq instance" $ liftEffect do
      checkEq (Proxy :: Proxy ByteArray)
    test "Ord instance" $ liftEffect do
      checkOrd (Proxy :: Proxy ByteArray)
    test "Semigroup instance" $ liftEffect do
      checkSemigroup (Proxy :: Proxy ByteArray)
    test "Monoid instance" $ liftEffect do
      checkMonoid (Proxy :: Proxy ByteArray)
    test "hexToByteArray <<< byteArrayToHex = Just" $ liftEffect do
      quickCheck \bytes ->
        hexToByteArray (byteArrayToHex bytes) === Just bytes
    test "byteArrayFromIntArrayUnsafe <<< byteArrayToIntArray = id" $ liftEffect
      do
        quickCheck \bytes ->
          byteArrayFromIntArrayUnsafe (byteArrayToIntArray bytes) === bytes
    test "byteArrayFromIntArray <<< byteArrayToIntArray = Just" $ liftEffect do
      quickCheck \bytes ->
        byteArrayFromIntArray (byteArrayToIntArray bytes) === Just bytes
