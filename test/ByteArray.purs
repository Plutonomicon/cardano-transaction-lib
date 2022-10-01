module Test.ByteArray where

import Prelude
import Ctl.Internal.Test.Utils (TestPlanM)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Mote (group, test)
import Test.QuickCheck (quickCheck, (===))
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray
  ( ByteArray
  , byteArrayFromIntArray
  , byteArrayFromIntArrayUnsafe
  , byteArrayToHex
  , byteArrayToIntArray
  , hexToByteArray
  )

suite :: TestPlanM (Aff Unit) Unit
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
