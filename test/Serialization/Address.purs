module Test.Serialization.Address(suite) where

import Prelude

import Data.Maybe (Maybe, maybe, fromJust)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Partial.Unsafe (unsafePartial)
import TestM (TestPlanM)
import Serialization.Address (Bech32String(..), addressBech32, addressNetworkId, addressPubKeyHash, addressStakeKeyHash, fromBech32, newBaseAddress)


errBool :: String -> Boolean -> TestPlanM Unit
errBool msg b =
  if b
  then pure unit
  else (liftEffect $ throwException $ error msg)

errMaybe :: forall a. String -> Maybe a -> TestPlanM a
errMaybe msg =
  maybe
  (liftEffect $ throwException $ error msg)
  pure

doesNotThrow :: forall a. a -> TestPlanM a
doesNotThrow = pure

-- This address should have both stake keyhash and payment keyhash
testAddrString :: Bech32String
testAddrString = Bech32String "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"

suite :: TestPlanM Unit
suite = do
  addr <- errMaybe "failed fromBech32" (fromBech32 testAddrString)
  errBool "Bech32 rep does not match origin" (addressBech32 addr == testAddrString)
  netId <- doesNotThrow $ addressNetworkId addr
  mpkh <- doesNotThrow $ addressPubKeyHash addr
  mskh <- doesNotThrow $ addressStakeKeyHash addr
  addr2 <- doesNotThrow $
    let skh = unsafePartial (fromJust mskh)
        pkh = unsafePartial (fromJust mpkh)
    in newBaseAddress netId pkh skh
  errBool "Reconstructed address does not match original" (addr2 == addr)
  pure unit
