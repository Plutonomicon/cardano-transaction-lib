module Test.Types.Address(suite) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import TestM (TestPlanM)
import Types.Address (Bech32String(..), addressBech32, addressNetworkId, addressPubKeyHash, addressStakeKeyHash, fromBech32, newBaseAddress)


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

testAddrString :: Bech32String
testAddrString = Bech32String "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"

suite :: TestPlanM Unit
suite = do
  addr <- errMaybe "failed fromBech32" (fromBech32 testAddrString)
  errBool "Bech32 rep does not match origin" (addressBech32 addr == testAddrString)
  netId <- doesNotThrow $ addressNetworkId addr
  pkh <- doesNotThrow $ addressPubKeyHash addr
  skh <- doesNotThrow $ addressStakeKeyHash addr
  addr2 <- doesNotThrow $ newBaseAddress netId pkh skh
  errBool "Reconstructed address does not match original" (addr2 == addr)
  pure unit
