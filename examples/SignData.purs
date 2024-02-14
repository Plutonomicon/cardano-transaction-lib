module Ctl.Examples.SignData (main, example, contract) where

import Contract.Prelude

import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.Prim.ByteArray (RawBytes, rawBytesFromAscii)
import Contract.Wallet (getChangeAddress, getRewardAddresses, signData)
import Ctl.Internal.Serialization.Address (Address)
import Data.Array (head) as Array
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Wallet.Cip30.SignData (checkCip30SignDataResponse)

main :: Effect Unit
main = example testnetNamiConfig

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
  payload = unsafePartial fromJust $ rawBytesFromAscii "Hello world!"

  testSignDataWithAddress :: String -> Address -> Contract Unit
  testSignDataWithAddress addressLabel address = do
    dataSignature <- signData address payload
    logInfo' $ "signData " <> addressLabel <> ": " <> show dataSignature
    void $ liftAff $ checkCip30SignDataResponse address dataSignature
