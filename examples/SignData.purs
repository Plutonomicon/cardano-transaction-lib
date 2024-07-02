module Ctl.Examples.SignData (main, example, contract) where

import Contract.Prelude

import Cardano.Types (RawBytes)
import Cardano.Wallet.Cip30.SignData (checkCip30SignDataResponse)
import Contract.Address (Address)
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.Wallet (getChangeAddress, getRewardAddresses, signData)
import Data.Array (head) as Array
import Data.ByteArray (byteArrayFromAscii)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

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
  payload = wrap $ unsafePartial fromJust $ byteArrayFromAscii "Hello world!"

  testSignDataWithAddress :: String -> Address -> Contract Unit
  testSignDataWithAddress addressLabel address = do
    dataSignature <- signData address payload
    logInfo' $ "signData " <> addressLabel <> ": " <> show dataSignature
    void $ liftAff $ checkCip30SignDataResponse address dataSignature
