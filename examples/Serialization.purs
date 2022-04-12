module Examples.Serialization (main) where

import Prelude

import Data.UInt as UInt
import Deserialization.UnspentOutput (convertInput)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console as Console
import Serialization (convertTxInput)
import Test.Main as Test
import Test.Serialization as Serialization
import Test.AffInterface as AffInterface
import Types.ByteArray (hexToByteArrayUnsafe)
import Types.Transaction as T

main :: Effect Unit
main = do
  Console.log "Hello browser! Serialization-lib is working:"
  input <- convertTxInput txInputFixture1
  Console.log $ show $ convertInput input
  launchAff_ $ do
    Test.interpret do
      AffInterface.suite
      Serialization.suite

txInputFixture1 :: T.TransactionInput
txInputFixture1 =
  T.TransactionInput
    { transactionId: T.TransactionHash $
        hexToByteArrayUnsafe
          "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
    , index: UInt.fromInt 0
    }
