module Examples.Nami.Simple (main) where

import Prelude

import Serialization (convertTxInput)
import Effect (Effect)
import Effect.Console as Console
import Test.Main as Test
import Types.Transaction as T
import Types.ByteArray (hexToByteArrayUnsafe)
import Data.UInt as UInt
import Deserialization.UnspentOutput (convertInput)
import Effect.Aff (launchAff_)
import Test.Serialization as Serialization

main :: Effect Unit
main = do
  Console.log "Hello browser! Serialization-lib is working:"
  input <- convertTxInput txInputFixture1
  Console.log $ show $ convertInput input
  launchAff_ $ Test.interpret do
    Serialization.suite

txInputFixture1 :: T.TransactionInput
txInputFixture1 =
  T.TransactionInput
    { transaction_id: T.TransactionHash $
        hexToByteArrayUnsafe "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
    , index: UInt.fromInt 0
    }
