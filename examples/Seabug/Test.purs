module Seabug.Test (main) where

import Contract.Prelude

import Contract.Monad
import Contract.Prim.ByteArray
import Contract.Transaction
import Seabug.Contract.MarketPlaceBuy (mkMarketplaceTx)
import Seabug.Types
import Serialization as Serialization
import Untagged.Union (asOneOf)

main :: Contract Unit
main = do
  UnbalancedTx { transaction } /\ _ <- mkMarketplaceTx testNftData
  log =<<
    ( liftEffect
        <<< map
          ( byteArrayToHex
              <<< Serialization.toBytes
              <<< asOneOf
          )
    )
      (Serialization.convertTransaction transaction)

testNftData :: NftData
testNftData = undefined
