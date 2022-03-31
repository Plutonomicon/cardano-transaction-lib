module Seabug.Test (main) where

import Contract.Prelude

import Contract.Monad
  ( Contract
  , defaultContractConfig
  , liftContractM
  , runContract_
  )
import Contract.Numeric.Natural (fromBigInt')
import Contract.Prim.ByteArray
  ( byteArrayFromString
  -- , byteArrayToHex
  , hexToByteArray
  )
import Contract.Time (Slot(Slot))
-- import Contract.Transaction (UnbalancedTx(UnbalancedTx))
import Contract.Value (mkCurrencySymbol, mkTokenName)
import Data.BigInt as BigInt
import Data.UInt as UInt
import Effect.Aff (launchAff_)
import Seabug.Contract.MarketPlaceBuy (marketplaceBuy)
import Seabug.Types
  ( NftCollection(NftCollection)
  , NftData(NftData)
  , NftId(NftId)
  )
-- import Serialization as Serialization
import Serialization.Hash (ed25519KeyHashFromBytes, scriptHashFromBytes)

-- import Untagged.Union (asOneOf)

main :: Effect Unit
main = launchAff_ $ do
  cfg <- defaultContractConfig
  runContract_ cfg $ do
    marketplaceBuy =<< testNftData

-- UnbalancedTx { transaction } /\ _ <- mkMarketplaceTx =<< testNftData
-- log =<<
--   ( liftEffect
--       <<< map
--         ( byteArrayToHex
--             <<< Serialization.toBytes
--             <<< asOneOf
--         )
--   ) (Serialization.convertTransaction transaction)

testNftData :: Contract NftData
testNftData = do
  kh <- liftContractM "`Ed25519KeyHash`"
    $ ed25519KeyHashFromBytes
    =<< hexToByteArray "3f3464650beb5324d0e463ebe81fbe1fd519b6438521e96d0d35bd75"
  collectionNftCs <- liftContractM "`CurrencySymbol`"
    $ mkCurrencySymbol
    =<< hexToByteArray "cf0c1cbf47537f238f756fc1be191abf76009e1988910092184c4b7f"
  lockingScript <- liftContractM "`ScriptHash`"
    $ scriptHashFromBytes
    =<< hexToByteArray "6c1039b6973bb0e7ad42de5b16a691ede3e0265cd58caf070ff15ef3"
  daoScript <- liftContractM "`ScriptHash`"
    $ scriptHashFromBytes
    =<< hexToByteArray "9da8fa76a2a0f52aa5df10fb7b81f9afe4b20e9068b3f95fadc7477a"
  tokenName <- liftContractM "`TokenName`"
    $ mkTokenName
    =<< byteArrayFromString "NFT-1-1"
  pure $ NftData
    { nftCollection: NftCollection
        { collectionNftCs
        , lockLockup: BigInt.fromInt 5
        , lockLockupEnd: Slot $ UInt.fromInt 5
        , lockingScript: wrap lockingScript
        , author: wrap $ wrap kh
        , authorShare: fromBigInt' $ BigInt.fromInt 1000
        , daoScript: wrap daoScript
        , daoShare: fromBigInt' $ BigInt.fromInt 500
        }
    , nftId: NftId
        { collectionNftTn: tokenName
        , price: fromBigInt' $ BigInt.fromInt 100_000_000
        , owner: wrap $ wrap kh
        }
    }
