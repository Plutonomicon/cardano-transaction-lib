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
  , byteArrayToHex
  , hexToByteArray
  )
import Contract.Time (Slot(Slot))
import Contract.Transaction (UnbalancedTx(UnbalancedTx))
import Contract.Value (mkCurrencySymbol, mkTokenName)
import Data.BigInt as BigInt
import Data.UInt as UInt
import Effect.Aff (launchAff_)
import Seabug.Contract.MarketPlaceBuy (marketplaceBuy, mkMarketplaceTx)
import Seabug.Types
  ( NftCollection(NftCollection)
  , NftData(NftData)
  , NftId(NftId)
  )
import Serialization as Serialization
import Serialization.Hash (ed25519KeyHashFromBytes, scriptHashFromBytes)
import Untagged.Union (asOneOf)

main :: Effect Unit
main = launchAff_ $ do
  cfg <- defaultContractConfig
  runContract_ cfg $ do
    log =<< map show testNftData
    tdt <- testNftData
    log $ show tdt
    log "logged again"
    -- marketplaceBuy tdt
    UnbalancedTx { transaction } /\ _ <- mkMarketplaceTx tdt
    log =<<
      ( liftEffect
          <<< map
            ( byteArrayToHex
                <<< Serialization.toBytes
                <<< asOneOf
            )
      ) (Serialization.convertTransaction transaction)

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

-- (NftData { nftCollection: (NftCollection { author: (PaymentPubKeyHash (PubKeyHash (Ed25519KeyHash 3f3464650beb5324d0e463ebe81fbe1fd519b6438521e96d0d35bd75))),
--  authorShare: (fromBigInt' (BigInt.fromString "1000")),
--  collectionNftCs: (CurrencySymbol(byteArrayFromIntArrayUnsafe [207,12,28,191,71,83,127,35,143,117,111,193,190,25,26,191,118,0,158,25,136,145,0,146,24,76,75,127])),
--   daoScript: (ValidatorHash (ScriptHash 9da8fa76a2a0f52aa5df10fb7b81f9afe4b20e9068b3f95fadc7477a)),
--   daoShare: (fromBigInt' (BigInt.fromString "500")),
--  lockLockup: fromString "5",
-- lockLockupEnd: (Slot 5u)
--    lockingScript: (ValidatorHash (ScriptHash 6c1039b6973bb0e7ad42de5b16a691ede3e0265cd58caf070ff15ef3)) }),
--    nftId: (NftId { collectionNftTn: (TokenName(byteArrayFromIntArrayUnsafe [78,70,84,45,49,45,49])),
--    owner: (PaymentPubKeyHash (PubKeyHash (Ed25519KeyHash 3f3464650beb5324d0e463ebe81fbe1fd519b6438521e96d0d35bd75))),
--    price: (fromBigInt' (BigInt.fromString "100000000")) }) })

-- let depositParams = NftData
--     { nftData'nftCollection = NftCollection
--       { nftCollection'collectionNftCs = "cf0c1cbf47537f238f756fc1be191abf76009e1988910092184c4b7f"
--       , nftCollection'lockLockup = 5
--       , nftCollection'lockLockupEnd = 5
--       , nftCollection'lockingScript = "6c1039b6973bb0e7ad42de5b16a691ede3e0265cd58caf070ff15ef3"
--           -- validatorHash $ lockValidator "cf0c1cbf47537f238f756fc1be191abf76009e1988910092184c4b7f" 5 5
--       , nftCollection'author = PaymentPubKeyHash "3f3464650beb5324d0e463ebe81fbe1fd519b6438521e96d0d35bd75"
--       , nftCollection'authorShare = toEnum 1000
--       , nftCollection'daoScript = "9da8fa76a2a0f52aa5df10fb7b81f9afe4b20e9068b3f95fadc7477a" -- validatorHash $ daoValidator []
--       , nftCollection'daoShare = toEnum 500
--       }
--     , nftData'nftId = NftId
--       { nftId'collectionNftTn = "NFT-1-1"
--       , nftId'price = toEnum 100_000_000
--       , nftId'owner = PaymentPubKeyHash "3f3464650beb5324d0e463ebe81fbe1fd519b6438521e96d0d35bd75"
--       }
--     }
