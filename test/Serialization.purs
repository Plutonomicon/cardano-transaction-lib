module Test.Serialization where

import Prelude

import Data.BigInt as BigInt
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, test)
import Serialization (addressPubKeyHash, convertBigInt, convertTransaction, convertTxOutput, newAddressFromBech32, newBaseAddressFromAddress, toBytes)
import Serialization.Types (TransactionHash)
import Deserialization.FromBytes (fromBytesEffect)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Types.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Types.Transaction as T
import Types.Value as Value
import Untagged.Union (asOneOf)

suite :: TestPlanM Unit
suite = do
  group "cardano-serialization-lib bindings" $ do
    group "conversion between types" $ do
      test "convertBigNum" do
        void $ liftEffect $ convertBigInt (BigInt.fromInt 123)
      test "newTransactionHash" do
        let
          txString = "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
          txBytes = hexToByteArrayUnsafe txString
        _txHash :: TransactionHash <- liftEffect $ fromBytesEffect txBytes
        pure unit
      test "BaseAddress <-> Address" do
        let
          addressString = "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"
        address <- liftEffect $ newAddressFromBech32 (T.Bech32 addressString)
        let
          mbKeyHash = newBaseAddressFromAddress address >>= addressPubKeyHash
        mbKeyHash `shouldEqual` Just (T.Bech32 "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp")
      test "TransactionOutput serialization" $ liftEffect do
        txo <- convertTxOutput txOutputFixture1
        let bytes = toBytes (asOneOf txo)
        byteArrayToHex bytes `shouldEqual` txOutputBinaryFixture1
      test "Transaction serialization #1" $ liftEffect do
        tx <- convertTransaction txFixture1
        let bytes = toBytes (asOneOf tx)
        byteArrayToHex bytes `shouldEqual` txBinaryFixture1
      test "Transaction serialization #2 - tokens" $ liftEffect do
        case txFixture2' of
          Nothing -> throw "Couldn't create Fixture 2 amount"
          Just txFixture2 -> do
            tx <- convertTransaction txFixture2
            let bytes = toBytes (asOneOf tx)
            byteArrayToHex bytes `shouldEqual` txBinaryFixture2
      test "Transaction serialization #3 - ada" $ liftEffect do
        tx <- convertTransaction txFixture3
        let bytes = toBytes (asOneOf tx)
        byteArrayToHex bytes `shouldEqual` txBinaryFixture3

txInputFixture1 :: T.TransactionInput
txInputFixture1 =
  T.TransactionInput
    { transaction_id: T.TransactionHash $
        hexToByteArrayUnsafe "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
    , index: UInt.fromInt 0
    }

txOutputFixture1 :: T.TransactionOutput
txOutputFixture1 =
  T.TransactionOutput
    { address: T.Address
        { "AddrType": T.BaseAddress
            { network: UInt.fromInt 0
            , stake: T.StakeCredentialKey $ T.Ed25519KeyHash
                -- $ T.Bech32 "hstk_1rsf0q0q77t5nttxrtmpwd7tvv58a80a686t92pgy65ekz0s8ncu"
                $ hexToByteArrayUnsafe "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"
            , payment: T.PaymentCredentialKey $ T.Ed25519KeyHash
                -- "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
                $ hexToByteArrayUnsafe "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
            }
        }
    , amount: mempty
    , data_hash: Nothing
    }

txOutputFixture2' :: Maybe T.TransactionOutput
txOutputFixture2' = do
  let
    currencySymbol1 = hexToByteArrayUnsafe "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"
    tokenName1 = hexToByteArrayUnsafe "4974657374546f6b656e"
  amount <- Value.mkValue mempty <$> Value.mkNonAdaAssets
    [ currencySymbol1 /\ [ tokenName1 /\ BigInt.fromInt 1000000 ]
    ]
  pure $ T.TransactionOutput
    { address: T.Address
        { "AddrType": T.BaseAddress
            { network: UInt.fromInt 0
            , stake: T.StakeCredentialKey $ T.Ed25519KeyHash
                -- "hstk_1rsf0q0q77t5nttxrtmpwd7tvv58a80a686t92pgy65ekz0s8ncu"
                $ hexToByteArrayUnsafe "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"
            , payment: T.PaymentCredentialKey $ T.Ed25519KeyHash
                -- "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
                $ hexToByteArrayUnsafe "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
            }
        }
    , amount: amount
    , data_hash: Nothing
    }

txOutputBinaryFixture1 :: String
txOutputBinaryFixture1 =
  "8258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d5336100"

txFixture1 :: T.Transaction
txFixture1 =
  T.Transaction
    { body: T.TxBody
        { inputs: [ txInputFixture1 ]
        , outputs: [ txOutputFixture1 ]
        , fee: Value.mkCoin 177513
        , ttl: Nothing
        , certs: Nothing
        , withdrawals: Nothing
        , update: Nothing
        , auxiliary_data_hash: Nothing
        , validity_start_interval: Nothing
        , mint: Nothing
        , script_data_hash: Nothing
        , collateral: Nothing
        , required_signers: Nothing
        , network_id: Just T.Mainnet
        }
    , witness_set: T.TransactionWitnessSet
        { vkeys: Nothing
        , native_scripts: Nothing
        , bootstraps: Nothing
        , plutus_scripts: Nothing
        , plutus_data: Nothing
        , redeemers: Nothing
        }
    , is_valid: true
    , auxiliary_data: Nothing
    }

txFixture2' :: Maybe T.Transaction
txFixture2' = do
  txOutputFixture2 <- txOutputFixture2'
  pure $ T.Transaction
    { body: T.TxBody
        { inputs: [ txInputFixture1 ]
        , outputs: [ txOutputFixture2 ]
        , fee: Value.mkCoin 177513
        , ttl: Nothing
        , certs: Nothing
        , withdrawals: Nothing
        , update: Nothing
        , auxiliary_data_hash: Nothing
        , validity_start_interval: Nothing
        , mint: Nothing
        , script_data_hash: Nothing
        , collateral: Nothing
        , required_signers: Nothing
        , network_id: Just T.Mainnet
        }
    , witness_set: T.TransactionWitnessSet
        { vkeys: Nothing
        , native_scripts: Nothing
        , bootstraps: Nothing
        , plutus_scripts: Nothing
        , plutus_data: Nothing
        , redeemers: Nothing
        }
    , is_valid: true
    , auxiliary_data: Nothing
    }

txFixture3 :: T.Transaction
txFixture3 =
  T.Transaction
    { body: T.TxBody
        { inputs: [ txInputFixture1 ]
        , outputs:
            [ T.TransactionOutput
                { address: T.Address
                    { "AddrType": T.BaseAddress
                        { network: UInt.fromInt 0
                        , stake: T.StakeCredentialKey $ T.Ed25519KeyHash
                            $ hexToByteArrayUnsafe "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
                        , payment: T.PaymentCredentialKey $ T.Ed25519KeyHash
                            -- $ T.Bech32 "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
                            $ hexToByteArrayUnsafe "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
                        }
                    }
                , amount: Value.mkValue (Value.mkCoin 2353402) mempty
                , data_hash: Nothing
                }
            , T.TransactionOutput
                { address: T.Address
                    { "AddrType": T.BaseAddress
                        { network: UInt.fromInt 0
                        , stake: T.StakeCredentialKey $ T.Ed25519KeyHash
                            $ hexToByteArrayUnsafe "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
                        , payment: T.PaymentCredentialKey $ T.Ed25519KeyHash
                            -- $ T.Bech32 "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
                            $ hexToByteArrayUnsafe "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
                        }
                    }
                , amount: Value.mkValue (Value.mkCoin 1000000) mempty
                , data_hash: Nothing
                }
            ]
        , fee: Value.mkCoin 177513
        , ttl: Nothing
        , certs: Nothing
        , withdrawals: Nothing
        , update: Nothing
        , auxiliary_data_hash: Nothing
        , validity_start_interval: Nothing
        , mint: Nothing
        , script_data_hash: Nothing
        , collateral: Nothing
        , required_signers: Nothing
        , network_id: Just T.Mainnet
        }
    , witness_set: T.TransactionWitnessSet
        { vkeys: Nothing
        , native_scripts: Nothing
        , bootstraps: Nothing
        , plutus_scripts: Nothing
        , plutus_data: Nothing
        , redeemers: Nothing
        }
    , is_valid: true
    , auxiliary_data: Nothing
    }

-- | This is a valid cborHex. To quickly check a serialized tx, create a file with the following contents:
-- |
-- |
-- | ```
-- | {
-- |   "type": "Tx AlonzoEra",
-- |   "description": "",
-- |   "cborHex": ...
-- | }
-- | ```
-- |
-- | And call `cardano-cli transaction view --tx-file ./that-file`
-- |
-- | TODO: integrate cardano-cli (or something else) into the testing toolchain to check validity of serialized TXs.
txBinaryFixture1 :: String
txBinaryFixture1 = "84a300818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960001818258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d5336100021a0002b569a0f5f6"

txBinaryFixture2 :: String
txBinaryFixture2 =
  "84a300818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960001818258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d533618200a1581c1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2ea14a4974657374546f6b656e1a000f4240021a0002b569a0f5f6"

txBinaryFixture3 :: String
txBinaryFixture3 = "84a300818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960001828258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9710f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f975461a0023e8fa8258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9710f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f975461a000f4240021a0002b569a0f5f6"
