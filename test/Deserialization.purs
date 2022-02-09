module Test.Deserialization where

import Prelude

import Data.BigInt as BigInt
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import TestM (TestPlanM)
import Types.ByteArray (ByteArray, hexToByteArrayUnsafe, byteArrayFromIntArrayUnsafe)

import Deserialization.Address (convertAddress)
import Deserialization.UnspentOutput (convertUnspentOutput, mkTransactionUnspentOutput, newTransactionUnspentOutputFromBytes)
import Serialization as Serialization
import Serialization.Types (TransactionUnspentOutput)
import Types.Transaction (Address(Address), BaseAddress(BaseAddress), Bech32(Bech32), Ed25519KeyHash(Ed25519KeyHash), PaymentCredential(PaymentCredentialKey), StakeCredential(StakeCredentialKey), TransactionHash(TransactionHash), TransactionInput(TransactionInput), TransactionOutput(TransactionOutput)) as T
import Types.Value (mkCoin, mkValue, mkNonAdaAssets) as V
import Types.TransactionUnspentOutput (TransactionUnspentOutput(TransactionUnspentOutput)) as T

suite :: TestPlanM Unit
suite = do
  group "cardano-serialization-lib bindings (deserialization)" $ do
    group "Address" $ do
      test "deserialization works" do
        address <- liftEffect $ Serialization.newAddressFromBech32 (T.Bech32 addressString)
        convertAddress address `shouldSatisfy` isJust
      test "deserialization is inverse to serialization" do
        address <- liftEffect $ Serialization.newAddressFromBech32 (T.Bech32 addressString)
        liftEffect case convertAddress address of
          Nothing -> throw "Failed deserialization 1"
          Just address' -> do
            address'' <- Serialization.convertAddress address'
            case convertAddress address'' of
              Nothing -> throw "Failed deserialization 2"
              Just address''' -> do
                address''' `shouldEqual` address'
    group "UnspentTransactionOutput" do
      test "deserialization is inverse to serialization" do
        case txOutputFixture1' of
          Nothing -> liftEffect $ throw "Couldn't create Fixture 1 amount"
          Just txOutputFixture1 -> do
            unspentOutput <- liftEffect $ createUnspentOutput txInputFixture1 txOutputFixture1
            let mbOutput = convertUnspentOutput unspentOutput
            case mbOutput of
              Nothing -> liftEffect $ throw "Failed deserialization 3"
              Just (T.TransactionUnspentOutput { input, output }) -> do
                input `shouldEqual` txInputFixture1
                output `shouldEqual` txOutputFixture1
      test "fixture #1" do
        let
          mbOutput = do
            out <- newTransactionUnspentOutputFromBytes utxoFixture1
            convertUnspentOutput out
        case mbOutput of
          Nothing -> liftEffect $ throw "Failed deserialization 4"
          Just res -> res `shouldEqual` utxoFixture1'

createUnspentOutput :: T.TransactionInput -> T.TransactionOutput -> Effect TransactionUnspentOutput
createUnspentOutput input output = do
  input' <- Serialization.convertTxInput input
  output' <- Serialization.convertTxOutput output
  pure $ mkTransactionUnspentOutput input' output'

addressString :: String
addressString = "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"

txInputFixture1 :: T.TransactionInput
txInputFixture1 =
  T.TransactionInput
    { transaction_id: T.TransactionHash $
        hexToByteArrayUnsafe "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
    , index: UInt.fromInt 0
    }

txOutputFixture1' :: Maybe T.TransactionOutput
txOutputFixture1' = do
  let
    currencySymbol1 = hexToByteArrayUnsafe "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"
    tokenName1 = hexToByteArrayUnsafe "4974657374546f6b656e"
  amount <- V.mkValue mempty <$> V.mkNonAdaAssets
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
    , amount
    , data_hash: Nothing
    }

utxoFixture1 :: ByteArray
utxoFixture1 = hexToByteArrayUnsafe "82825820c6b54aa301887af390bd3449833e4cd66ff61b5e68b1f77c84a8c0873b776ff90082583900f33ffa84fdf20a003443a5e2768e12e92db31535dca62088b153df243903103ae70681439b5476fef59f439b8bc86d84bfb2d376fc3f56171a004c4b40"

utxoFixture1' :: T.TransactionUnspentOutput
utxoFixture1' =
  T.TransactionUnspentOutput
    { input:
        ( T.TransactionInput
            { index: UInt.fromInt 0
            , transaction_id: T.TransactionHash (byteArrayFromIntArrayUnsafe [ 198, 181, 74, 163, 1, 136, 122, 243, 144, 189, 52, 73, 131, 62, 76, 214, 111, 246, 27, 94, 104, 177, 247, 124, 132, 168, 192, 135, 59, 119, 111, 249 ])
            }
        )
    , output:
        ( T.TransactionOutput
            { address:
                ( T.Address
                    { "AddrType":
                        ( T.BaseAddress
                            { network: UInt.fromInt 0
                            , payment:
                                (T.PaymentCredentialKey (T.Ed25519KeyHash (byteArrayFromIntArrayUnsafe [ 243, 63, 250, 132, 253, 242, 10, 0, 52, 67, 165, 226, 118, 142, 18, 233, 45, 179, 21, 53, 220, 166, 32, 136, 177, 83, 223, 36 ])))
                            , stake:
                                ( T.StakeCredentialKey
                                    (T.Ed25519KeyHash (byteArrayFromIntArrayUnsafe [ 57, 3, 16, 58, 231, 6, 129, 67, 155, 84, 118, 254, 245, 159, 67, 155, 139, 200, 109, 132, 191, 178, 211, 118, 252, 63, 86, 23 ]))
                                )
                            }
                        )
                    }
                )
            , amount: V.mkValue (V.mkCoin 5000000) mempty
            , data_hash: Nothing
            }
        )
    }
