module Test.Deserialization where

import Prelude

import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import TestM (TestPlanM)
import Types.ByteArray (hexToByteArrayUnsafe)
import Types.Value as Value

import Deserialization.Address (convertAddress)
import Deserialization.UnspentOutput (convertUnspentOutput, mkTransactionUnspentOutput)
import Serialization as Serialization
import Serialization.Types (TransactionUnspentOutput)
import Types.Transaction (Address(..), BaseAddress(..), Bech32(..), Ed25519KeyHash(..), PaymentCredential(..), StakeCredential(..), TransactionHash(..), TransactionInput(..), TransactionOutput(..)) as T
import Types.TransactionUnspentOutput (TransactionUnspentOutput(..)) as T

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
        unspentOutput <- liftEffect $ createUnspentOutput txInputFixture1 txOutputFixture1
        let mbOutput = convertUnspentOutput unspentOutput
        case mbOutput of
          Nothing -> liftEffect $ throw "Failed deserialization 3"
          Just (T.TransactionUnspentOutput { input, output }) -> do
            input `shouldEqual` txInputFixture1
            output `shouldEqual` txOutputFixture1

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

txOutputFixture1 :: T.TransactionOutput
txOutputFixture1 =
  T.TransactionOutput
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
    , amount: Value.Value (Value.Coin $ BigInt.fromInt 0) $ wrap $ Map.fromFoldable
        [ Value.CurrencySymbol currencySymbol1 /\ Map.fromFoldable
            [ Value.TokenName tokenName1 /\ BigInt.fromInt 1000000 ]
        ]
    , data_hash: Nothing
    }
  where
  currencySymbol1 = hexToByteArrayUnsafe "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"
  tokenName1 = hexToByteArrayUnsafe "4974657374546f6b656e"
