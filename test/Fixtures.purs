module Test.Fixtures
  ( txOutputFixture1
  , txOutputFixture2
  , currencySymbol1
  , nativeScriptFixture1
  , nativeScriptFixture2
  , nativeScriptFixture3
  , nativeScriptFixture4
  , nativeScriptFixture5
  , nativeScriptFixture6
  , nativeScriptFixture7
  , plutusDataFixture1
  , plutusDataFixture2
  , plutusDataFixture3
  , plutusDataFixture4
  , plutusDataFixture5
  , plutusDataFixture6
  , plutusDataFixture7
  , tokenName1
  , txOutputBinaryFixture1
  , txFixture1
  , txFixture2
  , txFixture3
  , txBinaryFixture1
  , txBinaryFixture2
  , txBinaryFixture3
  , utxoFixture1
  , utxoFixture1'
  , witnessSetFixture1
  , witnessSetFixture2
  , witnessSetFixture3
  , witnessSetFixture4
  , witnessSetFixture2Value
  , witnessSetFixture3Value
  , addressString1
  , txInputFixture1
  ) where

import Prelude

import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Partial.Unsafe (unsafePartial)
import Serialization.Address
  ( Address
  , NetworkId(MainnetId, TestnetId)
  , Slot(Slot)
  , baseAddress
  , baseAddressToAddress
  , keyHashCredential
  )
import Serialization.Hash (Ed25519KeyHash, ed25519KeyHashFromBytes)
import Types.ByteArray
  ( ByteArray
  , byteArrayFromIntArrayUnsafe
  , hexToByteArrayUnsafe
  )
import Types.PlutusData as PD
import Types.Transaction
  ( Ed25519Signature(Ed25519Signature)
  , NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  , PlutusData(PlutusData)
  , PublicKey(PublicKey)
  , Transaction(Transaction)
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TransactionWitnessSet(TransactionWitnessSet)
  , TxBody(TxBody)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  )
import Types.TransactionUnspentOutput (TransactionUnspentOutput(TransactionUnspentOutput))
import Types.Value
  ( Coin(Coin)
  , CurrencySymbol
  , TokenName
  , Value(Value)
  , mkCurrencySymbol
  , mkTokenName
  , mkSingletonNonAdaAsset
  )

txOutputFixture1 :: TransactionOutput
txOutputFixture1 =
  TransactionOutput
    { address: baseAddressToAddress $ baseAddress
        { network: TestnetId
        , delegationCred:
            keyHashCredential $ unsafePartial $ fromJust $ ed25519KeyHashFromBytes
              -- $ T.Bech32 "hstk_1rsf0q0q77t5nttxrtmpwd7tvv58a80a686t92pgy65ekz0s8ncu"
              $ hexToByteArrayUnsafe "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"
        , paymentCred:
            keyHashCredential $ unsafePartial $ fromJust $ ed25519KeyHashFromBytes
              -- "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
              $ hexToByteArrayUnsafe "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
        }
    , amount: Value (Coin $ BigInt.fromInt 0) mempty
    , data_hash: Nothing
    }

txOutputFixture2 :: TransactionOutput
txOutputFixture2 =
  TransactionOutput
    { address: keyHashBaseAddress
        { stake: "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"
        , payment: "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
        }
    , amount: Value (Coin $ BigInt.fromInt 0) $
        mkSingletonNonAdaAsset currencySymbol1 tokenName1 (BigInt.fromInt 1000000)
    , data_hash: Nothing
    }

currencySymbol1 :: CurrencySymbol
currencySymbol1 = unsafePartial $ fromJust $ mkCurrencySymbol $
  hexToByteArrayUnsafe "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"

tokenName1 :: TokenName
tokenName1 = unsafePartial $ fromJust $ mkTokenName $
  hexToByteArrayUnsafe "4974657374546f6b656e"

txOutputBinaryFixture1 :: String
txOutputBinaryFixture1 =
  "8258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d5336100"

txFixture1 :: Transaction
txFixture1 =
  Transaction
    { body: TxBody
        { inputs: [ txInputFixture1 ]
        , outputs: [ txOutputFixture1 ]
        , fee: Coin $ BigInt.fromInt 177513
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
        , network_id: Just MainnetId
        }
    , witness_set: TransactionWitnessSet
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

txFixture2 :: Transaction
txFixture2 =
  Transaction
    { body: TxBody
        { inputs: [ txInputFixture1 ]
        , outputs: [ txOutputFixture2 ]
        , fee: Coin $ BigInt.fromInt 177513
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
        , network_id: Just MainnetId
        }
    , witness_set: TransactionWitnessSet
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

txFixture3 :: Transaction
txFixture3 =
  Transaction
    { body: TxBody
        { inputs: [ txInputFixture1 ]
        , outputs:
            [ TransactionOutput
                { address: keyHashBaseAddress
                    { stake: "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
                    -- $ T.Bech32 "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
                    , payment: "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
                    }
                , amount: Value (Coin $ BigInt.fromInt 2353402) mempty
                , data_hash: Nothing
                }
            , TransactionOutput
                { address: keyHashBaseAddress
                    { stake: "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
                    -- $ T.Bech32 "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
                    , payment: "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
                    }
                , amount: Value (Coin $ BigInt.fromInt 1000000) mempty
                , data_hash: Nothing
                }
            ]
        , fee: Coin $ BigInt.fromInt 177513
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
        , network_id: Just MainnetId
        }
    , witness_set: TransactionWitnessSet
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

-- | To quickly check a serialized tx, create a file with the following contents:
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
txBinaryFixture1 :: String
txBinaryFixture1 = "84a300818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960001818258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d5336100021a0002b569a0f5f6"

txBinaryFixture2 :: String
txBinaryFixture2 =
  "84a300818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960001818258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d533618200a1581c1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2ea14a4974657374546f6b656e1a000f4240021a0002b569a0f5f6"

txBinaryFixture3 :: String
txBinaryFixture3 = "84a300818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599960001828258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9710f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f975461a0023e8fa8258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9710f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f975461a000f4240021a0002b569a0f5f6"

utxoFixture1 :: ByteArray
utxoFixture1 = hexToByteArrayUnsafe "82825820c6b54aa301887af390bd3449833e4cd66ff61b5e68b1f77c84a8c0873b776ff90082583900f33ffa84fdf20a003443a5e2768e12e92db31535dca62088b153df243903103ae70681439b5476fef59f439b8bc86d84bfb2d376fc3f56171a004c4b40"

utxoFixture1' :: TransactionUnspentOutput
utxoFixture1' =
  TransactionUnspentOutput
    { input:
        ( TransactionInput
            { index: UInt.fromInt 0
            , transaction_id: TransactionHash (byteArrayFromIntArrayUnsafe [ 198, 181, 74, 163, 1, 136, 122, 243, 144, 189, 52, 73, 131, 62, 76, 214, 111, 246, 27, 94, 104, 177, 247, 124, 132, 168, 192, 135, 59, 119, 111, 249 ])
            }
        )
    , output:
        ( TransactionOutput
            { address: baseAddressToAddress $ baseAddress
                { network: TestnetId
                , paymentCred: keyHashCredential $ unsafePartial $ fromJust $ ed25519KeyHashFromBytes $
                    byteArrayFromIntArrayUnsafe [ 243, 63, 250, 132, 253, 242, 10, 0, 52, 67, 165, 226, 118, 142, 18, 233, 45, 179, 21, 53, 220, 166, 32, 136, 177, 83, 223, 36 ]
                , delegationCred: keyHashCredential $ unsafePartial $ fromJust $ ed25519KeyHashFromBytes $
                    (byteArrayFromIntArrayUnsafe [ 57, 3, 16, 58, 231, 6, 129, 67, 155, 84, 118, 254, 245, 159, 67, 155, 139, 200, 109, 132, 191, 178, 211, 118, 252, 63, 86, 23 ])
                }
            , amount: Value (Coin (BigInt.fromInt 5000000)) mempty
            , data_hash: Nothing
            }
        )
    }

witnessSetFixture1 :: ByteArray
witnessSetFixture1 = hexToByteArrayUnsafe "a40081825820096092b8515d75c2a2f75d6aa7c5191996755840e81deaa403dba5b690f091b6584089ed6f628b02ed3c79f1b3508e35ea772aea916e7c88010f34cc57ee619a9b6ec3cadd9bb4aeb14374121111db4f75fc8c8fc8772ba6b82b599743abf6fa3a0503815909b75909b401000033233223322323233322232333222323333333322222222323332223233332222323233223233322232333222323233223322323233333222223322332233223322332233222222323253353031333006375c00a6eb8010cccd5cd19b8735573aa004900011980499191919191919191919191999ab9a3370e6aae754029200023333333333017335025232323333573466e1cd55cea8012400046603a60706ae854008c0a8d5d09aba250022350573530583357389201035054310005949926135573ca00226ea8004d5d0a80519a8128131aba150093335502c75ca0566ae854020ccd540b1d728159aba1500733502504135742a00c66a04a66aa0a4094eb4d5d0a8029919191999ab9a3370e6aae7540092000233501f3232323333573466e1cd55cea80124000466a04e66a080eb4d5d0a80118229aba135744a00446a0b66a60b866ae712401035054310005d49926135573ca00226ea8004d5d0a8011919191999ab9a3370e6aae7540092000233502533504075a6ae854008c114d5d09aba2500223505b35305c3357389201035054310005d49926135573ca00226ea8004d5d09aba250022350573530583357389201035054310005949926135573ca00226ea8004d5d0a80219a812bae35742a00666a04a66aa0a4eb88004d5d0a801181b9aba135744a00446a0a66a60a866ae71241035054310005549926135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a8011919191999ab9a3370ea00290031180e181c9aba135573ca00646666ae68cdc3a801240084603660866ae84d55cf280211999ab9a3370ea00690011180d98171aba135573ca00a46666ae68cdc3a802240004603c6eb8d5d09aab9e500623504e35304f3357389201035054310005049926499264984d55cea80089baa001357426ae8940088d411cd4c120cd5ce2490350543100049499261048135046353047335738920103505435000484984d55cf280089baa0012212330010030022001222222222212333333333300100b00a00900800700600500400300220012212330010030022001122123300100300212001122123300100300212001122123300100300212001212222300400521222230030052122223002005212222300100520011232230023758002640026aa068446666aae7c004940388cd4034c010d5d080118019aba200203323232323333573466e1cd55cea801a4000466600e6464646666ae68cdc39aab9d5002480008cc034c0c4d5d0a80119a8098169aba135744a00446a06c6a606e66ae712401035054310003849926135573ca00226ea8004d5d0a801999aa805bae500a35742a00466a01eeb8d5d09aba25002235032353033335738921035054310003449926135744a00226aae7940044dd50009110919980080200180110009109198008018011000899aa800bae75a224464460046eac004c8004d540b888c8cccd55cf80112804919a80419aa81898031aab9d5002300535573ca00460086ae8800c0b84d5d08008891001091091198008020018900089119191999ab9a3370ea002900011a80418029aba135573ca00646666ae68cdc3a801240044a01046a0526a605466ae712401035054310002b499264984d55cea80089baa001121223002003112200112001232323333573466e1cd55cea8012400046600c600e6ae854008dd69aba135744a00446a0466a604866ae71241035054310002549926135573ca00226ea80048848cc00400c00880048c8cccd5cd19b8735573aa002900011bae357426aae7940088d407cd4c080cd5ce24810350543100021499261375400224464646666ae68cdc3a800a40084a00e46666ae68cdc3a8012400446a014600c6ae84d55cf280211999ab9a3370ea00690001280511a8111a981199ab9c490103505431000244992649926135573aa00226ea8004484888c00c0104488800844888004480048c8cccd5cd19b8750014800880188cccd5cd19b8750024800080188d4068d4c06ccd5ce249035054310001c499264984d55ce9baa0011220021220012001232323232323333573466e1d4005200c200b23333573466e1d4009200a200d23333573466e1d400d200823300b375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c46601a6eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc048c050d5d0a8049bae357426ae8940248cccd5cd19b875006480088c050c054d5d09aab9e500b23333573466e1d401d2000230133016357426aae7940308d407cd4c080cd5ce2481035054310002149926499264992649926135573aa00826aae79400c4d55cf280109aab9e500113754002424444444600e01044244444446600c012010424444444600a010244444440082444444400644244444446600401201044244444446600201201040024646464646666ae68cdc3a800a400446660106eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d400920002300a300b357426aae7940188d4040d4c044cd5ce2490350543100012499264984d55cea80189aba25001135573ca00226ea80048488c00800c888488ccc00401401000c80048c8c8cccd5cd19b875001480088c018dd71aba135573ca00646666ae68cdc3a80124000460106eb8d5d09aab9e500423500a35300b3357389201035054310000c499264984d55cea80089baa001212230020032122300100320011122232323333573466e1cd55cea80124000466aa016600c6ae854008c014d5d09aba25002235007353008335738921035054310000949926135573ca00226ea8004498480048004448848cc00400c008448004448c8c00400488cc00cc008008004cccc888ccc888cccccccc88888888cc88ccccc88888ccc888cccc8888cc88cc88cc88ccc888cc88cc88ccc888cc88cc88cc88cc8888894cd4c084ccd5cd19b8f00337240040460442046266ae71240109426164206775657373000222212330010030022001222222222212333333333300100b00a009008007006005004003002200122123300100300220012221233300100400300220011122002122122330010040031200111221233001003002112001221233001003002200112122300200311220011200112212330010030021200112212330010030021200112212330010030021200112122230030041122200211222001120011220021220012001212222300400521222230030052122223002005212222300100520012212330010030022001212222222300700822122222223300600900821222222230050081222222200412222222003221222222233002009008221222222233001009008200121223002003222122333001005004003200121223002003212230010032001480101048158202bb80d537b1da3e38bd30361aa855686bde0eacd7162fef6a25fe97bf527a25b058184000046736563726574821a00065cd41a0af4845c"

witnessSetFixture2 :: ByteArray
witnessSetFixture2 = hexToByteArrayUnsafe "a10081825820096092b8515d75c2a2f75d6aa7c5191996755840e81deaa403dba5b690f091b65840d8f41dd2dd9f76a75b7dcf9f237932b0b2145f2993ffb9e05cd247dd7789570681af34e98d7e4dd5346bf8fceb9170f5d7f9895723fc326a0db04f9273fcbb0a"

witnessSetFixture2Value :: TransactionWitnessSet
witnessSetFixture2Value =
  TransactionWitnessSet
    { bootstraps: Nothing
    , native_scripts: Nothing
    , plutus_data: Nothing
    , plutus_scripts: Nothing
    , redeemers: Nothing
    , vkeys: Just
        [ Vkeywitness
            ( (Vkey (PublicKey "ed25519_pk1p9sf9wz3t46u9ghht44203gerxt82kzqaqw74fqrmwjmdy8sjxmqknzq8j"))
                /\ (Ed25519Signature "ed25519_sig1mr6pm5kanam2wkmae70jx7fjkzepghefj0lmnczu6fra6auf2urgrte5axxhunw4x34l3l8tj9c0t4le39tj8lpjdgxmqnujw07tkzs9m6t6x")
            )
        ]
    }

witnessSetFixture3 :: ByteArray
witnessSetFixture3 = hexToByteArrayUnsafe "a20081825820096092b8515d75c2a2f75d6aa7c5191996755840e81deaa403dba5b690f091b65840c7f77418c5c956aab848b6f101d6bc014f22c699642531c8f39f91979c0313f686999402c4769117857c08fcceb57b60478a590ecae5190317f7abff7cd38000048158202bb80d537b1da3e38bd30361aa855686bde0eacd7162fef6a25fe97bf527a25b"

witnessSetFixture3Value :: TransactionWitnessSet
witnessSetFixture3Value =
  TransactionWitnessSet
    { bootstraps: Nothing
    , native_scripts: Nothing
    , plutus_data:
        Just [ PlutusData (byteArrayFromIntArrayUnsafe [ 88, 32, 43, 184, 13, 83, 123, 29, 163, 227, 139, 211, 3, 97, 170, 133, 86, 134, 189, 224, 234, 205, 113, 98, 254, 246, 162, 95, 233, 123, 245, 39, 162, 91 ]) ]
    , plutus_scripts: Nothing
    , redeemers: Nothing
    , vkeys: Just
        [ Vkeywitness
            ( (Vkey (PublicKey "ed25519_pk1p9sf9wz3t46u9ghht44203gerxt82kzqaqw74fqrmwjmdy8sjxmqknzq8j")) /\
                (Ed25519Signature "ed25519_sig1clmhgxx9e9t24wzgkmcsr44uq98j935evsjnrj8nn7ge08qrz0mgdxv5qtz8dyghs47q3lxwk4akq3u2ty8v4egeqvtl02ll0nfcqqq6faxl6")
            )
        ]
    }

witnessSetFixture4 :: ByteArray
witnessSetFixture4 = hexToByteArrayUnsafe "a30081825820096092b8515d75c2a2f75d6aa7c5191996755840e81deaa403dba5b690f091b658400d91f7ab723ed0adb9f7ec06bba5cb99b4dcbbe8fb6ce45fb3fcab31ddf57ca085437d7ec4e6fea8d10d0c455fdfb2fdbcf1d89643f635841da0e2593f6dd50a01818204187b048102"

addressString1 :: String
addressString1 = "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"

txInputFixture1 :: TransactionInput
txInputFixture1 =
  TransactionInput
    { transaction_id: TransactionHash $
        hexToByteArrayUnsafe "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
    , index: UInt.fromInt 0
    }

ed25519KeyHashFixture1 :: Ed25519KeyHash
ed25519KeyHashFixture1 =
  -- $ Bech32 "hstk_1rsf0q0q77t5nttxrtmpwd7tvv58a80a686t92pgy65ekz0s8ncu"
  unsafePartial $ fromJust
    $ ed25519KeyHashFromBytes
    $ hexToByteArrayUnsafe "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"

ed25519KeyHashFixture2 :: Ed25519KeyHash
ed25519KeyHashFixture2 =
  -- "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
  unsafePartial $ fromJust
    $ ed25519KeyHashFromBytes
    $ hexToByteArrayUnsafe "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"

nativeScriptFixture1 :: NativeScript
nativeScriptFixture1 = ScriptPubkey ed25519KeyHashFixture1

nativeScriptFixture2 :: NativeScript
nativeScriptFixture2 = ScriptPubkey ed25519KeyHashFixture2

nativeScriptFixture3 :: NativeScript
nativeScriptFixture3 = ScriptAll [ nativeScriptFixture1, nativeScriptFixture2 ]

nativeScriptFixture4 :: NativeScript
nativeScriptFixture4 = ScriptAny [ nativeScriptFixture1, nativeScriptFixture2 ]

nativeScriptFixture5 :: NativeScript
nativeScriptFixture5 = ScriptNOfK 1 [ nativeScriptFixture1, nativeScriptFixture2 ]

nativeScriptFixture6 :: NativeScript
nativeScriptFixture6 = TimelockStart $ Slot $ UInt.fromInt 1000

nativeScriptFixture7 :: NativeScript
nativeScriptFixture7 = TimelockExpiry $ Slot $ UInt.fromInt 2000

keyHashBaseAddress :: { payment :: String, stake :: String } -> Address
keyHashBaseAddress { payment, stake } = baseAddressToAddress $ baseAddress
  { network: TestnetId
  , delegationCred:
      keyHashCredential $ unsafePartial $ fromJust $ ed25519KeyHashFromBytes
        -- $ T.Bech32 "hstk_1rsf0q0q77t5nttxrtmpwd7tvv58a80a686t92pgy65ekz0s8ncu"
        $ hexToByteArrayUnsafe stake
  , paymentCred:
      keyHashCredential $ unsafePartial $ fromJust $ ed25519KeyHashFromBytes
        -- "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
        $ hexToByteArrayUnsafe payment
  }

plutusDataFixture1 :: PD.PlutusData
plutusDataFixture1 = PD.List []

plutusDataFixture2 :: PD.PlutusData
plutusDataFixture2 = PD.List [ plutusDataFixture1 ]

plutusDataFixture3 :: PD.PlutusData
plutusDataFixture3 = PD.Bytes (hexToByteArrayUnsafe "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971")

plutusDataFixture4 :: PD.PlutusData
plutusDataFixture4 = PD.Constr (BigInt.fromInt 1)
  [ plutusDataFixture2, plutusDataFixture3 ]

plutusDataFixture5 :: PD.PlutusData
plutusDataFixture5 = PD.Integer (BigInt.fromInt 42)

plutusDataFixture6 :: PD.PlutusData
plutusDataFixture6 = PD.Map $ Map.fromFoldable
  [ plutusDataFixture1 /\ plutusDataFixture2
  , plutusDataFixture3 /\ plutusDataFixture4
  ]

plutusDataFixture7 :: PD.PlutusData
plutusDataFixture7 = PD.List
  [ plutusDataFixture1
  , plutusDataFixture2
  , plutusDataFixture3
  , plutusDataFixture4
  , plutusDataFixture5
  , plutusDataFixture6
  ]
