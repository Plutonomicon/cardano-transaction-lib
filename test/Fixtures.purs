-- Feel free to update binary fixtures if they do not match the results you are
-- getting in tests. However, make sure you understand the reason why they
-- don't match.
-- To update the fixture, simply copy the value from failing test output.
--
-- Or construct a value using CSL and get the hex string:
--
-- ```
-- const byteArrayToHex = arr => Buffer.from(arr).toString('hex');
-- console.log(byteArrayToHex(something.to_bytes()))
-- ```
module Test.Ctl.Fixtures
  ( addressString1
  , currencySymbol1
  , ed25519KeyHash1
  , ed25519KeyHashFixture1
  , fullyAppliedScriptFixture
  , int1
  , mint1
  , mkSampleTx
  , mkTxInput
  , nativeScriptFixture1
  , nativeScriptFixture2
  , nativeScriptFixture3
  , nativeScriptFixture4
  , nativeScriptFixture5
  , nativeScriptFixture6
  , nativeScriptFixture7
  , nullPaymentPubKeyHash
  , ogmiosEvaluateTxFailIncompatibleEraFixture
  , ogmiosEvaluateTxFailScriptErrorsFixture
  , ogmiosEvaluateTxInvalidPointerFormatFixture
  , ogmiosEvaluateTxValidRespFixture
  , partiallyAppliedScriptFixture
  , plutusDataFixture1
  , plutusDataFixture2
  , plutusDataFixture3
  , plutusDataFixture4
  , plutusDataFixture5
  , plutusDataFixture6
  , plutusDataFixture7
  , plutusDataFixture8
  , plutusDataFixture8Bytes
  , plutusDataFixture8Bytes'
  , plutusScriptFixture1
  , plutusScriptFixture2
  , plutusScriptFixture3
  , redeemerFixture1
  , tokenName1
  , tokenName2
  , txBinaryFixture1
  , txBinaryFixture2
  , txBinaryFixture3
  , txBinaryFixture4
  , txBinaryFixture5
  , txBinaryFixture6
  , txFixture1
  , txInputFixture1
  , txOutputBinaryFixture1
  , txOutputFixture1
  , txOutputFixture2
  , unappliedScriptFixture
  , utxoFixture1
  , utxoFixture1'
  , utxoMapFixture
  , witnessSetFixture1
  , witnessSetFixture2
  , witnessSetFixture2Value
  , witnessSetFixture3
  , witnessSetFixture3Value
  , witnessSetFixture4
  , txMetadatumFixture
  ) where

import Prelude

import Aeson (Aeson, aesonNull, decodeAeson, fromString, parseJsonStringToAeson)
import Cardano.AsCbor (decodeCbor)
import Cardano.Types
  ( Bech32String
  , Coin(Coin)
  , Credential(PubKeyHashCredential)
  , Ed25519KeyHash
  , ExUnits(ExUnits)
  , Language(PlutusV2)
  , NativeScript
      ( TimelockExpiry
      , TimelockStart
      , ScriptNOfK
      , ScriptAny
      , ScriptAll
      , ScriptPubkey
      )
  , NetworkId(TestnetId, MainnetId)
  , PaymentPubKeyHash(PaymentPubKeyHash)
  , PlutusData(Integer, Bytes, Constr, List, Map)
  , PlutusScript(PlutusScript)
  , Redeemer(Redeemer)
  , RedeemerTag(Spend)
  , ScriptHash
  , Slot(Slot)
  , Transaction(Transaction)
  , TransactionBody(TransactionBody)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TransactionUnspentOutput(TransactionUnspentOutput)
  , TransactionWitnessSet(TransactionWitnessSet)
  , UtxoMap
  , Value(Value)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  )
import Cardano.Types.Address (Address(BaseAddress))
import Cardano.Types.AssetName (AssetName, mkAssetName)
import Cardano.Types.Ed25519KeyHash as Ed25519KeyHash
import Cardano.Types.Ed25519Signature as Ed25519Signature
import Cardano.Types.Int as Int
import Cardano.Types.Mint (Mint(Mint))
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.PlutusScript (plutusV1Script, plutusV2Script)
import Cardano.Types.PublicKey as PublicKey
import Contract.Numeric.BigNum (fromInt, one, zero) as BigNum
import Data.ByteArray
  ( ByteArray
  , byteArrayFromIntArrayUnsafe
  , hexToByteArrayUnsafe
  )
import Data.Either (fromRight, hush)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import JS.BigInt as BigInt
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

txOutputFixture1 :: TransactionOutput
txOutputFixture1 =
  TransactionOutput
    { address: BaseAddress
        { networkId: TestnetId
        , stakeCredential:
            wrap $ PubKeyHashCredential $ unsafePartial $ fromJust
              $ decodeCbor
              $ wrap
              -- $ T.Bech32 "hstk_1rsf0q0q77t5nttxrtmpwd7tvv58a80a686t92pgy65ekz0s8ncu"
              $ hexToByteArrayUnsafe
                  "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"
        , paymentCredential:
            wrap $ PubKeyHashCredential $ unsafePartial $ fromJust
              $ decodeCbor
              $ wrap
              -- "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
              $ hexToByteArrayUnsafe
                  "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
        }
    , amount: Value (Coin $ BigNum.fromInt 0) MultiAsset.empty
    , datum: Nothing
    , scriptRef: Nothing
    }

txOutputFixture2 :: TransactionOutput
txOutputFixture2 =
  TransactionOutput
    { address: keyHashBaseAddress
        { stake: "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"
        , payment: "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
        }
    , amount: Value (Coin $ BigNum.fromInt 0) $
        MultiAsset.singleton currencySymbol1 tokenName1
          (BigNum.fromInt 1000000)
    , datum: Nothing
    , scriptRef: Nothing
    }

currencySymbol1 :: ScriptHash
currencySymbol1 = unsafePartial $ fromJust $ decodeCbor $ wrap $
  hexToByteArrayUnsafe
    "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"

tokenNameFromString :: String -> AssetName
tokenNameFromString s = unsafePartial $ fromJust $ mkAssetName $
  hexToByteArrayUnsafe s

tokenName1 :: AssetName
tokenName1 = tokenNameFromString "4974657374546f6b656e"

tokenName2 :: AssetName
tokenName2 = tokenNameFromString "54657374546f6b656e32"

txOutputBinaryFixture1 :: String
txOutputBinaryFixture1 =
  "8258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2\
  \e935acc35ec2e6f96c650fd3bfba3e96550504d5336100"

pkhBech32 :: Bech32String
pkhBech32 = "addr_vkh1zuctrdcq6ctd29242w8g84nlz0q38t2lnv3zzfcrfqktx0c9tzp"

ed25519KeyHash1 :: Ed25519KeyHash
ed25519KeyHash1 = unsafePartial $ fromJust $ Ed25519KeyHash.fromBech32 pkhBech32

-- | Extend this for your needs.
type SampleTxConfig =
  { inputs :: Array TransactionInput }

-- | Build a sample transaction using convenient config
-- | and existing one as a base.
mkSampleTx
  :: Transaction
  -> (SampleTxConfig -> SampleTxConfig)
  -> Transaction
mkSampleTx startTx changes =
  applyChanges startTx $ buildChanges startTx changes

  where
  buildChanges
    :: Transaction -> (SampleTxConfig -> SampleTxConfig) -> SampleTxConfig
  buildChanges (Transaction { body: TransactionBody { inputs } }) mkChanges =
    mkChanges { inputs }

  applyChanges :: Transaction -> SampleTxConfig -> Transaction
  applyChanges
    ( Transaction
        { body: TransactionBody
            { outputs
            , fee
            , ttl
            , certs
            , withdrawals
            , auxiliaryDataHash
            , validityStartInterval
            , mint
            , referenceInputs
            , scriptDataHash
            , collateral
            , requiredSigners
            , networkId
            , collateralReturn
            , totalCollateral
            , votingProposals
            , votingProcedures
            , currentTreasuryValue
            , donation
            }
        , witnessSet
        , isValid
        , auxiliaryData
        }
    )
    { inputs: newInputs } =
    ( Transaction
        { body: TransactionBody
            { inputs: newInputs
            , outputs
            , fee
            , ttl
            , certs
            , withdrawals
            , auxiliaryDataHash
            , validityStartInterval
            , mint
            , referenceInputs
            , scriptDataHash
            , collateral
            , requiredSigners
            , networkId
            , collateralReturn
            , totalCollateral
            , votingProposals
            , votingProcedures
            , currentTreasuryValue
            , donation
            }
        , witnessSet
        , isValid
        , auxiliaryData
        }
    )

-- Always succeeds
plutusScriptFixture1 :: PlutusScript
plutusScriptFixture1 = unsafePartial $ fromJust $ map plutusV1Script $ hush
  $ decodeAeson
  $ fromString "4d01000033222220051200120011"

plutusScriptFixture2 :: PlutusScript
plutusScriptFixture2 = unsafePartial $ fromJust $ map plutusV2Script $ hush
  $ decodeAeson
  $ fromString "4d010000deadbeef33222220051200120011"

txMetadatumFixture :: ByteArray
txMetadatumFixture = hexToByteArrayUnsafe "60" -- "a11902d1a278383733336136646663363063613639396261616236346336353836663464633138653263613733343232363730333439666361396235346339a174537061636550756773476c617373504334393730ab646e616d65781b5370616365205075677320476c617373202d20504320233439373065696d6167657835697066733a2f2f516d516f4d61554e3239554278776154515373566b48783748324d57627365794e6d4c78386f745a7a5167395a34696d656469615479706569696d6167652f6769666b6465736372697074696f6e606566696c657381a3646e616d6574537061636550756773476c617373504334393730696d656469615479706569766964656f2f6d7034637372637835697066733a2f2f516d57646b7231634b42345065764739784a4b76536e71336d33354a5478726771673158725742475062456f62696a4261636b67726f756e646b507572706c652046656c7463546f706e517565656e204f6620436c75627366426f74746f6d754a61636b204f662048656172747320426f74746f6d6644657369676e75323920284d616420446f672043617220436c756229664578747261737157696e73746f6e20436875726368696c6c684c69676874696e676c4e6f726d616c204c696768746776657273696f6e63312e30"

plutusScriptFixture3 :: PlutusScript
plutusScriptFixture3 =
  ( PlutusScript
      ( Tuple
          ( hexToByteArrayUnsafe
              "59088501000032332232323233223232323232323232323322323232323232322223232533532325335001101b13357389211d556e657870656374656420646174756d206174206f776e20696e7075740001a323253335002153335001101c2101c2101c2153335002101c21333573466ebc00800407807484074854ccd400840708407484ccd5cd19b8f00200101e01d323500122220023235001220013553353500222350022222222222223333500d2501e2501e2501e233355302d12001321233001225335002210031001002501e2350012253355335333573466e3cd400888008d4010880080b40b04ccd5cd19b873500222001350042200102d02c102c1350220031502100d21123001002162001300a0053333573466e1cd55cea80124000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4054058d5d0a80619a80a80b1aba1500b33501501735742a014666aa034eb94064d5d0a804999aa80d3ae501935742a01066a02a0426ae85401cccd54068089d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40b1d69aba15002302d357426ae8940088c98c80bccd5ce01901881689aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8163ad35742a004605a6ae84d5d1280111931901799ab9c03203102d135573ca00226ea8004d5d09aba2500223263202b33573805c05a05226aae7940044dd50009aba1500533501575c6ae854010ccd540680788004d5d0a801999aa80d3ae200135742a00460406ae84d5d1280111931901399ab9c02a029025135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00460206ae84d5d1280111931900c99ab9c01c01b017101a132632018335738921035054350001a135573ca00226ea800448c88c008dd6000990009aa80d111999aab9f0012501c233501b30043574200460066ae8800805c8c8c8cccd5cd19b8735573aa004900011991091980080180118069aba150023005357426ae8940088c98c8054cd5ce00c00b80989aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180b1aba1500233500e015357426ae8940088c98c8068cd5ce00e80e00c09aab9e5001137540026ae854010ccd54025d728041aba150033232323333573466e1d400520042300b357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403866ae7007c0780680640604d55cea80089baa00135742a00466a014eb8d5d09aba2500223263201633573803203002826ae8940044d5d1280089aab9e500113754002424446004008266aa002eb9d6889119118011bab00132001355016223233335573e0044a032466a03066442466002006004600c6aae754008c014d55cf280118021aba200301413574200224464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900919ab9c01501401000f135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900819ab9c01301200e00d00c00b135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900619ab9c00f00e00a135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00680600409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00b00a80880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae7003c0380280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801801600e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7003403002001c0184d55cea80089baa0012323333573466e1d40052002200623333573466e1d40092000200623263200633573801201000800626aae74dd5000a4c24400424400224002920103505431003200135500322112225335001135003220012213335005220023004002333553007120010050040011122002122122330010040031123230010012233003300200200101"
          )
          PlutusV2
      )
  )

txFixture1 :: Transaction
txFixture1 =
  Transaction
    { body: TransactionBody
        { inputs: [ txInputFixture1 ]
        , outputs: [ txOutputFixture1 ]
        , fee: Coin $ BigNum.fromInt 177513
        , ttl: Nothing
        , certs: []
        , withdrawals: Map.empty
        , auxiliaryDataHash: Nothing
        , validityStartInterval: Nothing
        , mint: Nothing
        , referenceInputs: mempty
        , scriptDataHash: Nothing
        , collateral: []
        , requiredSigners: []
        , networkId: Just MainnetId
        , collateralReturn: Nothing
        , totalCollateral: Nothing
        , votingProposals: []
        , votingProcedures: mempty
        , currentTreasuryValue: Nothing
        , donation: Nothing
        }
    , witnessSet: TransactionWitnessSet
        { vkeys: []
        , nativeScripts: []
        , bootstraps: []
        , plutusScripts: []
        , plutusData: []
        , redeemers: []
        }
    , isValid: true
    , auxiliaryData: mempty
    }

mint1 :: Mint
mint1 = Mint $ Map.fromFoldable
  [ currencySymbol1 /\
      Map.fromFoldable
        [ tokenName2 /\ Int.newPositive BigNum.one ]
  ]

int1 :: Int.Int
int1 = Int.newPositive BigNum.one

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
txBinaryFixture1 =
  "84a400818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599\
  \960001818258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12\
  \f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d5336100021a0002b5690f01\
  \a0f5f6"

txBinaryFixture2 :: String
txBinaryFixture2 =
  "84a400818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599\
  \960001818258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12\
  \f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d533618200a1581c1d6445ddeda578\
  \117f393848e685128f1e78ad0c4e48129c5964dc2ea14a4974657374546f6b656e1a000f4240\
  \021a0002b5690f01a0f5f6"

txBinaryFixture3 :: String
txBinaryFixture3 =
  "84a500818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599\
  \960001828258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9710f45\
  \aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f975461a0023e8fa8258390030fb3b\
  \8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9710f45aaf1b2959db6e5ff94dbb1\
  \f823bf257680c3c723ac2d49f975461a000f4240021a0002b5690f0112818258205d677265fa\
  \5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad95999600a0f5f6"

txBinaryFixture4 :: String
txBinaryFixture4 =
  "84ae00818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599\
  \96000182a40058390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9710f\
  \45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546011a0023e8fa028201d818\
  \418003d8185182014e4d01000033222220051200120011a30058390030fb3b8539951e26f034\
  \910a5a37f22cb99d94d1d409f69ddbaea9710f45aaf1b2959db6e5ff94dbb1f823bf257680c3\
  \c723ac2d49f97546011a000f424003d81858468200830301828200581c1c12f03c1ef2e935ac\
  \c35ec2e6f96c650fd3bfba3e96550504d533618200581c30fb3b8539951e26f034910a5a37f2\
  \2cb99d94d1d409f69ddbaea971021a0002b56903187b048882008200581c1730b1b700d616d5\
  \1555538e83d67f13c113ad5f9b22212703482cb382018200581c1730b1b700d616d51555538e\
  \83d67f13c113ad5f9b22212703482cb383028200581c1730b1b700d616d51555538e83d67f13\
  \c113ad5f9b22212703482cb3581c1730b1b700d616d51555538e83d67f13c113ad5f9b222127\
  \03482cb38a03581c1730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb35820\
  \fbf6d41985670b9041c5bf362b5262cf34add5d265975de176d613ca05f370960101d81e8201\
  \01581de11730b1b700d616d51555538e83d67f13c113ad5f9b22212703482cb381581c1730b1\
  \b700d616d51555538e83d67f13c113ad5f9b22212703482cb3838400191f90447f000001507b\
  \7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b8301191f906b6578616d706c652e636f6d82026b657861\
  \6d706c652e636f6d827468747470733a2f2f6578616d706c652e636f6d2f582094b8cac47761\
  \c1140c57a48d56ab15d27a842abff041b3798b8618fa84641f5a8304581c1730b1b700d616d5\
  \1555538e83d67f13c113ad5f9b22212703482cb3018405581c5d677265fa5bb21ce6d8c7502a\
  \ca70b9316d10e958611f3c6b758f65581c5d677265fa5bb21ce6d8c7502aca70b9316d10e958\
  \611f3c6b758f655820fbf6d41985670b9041c5bf362b5262cf34add5d265975de176d613ca05\
  \f37096820682010182068201a18200581c1730b1b700d616d51555538e83d67f13c113ad5f9b\
  \22212703482cb30105a1581de01730b1b700d616d51555538e83d67f13c113ad5f9b22212703\
  \482cb3010682a1581c5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65b6\
  \000101010219271003192710041903e8050106010701080109d81e8201010ad81e8201010bd8\
  \1e8201010e8201011001110112a20098a61a0003236119032c01011903e819023b00011903e8\
  \195e7104011903e818201a0001ca761928eb041959d818641959d818641959d818641959d818\
  \641959d818641959d81864186418641959d81864194c5118201a0002acfa182019b551041a00\
  \0363151901ff00011a00015c3518201a000797751936f404021a0002ff941a0006ea7818dc00\
  \01011903e8196ff604021a0003bd081a00034ec5183e011a00102e0f19312a011a00032e8019\
  \01a5011a0002da781903e819cf06011a00013a34182019a8f118201903e818201a00013aac01\
  \19e143041903e80a1a00030219189c011a00030219189c011a0003207c1901d9011a00033000\
  \1901ff0119ccf3182019fd40182019ffd5182019581e18201940b318201a00012adf18201a00\
  \02ff941a0006ea7818dc0001011a00010f92192da7000119eabb18201a0002ff941a0006ea78\
  \18dc0001011a0002ff941a0006ea7818dc0001011a000c504e197712041a001d6af61a000142\
  \5b041a00040c660004001a00014fab18201a0003236119032c010119a0de18201a00033d7618\
  \201979f41820197fb8182019a95d1820197df718201995aa18201a0374f693194a1f0a0198af\
  \1a0003236119032c01011903e819023b00011903e8195e7104011903e818201a0001ca761928\
  \eb041959d818641959d818641959d818641959d818641959d818641959d81864186418641959\
  \d81864194c5118201a0002acfa182019b551041a000363151901ff00011a00015c3518201a00\
  \0797751936f404021a0002ff941a0006ea7818dc0001011903e8196ff604021a0003bd081a00\
  \034ec5183e011a00102e0f19312a011a00032e801901a5011a0002da781903e819cf06011a00\
  \013a34182019a8f118201903e818201a00013aac0119e143041903e80a1a00030219189c011a\
  \00030219189c011a0003207c1901d9011a000330001901ff0119ccf3182019fd40182019ffd5\
  \182019581e18201940b318201a00012adf18201a0002ff941a0006ea7818dc0001011a00010f\
  \92192da7000119eabb18201a0002ff941a0006ea7818dc0001011a0002ff941a0006ea7818dc\
  \0001011a0011b22c1a0005fdde00021a000c504e197712041a001d6af61a0001425b041a0004\
  \0c660004001a00014fab18201a0003236119032c010119a0de18201a00033d7618201979f418\
  \20197fb8182019a95d1820197df718201995aa18201a0223accc0a1a0374f693194a1f0a1a02\
  \515e841980b30a1382d81e820101d81e8201011482010115820101160117188c18180a010758\
  \20000000000000000000000000000000000000000000000000000000000000000008187c09a1\
  \581c1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2ea14a497465737454\
  \6f6b656e010e81581c1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d533610f\
  \01108258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c\
  \1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d5336100111a004c4b40a0f5f6"

txBinaryFixture5 :: String
txBinaryFixture5 =
  "84a400818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599\
  \96000181a40058390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9710f\
  \45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546011a1d3860f2028201d818\
  \418003d818558202524d010000deadbeef33222220051200120011021a05557fac0f01a0f5f6"

txBinaryFixture6 :: String
txBinaryFixture6 =
  "84a400818258205d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad9599\
  \960001818258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12\
  \f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d5336100021a0002b5690f01a0f5a1\
  \0863666f6f"

utxoFixture1 :: ByteArray
utxoFixture1 = hexToByteArrayUnsafe
  "82825820c6b54aa301887af390bd3449833e4cd66ff61b5e68b1f77c84a8c0873b776ff90082\
  \583900f33ffa84fdf20a003443a5e2768e12e92db31535dca62088b153df243903103ae70681\
  \439b5476fef59f439b8bc86d84bfb2d376fc3f56171a004c4b40"

input :: TransactionInput
input = TransactionInput
  { index: UInt.fromInt 0
  , transactionId: unsafePartial $ fromJust $ decodeCbor $ wrap
      ( byteArrayFromIntArrayUnsafe
          [ 198
          , 181
          , 74
          , 163
          , 1
          , 136
          , 122
          , 243
          , 144
          , 189
          , 52
          , 73
          , 131
          , 62
          , 76
          , 214
          , 111
          , 246
          , 27
          , 94
          , 104
          , 177
          , 247
          , 124
          , 132
          , 168
          , 192
          , 135
          , 59
          , 119
          , 111
          , 249
          ]
      )
  }

output :: TransactionOutput
output =
  ( TransactionOutput
      { address: BaseAddress
          { networkId: TestnetId
          , paymentCredential: wrap $ PubKeyHashCredential $ unsafePartial
              $ fromJust
              $ decodeCbor
              $ wrap
              $
                byteArrayFromIntArrayUnsafe
                  [ 243
                  , 63
                  , 250
                  , 132
                  , 253
                  , 242
                  , 10
                  , 0
                  , 52
                  , 67
                  , 165
                  , 226
                  , 118
                  , 142
                  , 18
                  , 233
                  , 45
                  , 179
                  , 21
                  , 53
                  , 220
                  , 166
                  , 32
                  , 136
                  , 177
                  , 83
                  , 223
                  , 36
                  ]
          , stakeCredential: wrap $ PubKeyHashCredential $ unsafePartial
              $ fromJust
              $ decodeCbor
              $ wrap
                  ( byteArrayFromIntArrayUnsafe
                      [ 57
                      , 3
                      , 16
                      , 58
                      , 231
                      , 6
                      , 129
                      , 67
                      , 155
                      , 84
                      , 118
                      , 254
                      , 245
                      , 159
                      , 67
                      , 155
                      , 139
                      , 200
                      , 109
                      , 132
                      , 191
                      , 178
                      , 211
                      , 118
                      , 252
                      , 63
                      , 86
                      , 23
                      ]
                  )
          }
      , amount: Value (Coin (BigNum.fromInt 5000000)) MultiAsset.empty
      , datum: Nothing
      , scriptRef: Nothing
      }
  )

utxoMapFixture :: UtxoMap
utxoMapFixture = Map.singleton input output

utxoFixture1' :: TransactionUnspentOutput
utxoFixture1' =
  TransactionUnspentOutput
    { input
    , output
    }

witnessSetFixture1 :: ByteArray
witnessSetFixture1 = hexToByteArrayUnsafe
  "a40081825820096092b8515d75c2a2f75d6aa7c5191996755840e81deaa403dba5b690f091b6\
  \584089ed6f628b02ed3c79f1b3508e35ea772aea916e7c88010f34cc57ee619a9b6ec3cadd9b\
  \b4aeb14374121111db4f75fc8c8fc8772ba6b82b599743abf6fa3a0503815909b75909b40100\
  \0033233223322323233322232333222323333333322222222323332223233332222323233223\
  \2333222323332223232332233223232333332222233223322332233223322332222223232533\
  \53031333006375c00a6eb8010cccd5cd19b8735573aa00490001198049919191919191919191\
  \9191999ab9a3370e6aae754029200023333333333017335025232323333573466e1cd55cea80\
  \12400046603a60706ae854008c0a8d5d09aba250022350573530583357389201035054310005\
  \949926135573ca00226ea8004d5d0a80519a8128131aba150093335502c75ca0566ae854020c\
  \cd540b1d728159aba1500733502504135742a00c66a04a66aa0a4094eb4d5d0a802991919199\
  \9ab9a3370e6aae7540092000233501f3232323333573466e1cd55cea80124000466a04e66a08\
  \0eb4d5d0a80118229aba135744a00446a0b66a60b866ae712401035054310005d49926135573\
  \ca00226ea8004d5d0a8011919191999ab9a3370e6aae7540092000233502533504075a6ae854\
  \008c114d5d09aba2500223505b35305c3357389201035054310005d49926135573ca00226ea8\
  \004d5d09aba250022350573530583357389201035054310005949926135573ca00226ea8004d\
  \5d0a80219a812bae35742a00666a04a66aa0a4eb88004d5d0a801181b9aba135744a00446a0a\
  \66a60a866ae71241035054310005549926135744a00226ae8940044d5d1280089aba25001135\
  \744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a8011919191999ab9\
  \a3370ea00290031180e181c9aba135573ca00646666ae68cdc3a801240084603660866ae84d5\
  \5cf280211999ab9a3370ea00690011180d98171aba135573ca00a46666ae68cdc3a802240004\
  \603c6eb8d5d09aab9e500623504e35304f3357389201035054310005049926499264984d55ce\
  \a80089baa001357426ae8940088d411cd4c120cd5ce249035054310004949926104813504635\
  \3047335738920103505435000484984d55cf280089baa0012212330010030022001222222222\
  \212333333333300100b00a009008007006005004003002200122123300100300220011221233\
  \0010030021200112212330010030021200112212330010030021200121222230040052122223\
  \0030052122223002005212222300100520011232230023758002640026aa068446666aae7c00\
  \4940388cd4034c010d5d080118019aba200203323232323333573466e1cd55cea801a4000466\
  \600e6464646666ae68cdc39aab9d5002480008cc034c0c4d5d0a80119a8098169aba135744a0\
  \0446a06c6a606e66ae712401035054310003849926135573ca00226ea8004d5d0a801999aa80\
  \5bae500a35742a00466a01eeb8d5d09aba250022350323530333357389210350543100034499\
  \26135744a00226aae7940044dd50009110919980080200180110009109198008018011000899\
  \aa800bae75a224464460046eac004c8004d540b888c8cccd55cf80112804919a80419aa81898\
  \031aab9d5002300535573ca00460086ae8800c0b84d5d0800889100109109119800802001890\
  \0089119191999ab9a3370ea002900011a80418029aba135573ca00646666ae68cdc3a8012400\
  \44a01046a0526a605466ae712401035054310002b499264984d55cea80089baa001121223002\
  \003112200112001232323333573466e1cd55cea8012400046600c600e6ae854008dd69aba135\
  \744a00446a0466a604866ae71241035054310002549926135573ca00226ea80048848cc00400\
  \c00880048c8cccd5cd19b8735573aa002900011bae357426aae7940088d407cd4c080cd5ce24\
  \810350543100021499261375400224464646666ae68cdc3a800a40084a00e46666ae68cdc3a8\
  \012400446a014600c6ae84d55cf280211999ab9a3370ea00690001280511a8111a981199ab9c\
  \490103505431000244992649926135573aa00226ea8004484888c00c01044888008448880044\
  \80048c8cccd5cd19b8750014800880188cccd5cd19b8750024800080188d4068d4c06ccd5ce2\
  \49035054310001c499264984d55ce9baa0011220021220012001232323232323333573466e1d\
  \4005200c200b23333573466e1d4009200a200d23333573466e1d400d200823300b375c6ae854\
  \014dd69aba135744a00a46666ae68cdc3a8022400c46601a6eb8d5d0a8039bae357426ae8940\
  \1c8cccd5cd19b875005480108cc048c050d5d0a8049bae357426ae8940248cccd5cd19b87500\
  \6480088c050c054d5d09aab9e500b23333573466e1d401d2000230133016357426aae7940308\
  \d407cd4c080cd5ce2481035054310002149926499264992649926135573aa00826aae79400c4\
  \d55cf280109aab9e500113754002424444444600e01044244444446600c01201042444444460\
  \0a01024444444008244444440064424444444660040120104424444444660020120104002464\
  \6464646666ae68cdc3a800a400446660106eb4d5d0a8021bad35742a0066eb4d5d09aba25003\
  \23333573466e1d400920002300a300b357426aae7940188d4040d4c044cd5ce2490350543100\
  \012499264984d55cea80189aba25001135573ca00226ea80048488c00800c888488ccc004014\
  \01000c80048c8c8cccd5cd19b875001480088c018dd71aba135573ca00646666ae68cdc3a801\
  \24000460106eb8d5d09aab9e500423500a35300b3357389201035054310000c499264984d55c\
  \ea80089baa001212230020032122300100320011122232323333573466e1cd55cea801240004\
  \66aa016600c6ae854008c014d5d09aba25002235007353008335738921035054310000949926\
  \135573ca00226ea8004498480048004448848cc00400c008448004448c8c00400488cc00cc00\
  \8008004cccc888ccc888cccccccc88888888cc88ccccc88888ccc888cccc8888cc88cc88cc88\
  \ccc888cc88cc88ccc888cc88cc88cc88cc8888894cd4c084ccd5cd19b8f00337240040460442\
  \046266ae71240109426164206775657373000222212330010030022001222222222212333333\
  \333300100b00a009008007006005004003002200122123300100300220012221233300100400\
  \3002200111220021221223300100400312001112212330010030021120012212330010030022\
  \0011212230020031122001120011221233001003002120011221233001003002120011221233\
  \0010030021200112122230030041122200211222001120011220021220012001212222300400\
  \5212222300300521222230020052122223001005200122123300100300220012122222223007\
  \0082212222222330060090082122222223005008122222220041222222200322122222223300\
  \2009008221222222233001009008200121223002003222122333001005004003200121223002\
  \003212230010032001480101049f58202bb80d537b1da3e38bd30361aa855686bde0eacd7162\
  \fef6a25fe97bf527a25bff058184000046736563726574821a00065cd41a0af4845c"

witnessSetFixture2 :: ByteArray
witnessSetFixture2 = hexToByteArrayUnsafe
  "a10081825820096092b8515d75c2a2f75d6aa7c5191996755840e81deaa403dba5b690f091b6\
  \5840d8f41dd2dd9f76a75b7dcf9f237932b0b2145f2993ffb9e05cd247dd7789570681af34e9\
  \8d7e4dd5346bf8fceb9170f5d7f9895723fc326a0db04f9273fcbb0a"

witnessSetFixture2Value :: TransactionWitnessSet
witnessSetFixture2Value =
  TransactionWitnessSet
    { bootstraps: []
    , nativeScripts: []
    , plutusData: []
    , plutusScripts: []
    , redeemers: []
    , vkeys:
        [ Vkeywitness
            { vkey: Vkey
                ( unsafePartial $ fromJust $ PublicKey.fromBech32
                    "ed25519_pk1p9sf9wz3t46u9ghht44203gerxt82kzqaqw74fqrmwjmdy8sjxmqknzq8j"
                )
            , signature:
                ( unsafePartial $ fromJust <<< Ed25519Signature.fromBech32 $
                    "ed25519_sig1mr6pm5kanam2wkmae70jx7fjkzepghefj0lmnczu6fra\
                    \6auf2urgrte5axxhunw4x34l3l8tj9c0t4le39tj8lpjdgxmqnujw07t\
                    \kzs9m6t6x"
                )
            }
        ]
    }

witnessSetFixture3 :: ByteArray
witnessSetFixture3 = hexToByteArrayUnsafe
  "a20081825820096092b8515d75c2a2f75d6aa7c5191996755840e81deaa403dba5b690f091b6\
  \5840c7f77418c5c956aab848b6f101d6bc014f22c699642531c8f39f91979c0313f686999402\
  \c4769117857c08fcceb57b60478a590ecae5190317f7abff7cd38000049f58202bb80d537b1d\
  \a3e38bd30361aa855686bde0eacd7162fef6a25fe97bf527a25bff"

witnessSetFixture3Value :: TransactionWitnessSet
witnessSetFixture3Value =
  TransactionWitnessSet
    { bootstraps: []
    , nativeScripts: []
    , plutusData:
        [ Bytes
            ( byteArrayFromIntArrayUnsafe
                [ 43
                , 184
                , 13
                , 83
                , 123
                , 29
                , 163
                , 227
                , 139
                , 211
                , 3
                , 97
                , 170
                , 133
                , 86
                , 134
                , 189
                , 224
                , 234
                , 205
                , 113
                , 98
                , 254
                , 246
                , 162
                , 95
                , 233
                , 123
                , 245
                , 39
                , 162
                , 91
                ]
            )
        ]
    , plutusScripts: [ plutusScriptFixture3 ]
    , redeemers: []
    , vkeys:
        [ Vkeywitness
            { vkey: Vkey
                ( unsafePartial $ fromJust $ PublicKey.fromBech32
                    "ed25519_pk1p9sf9wz3t46u9ghht44203gerxt82kzqaqw74fqrmwjmdy8sjxmqknzq8j"
                )
            , signature:
                ( unsafePartial $ fromJust <<< Ed25519Signature.fromBech32 $
                    "ed25519_sig1clmhgxx9e9t24wzgkmcsr44uq98j935evsjnrj8nn7ge08\
                    \qrz0mgdxv5qtz8dyghs47q3lxwk4akq3u2ty8v4egeqvtl02ll0nfcqqq\
                    \6faxl6"
                )
            }
        ]
    }

witnessSetFixture4 :: ByteArray
witnessSetFixture4 = hexToByteArrayUnsafe
  "a30081825820096092b8515d75c2a2f75d6aa7c5191996755840e81deaa403dba5b690f091b6\
  \58400d91f7ab723ed0adb9f7ec06bba5cb99b4dcbbe8fb6ce45fb3fcab31ddf57ca085437d7e\
  \c4e6fea8d10d0c455fdfb2fdbcf1d89643f635841da0e2593f6dd50a01818204187b049f02ff"

addressString1 :: String
addressString1 =
  "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr\
  \9plfmlw37je2s2px4xdssgvxerq"

mkTxInput :: { txId :: String, ix :: Int } -> TransactionInput
mkTxInput { txId, ix } =
  TransactionInput
    { transactionId: unsafePartial $ fromJust $ decodeCbor $ wrap $
        hexToByteArrayUnsafe txId
    , index: UInt.fromInt ix
    }

txInputFixture1 :: TransactionInput
txInputFixture1 = mkTxInput
  { txId: "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
  , ix: 0
  }

ed25519KeyHashFixture1 :: Ed25519KeyHash
ed25519KeyHashFixture1 =
  -- $ Bech32 "hstk_1rsf0q0q77t5nttxrtmpwd7tvv58a80a686t92pgy65ekz0s8ncu"
  unsafePartial $ fromJust
    $ decodeCbor
    $ wrap
    $ hexToByteArrayUnsafe
        "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"

ed25519KeyHashFixture2 :: Ed25519KeyHash
ed25519KeyHashFixture2 =
  -- "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
  unsafePartial $ fromJust
    $ decodeCbor
    $ wrap
    $ hexToByteArrayUnsafe
        "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"

nativeScriptFixture1 :: NativeScript
nativeScriptFixture1 = ScriptPubkey ed25519KeyHashFixture1

nativeScriptFixture2 :: NativeScript
nativeScriptFixture2 = ScriptPubkey ed25519KeyHashFixture2

nativeScriptFixture3 :: NativeScript
nativeScriptFixture3 = ScriptAll [ nativeScriptFixture1, nativeScriptFixture2 ]

nativeScriptFixture4 :: NativeScript
nativeScriptFixture4 = ScriptAny [ nativeScriptFixture1, nativeScriptFixture2 ]

nativeScriptFixture5 :: NativeScript
nativeScriptFixture5 = ScriptNOfK 1
  [ nativeScriptFixture1, nativeScriptFixture2 ]

nativeScriptFixture6 :: NativeScript
nativeScriptFixture6 = TimelockStart $ Slot $ BigNum.fromInt 1000

nativeScriptFixture7 :: NativeScript
nativeScriptFixture7 = TimelockExpiry $ Slot $ BigNum.fromInt 2000

keyHashBaseAddress :: { payment :: String, stake :: String } -> Address
keyHashBaseAddress { payment, stake } = BaseAddress
  { networkId: TestnetId
  , stakeCredential:
      wrap $ PubKeyHashCredential $ unsafePartial $ fromJust $ decodeCbor
        -- $ T.Bech32 "hstk_1rsf0q0q77t5nttxrtmpwd7tvv58a80a686t92pgy65ekz0s8ncu"
        $ wrap
        $ hexToByteArrayUnsafe stake
  , paymentCredential:
      wrap $ PubKeyHashCredential $ unsafePartial $ fromJust $ decodeCbor
        -- "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
        $ wrap
        $ hexToByteArrayUnsafe payment
  }

plutusDataFixture1 :: PlutusData
plutusDataFixture1 = List []

plutusDataFixture2 :: PlutusData
plutusDataFixture2 = List [ plutusDataFixture1 ]

plutusDataFixture3 :: PlutusData
plutusDataFixture3 = Bytes
  ( hexToByteArrayUnsafe
      "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
  )

plutusDataFixture4 :: PlutusData
plutusDataFixture4 = Constr BigNum.one
  [ plutusDataFixture2, plutusDataFixture3 ]

plutusDataFixture5 :: PlutusData
plutusDataFixture5 = Integer (BigInt.fromInt 42)

plutusDataFixture6 :: PlutusData
plutusDataFixture6 = Map
  [ plutusDataFixture1 /\ plutusDataFixture2
  , plutusDataFixture3 /\ plutusDataFixture4
  ]

plutusDataFixture7 :: PlutusData
plutusDataFixture7 = List
  [ plutusDataFixture1
  , plutusDataFixture2
  , plutusDataFixture3
  , plutusDataFixture4
  , plutusDataFixture5
  , plutusDataFixture6
  ]

plutusDataFixture8 :: PlutusData
plutusDataFixture8 = Constr BigNum.zero
  [ Bytes
      ( hexToByteArrayUnsafe
          "da13ed22b9294f1d86bbd530e99b1456884c7364bf16c90edc1ae41e"
      )
  , Integer (BigInt.fromInt 500000000)
  , Bytes
      ( hexToByteArrayUnsafe
          "82325cbfc20b85bd1ca12e5d12b44b83f68662d8395167b45f1ff7fa"
      )
  , Bytes (hexToByteArrayUnsafe "746f6e6573206f6620736b7920")
  , Bytes
      ( hexToByteArrayUnsafe
          "da13ed22b9294f1d86bbd530e99b1456884c7364bf16c90edc1ae41e"
      )
  , Integer (BigInt.fromInt 45)
  ]

plutusDataFixture8Bytes :: ByteArray
plutusDataFixture8Bytes = hexToByteArrayUnsafe
  "d8799f581cda13ed22b9294f1d86bbd530e99b1456884c7364bf16c90edc1ae41e1a1dcd6500\
  \581c82325cbfc20b85bd1ca12e5d12b44b83f68662d8395167b45f1ff7fa4d746f6e6573206f\
  \6620736b7920581cda13ed22b9294f1d86bbd530e99b1456884c7364bf16c90edc1ae41e\
  \182dff"

plutusDataFixture8Bytes' :: ByteArray
plutusDataFixture8Bytes' = hexToByteArrayUnsafe
  "d866820086581cda13ed22b9294f1d86bbd530e99b1456884c7364bf16c90edc1ae41e1a1dcd\
  \6500581c82325cbfc20b85bd1ca12e5d12b44b83f68662d8395167b45f1ff7fa4d746f6e6573\
  \206f6620736b7920581cda13ed22b9294f1d86bbd530e99b1456884c7364bf16c90edc1ae41e\
  \182d"

readJsonFixtureFile :: String -> Effect Aeson
readJsonFixtureFile path =
  readTextFile UTF8 path >>=
    pure <<< fromRight aesonNull <<< parseJsonStringToAeson

-- TODO: remove CIP25 fixtures below

ogmiosEvaluateTxValidRespFixture :: Effect Aeson
ogmiosEvaluateTxValidRespFixture =
  readJsonFixtureFile "test/Fixtures/OgmiosEvaluateTxValidRespFixture.json"

ogmiosEvaluateTxInvalidPointerFormatFixture :: Effect Aeson
ogmiosEvaluateTxInvalidPointerFormatFixture =
  readJsonFixtureFile
    "test/Fixtures/OgmiosEvaluateTxInvalidPointerFormatFixture.json"

ogmiosEvaluateTxFailIncompatibleEraFixture :: Effect Aeson
ogmiosEvaluateTxFailIncompatibleEraFixture =
  readJsonFixtureFile
    "test/Fixtures/OgmiosEvaluateTxFailIncompatibleEraFixture.json"

ogmiosEvaluateTxFailScriptErrorsFixture :: Effect Aeson
ogmiosEvaluateTxFailScriptErrorsFixture =
  readJsonFixtureFile
    "test/Fixtures/OgmiosEvaluateTxFailScriptErrorsFixture.json"

redeemerFixture1 :: Redeemer
redeemerFixture1 = Redeemer
  { tag: Spend
  , index: BigNum.fromInt 0
  , data: wrap plutusDataFixture7
  , exUnits: ExUnits
      { mem: BigNum.fromInt 1
      , steps: BigNum.fromInt 1
      }
  }

unappliedScriptFixture :: PlutusScript
unappliedScriptFixture =
  plutusV1Script $ wrap $ hexToByteArrayUnsafe $
    "586f010000333222323233222233222225335300c33225335300e0021001100f333500b223\
    \33573466e1c00800404003c0152002333500b22333573466e3c00800404003c01122010010\
    \091326353008009498cd4015d680119a802bae001120012001120011200112200212200120\
    \0101"

partiallyAppliedScriptFixture :: PlutusScript
partiallyAppliedScriptFixture =
  plutusV1Script $ wrap $ hexToByteArrayUnsafe $
    "58750100003333222323233222233222225335300c33225335300e0021001100f333500b22\
    \333573466e1c00800404003c0152002333500b22333573466e3c00800404003c0112210010\
    \091326353008009498cd4015d680119a802bae001120012001120011200112200212200120\
    \014c010218200001"

fullyAppliedScriptFixture :: PlutusScript
fullyAppliedScriptFixture =
  plutusV1Script $ wrap $ hexToByteArrayUnsafe $
    "587f01000033333222323233222233222225335300c33225335300e0021001100f333500b2\
    \2333573466e1c00800404003c0152002333500b22333573466e3c00800404003c011220100\
    \10091326353008009498cd4015d680119a802bae0011200120011200112001122002122001\
    \20014c01021820004c010544746573740001"

nullPaymentPubKeyHash :: PaymentPubKeyHash
nullPaymentPubKeyHash = PaymentPubKeyHash
  $ unsafePartial
  $ fromJust
  $ decodeCbor
  $ wrap
  $ hexToByteArrayUnsafe
      "f9dca21a6c826ec8acb4cf395cbc24351937bfe6560b2683ab8b415f"
