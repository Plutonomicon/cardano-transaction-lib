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
  , cip25MetadataFixture1
  , cip25MetadataFixture2
  , cip25MetadataFixture3
  , cip25MetadataFixture4
  , cip25MetadataJsonFixture1
  , cip25MetadataJsonFixture2
  , cip25MetadataJsonFixture3
  , currencySymbol1
  , ed25519KeyHash1
  , ed25519KeyHashFixture1
  , fullyAppliedScriptFixture
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
  , ogmiosEvaluateTxFailScriptErrorsFixture
  , ogmiosEvaluateTxFailIncompatibleEraFixture
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
  , txFixture2
  , txFixture3
  , txFixture4
  , txFixture5
  , txFixture6
  , txInputFixture1
  , txOutputBinaryFixture1
  , txOutputFixture1
  , txOutputFixture2
  , unappliedScriptFixture
  , unsafeMkCip25String
  , utxoFixture1
  , utxoFixture1'
  , witnessSetFixture1
  , witnessSetFixture2
  , witnessSetFixture2Value
  , witnessSetFixture3
  , witnessSetFixture3Value
  , witnessSetFixture4
  , utxoMapFixture
  ) where

import Prelude

import Aeson (Aeson, aesonNull, decodeAeson, fromString, parseJsonStringToAeson)
import Contract.Numeric.BigNum (BigNum)
import Contract.Numeric.BigNum (fromBigInt, fromInt, one, zero) as BigNum
import Contract.Transaction
  ( PoolPubKeyHash(PoolPubKeyHash)
  , vrfKeyHashFromBytes
  )
import Ctl.Internal.Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  )
import Ctl.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(PlutusScriptRef, NativeScriptRef)
  )
import Ctl.Internal.Cardano.Types.Transaction
  ( AuxiliaryData(AuxiliaryData)
  , AuxiliaryDataHash(AuxiliaryDataHash)
  , Certificate
      ( MoveInstantaneousRewardsCert
      , GenesisKeyDelegation
      , PoolRetirement
      , PoolRegistration
      , StakeDelegation
      , StakeDeregistration
      , StakeRegistration
      )
  , Epoch(Epoch)
  , GenesisDelegateHash(GenesisDelegateHash)
  , GenesisHash(GenesisHash)
  , Ipv4(Ipv4)
  , Ipv6(Ipv6)
  , MIRToStakeCredentials(MIRToStakeCredentials)
  , Mint(Mint)
  , MoveInstantaneousReward(ToStakeCreds, ToOtherPot)
  , PoolMetadata(PoolMetadata)
  , PoolMetadataHash(PoolMetadataHash)
  , ProposedProtocolParameterUpdates(ProposedProtocolParameterUpdates)
  , Redeemer(Redeemer)
  , Relay(MultiHostName, SingleHostName, SingleHostAddr)
  , RequiredSigner(RequiredSigner)
  , Transaction(Transaction)
  , TransactionOutput(TransactionOutput)
  , TransactionWitnessSet(TransactionWitnessSet)
  , TxBody(TxBody)
  , URL(URL)
  , UtxoMap
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  , mkEd25519Signature
  , mkPublicKey
  )
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Ctl.Internal.Cardano.Types.Value
  ( Coin(Coin)
  , CurrencySymbol
  , Value(Value)
  , mkCurrencySymbol
  , mkNonAdaAsset
  , mkSingletonNonAdaAsset
  )
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Metadata.Cip25.Cip25String (Cip25String, mkCip25String)
import Ctl.Internal.Metadata.Cip25.Common (Cip25TokenName(Cip25TokenName))
import Ctl.Internal.Metadata.Cip25.V2
  ( Cip25Metadata(Cip25Metadata)
  , Cip25MetadataEntry(Cip25MetadataEntry)
  , Cip25MetadataFile(Cip25MetadataFile)
  )
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId(MainnetId, TestnetId)
  , Slot(Slot)
  , StakeCredential
  , baseAddress
  , baseAddressToAddress
  , keyHashCredential
  , rewardAddress
  )
import Ctl.Internal.Serialization.Hash
  ( Ed25519KeyHash
  , ScriptHash
  , ed25519KeyHashFromBech32
  , ed25519KeyHashFromBytes
  , scriptHashFromBytes
  )
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.ByteArray
  ( ByteArray
  , byteArrayFromIntArrayUnsafe
  , hexToByteArray
  , hexToByteArrayUnsafe
  )
import Ctl.Internal.Types.CborBytes (CborBytes(CborBytes))
import Ctl.Internal.Types.Int as Int
import Ctl.Internal.Types.OutputDatum (OutputDatum(NoOutputDatum, OutputDatum))
import Ctl.Internal.Types.PlutusData as PD
import Ctl.Internal.Types.PubKeyHash
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  )
import Ctl.Internal.Types.RedeemerTag (RedeemerTag(Spend))
import Ctl.Internal.Types.RewardAddress (RewardAddress(RewardAddress))
import Ctl.Internal.Types.Scripts
  ( MintingPolicyHash(MintingPolicyHash)
  , PlutusScript
  , Validator
  , plutusV1Script
  , plutusV2Script
  )
import Ctl.Internal.Types.TokenName (TokenName, mkTokenName)
import Ctl.Internal.Types.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  )
import Ctl.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  , TransactionMetadatum(Text)
  , TransactionMetadatumLabel(TransactionMetadatumLabel)
  )
import Data.Array as Array
import Data.Either (fromRight, hush)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set (singleton) as Set
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import JS.BigInt as BigInt
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Fixtures.CostModels (costModelsFixture1)

txOutputFixture1 :: TransactionOutput
txOutputFixture1 =
  TransactionOutput
    { address: baseAddressToAddress $ baseAddress
        { network: TestnetId
        , delegationCred:
            keyHashCredential $ unsafePartial $ fromJust
              $ ed25519KeyHashFromBytes
              -- $ T.Bech32 "hstk_1rsf0q0q77t5nttxrtmpwd7tvv58a80a686t92pgy65ekz0s8ncu"
              $ hexToByteArrayUnsafe
                  "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"
        , paymentCred:
            keyHashCredential $ unsafePartial $ fromJust
              $ ed25519KeyHashFromBytes
              -- "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
              $ hexToByteArrayUnsafe
                  "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
        }
    , amount: Value (Coin $ BigInt.fromInt 0) mempty
    , datum: NoOutputDatum
    , scriptRef: Nothing
    }

txOutputFixture2 :: TransactionOutput
txOutputFixture2 =
  TransactionOutput
    { address: keyHashBaseAddress
        { stake: "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"
        , payment: "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
        }
    , amount: Value (Coin $ BigInt.fromInt 0) $
        mkSingletonNonAdaAsset currencySymbol1 tokenName1
          (BigInt.fromInt 1000000)
    , datum: NoOutputDatum
    , scriptRef: Nothing
    }

currencySymbol1 :: CurrencySymbol
currencySymbol1 = unsafePartial $ fromJust $ mkCurrencySymbol $
  hexToByteArrayUnsafe
    "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"

tokenNameFromString :: String -> TokenName
tokenNameFromString s = unsafePartial $ fromJust $ mkTokenName $
  hexToByteArrayUnsafe s

tokenName1 :: TokenName
tokenName1 = tokenNameFromString "4974657374546f6b656e"

tokenName2 :: TokenName
tokenName2 = tokenNameFromString "54657374546f6b656e32"

tokenName4 :: TokenName
tokenName4 = tokenNameFromString "abcdef"

txOutputBinaryFixture1 :: String
txOutputBinaryFixture1 =
  "8258390030fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea9711c12f03c1ef2\
  \e935acc35ec2e6f96c650fd3bfba3e96550504d5336100"

pkhBech32 :: Bech32String
pkhBech32 = "addr_vkh1zuctrdcq6ctd29242w8g84nlz0q38t2lnv3zzfcrfqktx0c9tzp"

stake1 :: StakeCredential
stake1 = unsafePartial $ fromJust do
  keyHashCredential <$> ed25519KeyHashFromBech32 pkhBech32

ed25519KeyHash1 :: Ed25519KeyHash
ed25519KeyHash1 = unsafePartial $ fromJust $ ed25519KeyHashFromBech32 pkhBech32

bigNumOne :: BigNum
bigNumOne = unsafePartial $ fromJust $ BigNum.fromBigInt $ BigInt.fromInt 1

rewardAddress1 :: RewardAddress
rewardAddress1 = RewardAddress $ rewardAddress
  { network: TestnetId, paymentCred: stake1 }

proposedProtocolParameterUpdates1 :: ProposedProtocolParameterUpdates
proposedProtocolParameterUpdates1 = ProposedProtocolParameterUpdates $
  Map.fromFoldable
    [ GenesisHash
        ( hexToByteArrayUnsafe
            "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65"
        ) /\
        { minfeeA: Just $ Coin $ BigInt.fromInt 1
        , minfeeB: Just $ Coin $ BigInt.fromInt 1
        , maxBlockBodySize: Just $ UInt.fromInt 10000
        , maxTxSize: Just $ UInt.fromInt 10000
        , maxBlockHeaderSize: Just $ UInt.fromInt 1000
        , keyDeposit: Just $ Coin $ BigInt.fromInt 1
        , poolDeposit: Just $ Coin $ BigInt.fromInt 1
        , maxEpoch: Just $ Epoch one
        , nOpt: Just $ UInt.fromInt 1
        , poolPledgeInfluence: Just
            { numerator: bigNumOne, denominator: bigNumOne }
        , expansionRate: Just { numerator: bigNumOne, denominator: bigNumOne }
        , treasuryGrowthRate: Just
            { numerator: bigNumOne, denominator: bigNumOne }
        , protocolVersion: Just
            { major: UInt.fromInt 1, minor: UInt.fromInt 1 }
        , minPoolCost: Just bigNumOne
        , adaPerUtxoByte: Just bigNumOne
        , costModels: Just costModelsFixture1
        , executionCosts: Just
            { memPrice: { numerator: bigNumOne, denominator: bigNumOne }
            , stepPrice: { numerator: bigNumOne, denominator: bigNumOne }
            }
        , maxTxExUnits: Just { mem: BigInt.fromInt 1, steps: BigInt.fromInt 1 }
        , maxBlockExUnits: Just
            { mem: BigInt.fromInt 1, steps: BigInt.fromInt 1 }
        , maxValueSize: Just $ UInt.fromInt 1
        , collateralPercentage: Just $ UInt.fromInt 140
        , maxCollateralInputs: Just $ UInt.fromInt 10
        }
    ]

-- | Extend this for your needs.
type SampleTxConfig =
  { inputs :: Set TransactionInput }

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
  buildChanges (Transaction { body: TxBody { inputs } }) mkChanges =
    mkChanges { inputs }

  applyChanges :: Transaction -> SampleTxConfig -> Transaction
  applyChanges
    ( Transaction
        { body: TxBody
            { outputs
            , fee
            , ttl
            , certs
            , withdrawals
            , update
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
            }
        , witnessSet
        , isValid
        , auxiliaryData
        }
    )
    { inputs: newInputs } =
    ( Transaction
        { body: TxBody
            { inputs: newInputs
            , outputs
            , fee
            , ttl
            , certs
            , withdrawals
            , update
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

txFixture1 :: Transaction
txFixture1 =
  Transaction
    { body: TxBody
        { inputs: Set.singleton txInputFixture1
        , outputs: [ txOutputFixture1 ]
        , fee: Coin $ BigInt.fromInt 177513
        , ttl: Nothing
        , certs: Nothing
        , withdrawals: Nothing
        , update: Nothing
        , auxiliaryDataHash: Nothing
        , validityStartInterval: Nothing
        , mint: Nothing
        , referenceInputs: mempty
        , scriptDataHash: Nothing
        , collateral: Nothing
        , requiredSigners: Nothing
        , networkId: Just MainnetId
        , collateralReturn: Nothing
        , totalCollateral: Nothing
        }
    , witnessSet: TransactionWitnessSet
        { vkeys: Nothing
        , nativeScripts: Nothing
        , bootstraps: Nothing
        , plutusScripts: Nothing
        , plutusData: Nothing
        , redeemers: Nothing
        }
    , isValid: true
    , auxiliaryData: Nothing
    }

txFixture2 :: Transaction
txFixture2 =
  Transaction
    { body: TxBody
        { inputs: Set.singleton txInputFixture1
        , outputs: [ txOutputFixture2 ]
        , fee: Coin $ BigInt.fromInt 177513
        , ttl: Nothing
        , certs: Nothing
        , withdrawals: Nothing
        , update: Nothing
        , auxiliaryDataHash: Nothing
        , validityStartInterval: Nothing
        , mint: Nothing
        , referenceInputs: mempty
        , scriptDataHash: Nothing
        , collateral: Nothing
        , requiredSigners: Nothing
        , networkId: Just MainnetId
        , collateralReturn: Nothing
        , totalCollateral: Nothing
        }
    , witnessSet: TransactionWitnessSet
        { vkeys: Nothing
        , nativeScripts: Nothing
        , bootstraps: Nothing
        , plutusScripts: Nothing
        , plutusData: Nothing
        , redeemers: Nothing
        }
    , isValid: true
    , auxiliaryData: Nothing
    }

txFixture3 :: Transaction
txFixture3 =
  Transaction
    { body: TxBody
        { inputs: Set.singleton txInputFixture1
        , outputs:
            [ TransactionOutput
                { address: keyHashBaseAddress
                    { stake:
                        "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
                    -- $ T.Bech32 "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
                    , payment:
                        "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
                    }
                , amount: Value (Coin $ BigInt.fromInt 2353402) mempty
                , datum: NoOutputDatum
                , scriptRef: Nothing
                }
            , TransactionOutput
                { address: keyHashBaseAddress
                    { stake:
                        "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
                    -- $ T.Bech32 "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
                    , payment:
                        "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
                    }
                , amount: Value (Coin $ BigInt.fromInt 1000000) mempty
                , datum: NoOutputDatum
                , scriptRef: Nothing
                }
            ]
        , fee: Coin $ BigInt.fromInt 177513
        , ttl: Nothing
        , certs: Nothing
        , withdrawals: Nothing
        , update: Nothing
        , referenceInputs: Set.singleton txInputFixture1
        , auxiliaryDataHash: Nothing
        , validityStartInterval: Nothing
        , mint: Nothing
        , scriptDataHash: Nothing
        , collateral: Nothing
        , requiredSigners: Nothing
        , networkId: Just MainnetId
        , collateralReturn: Nothing
        , totalCollateral: Nothing
        }
    , witnessSet: TransactionWitnessSet
        { vkeys: Nothing
        , nativeScripts: Nothing
        , bootstraps: Nothing
        , plutusScripts: Nothing
        , plutusData: Nothing
        , redeemers: Nothing
        }
    , isValid: true
    , auxiliaryData: Nothing
    }

txFixture4 :: Transaction
txFixture4 =
  Transaction
    { body: TxBody
        { inputs: Set.singleton txInputFixture1
        , outputs:
            [ TransactionOutput
                { address: keyHashBaseAddress
                    { stake:
                        "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
                    -- $ T.Bech32 "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
                    , payment:
                        "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
                    }
                , amount: Value (Coin $ BigInt.fromInt 2353402) mempty
                , datum: OutputDatum $ wrap plutusDataFixture1
                , scriptRef: Just $ PlutusScriptRef plutusScriptFixture1
                }
            , TransactionOutput
                { address: keyHashBaseAddress
                    { stake:
                        "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
                    -- $ T.Bech32 "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
                    , payment:
                        "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
                    }
                , amount: Value (Coin $ BigInt.fromInt 1000000) mempty
                , datum: NoOutputDatum
                , scriptRef: Just $ NativeScriptRef nativeScriptFixture5
                }
            ]
        , fee: Coin $ BigInt.fromInt 177513
        , ttl: Just $ Slot $ BigNum.fromInt 123
        , certs: Just
            [ StakeRegistration stake1
            , StakeDeregistration stake1
            , StakeDelegation stake1
                (PoolPubKeyHash $ PubKeyHash ed25519KeyHash1)
            , PoolRegistration
                { operator: PoolPubKeyHash $ PubKeyHash ed25519KeyHash1
                , vrfKeyhash: unsafePartial $ fromJust $
                    hexToByteArray
                      "fbf6d41985670b9041c5bf362b5262cf34add5d265975de176d613ca05f37096"
                      >>= vrfKeyHashFromBytes
                , pledge: bigNumOne
                , cost: bigNumOne
                , margin: { numerator: bigNumOne, denominator: bigNumOne }
                , rewardAccount: RewardAddress $ rewardAddress
                    { network: MainnetId, paymentCred: stake1 }
                , poolOwners: [ wrap $ wrap ed25519KeyHash1 ]
                , relays:
                    [ SingleHostAddr
                        { port: Just 8080
                        , ipv4: Just $ Ipv4 $ byteArrayFromIntArrayUnsafe
                            [ 127, 0, 0, 1 ]
                        , ipv6: Just $ Ipv6 $ byteArrayFromIntArrayUnsafe
                            $ Array.replicate 16 123
                        }
                    , SingleHostName
                        { port: Just 8080
                        , dnsName: "example.com"
                        }
                    , MultiHostName { dnsName: "example.com" }
                    ]
                , poolMetadata: Just $ PoolMetadata
                    { url: URL "https://example.com/"
                    , hash: PoolMetadataHash $
                        hexToByteArrayUnsafe
                          "94b8cac47761c1140c57a48d56ab15d27a842abff041b3798b8618fa84641f5a"
                    }
                }
            , PoolRetirement
                { poolKeyHash: PoolPubKeyHash $ PubKeyHash ed25519KeyHash1
                , epoch: Epoch one
                }
            , GenesisKeyDelegation
                { genesisHash: GenesisHash $
                    hexToByteArrayUnsafe
                      "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65"
                , genesisDelegateHash: GenesisDelegateHash $
                    hexToByteArrayUnsafe
                      "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65"
                , vrfKeyhash: unsafePartial $ fromJust $
                    hexToByteArray
                      "fbf6d41985670b9041c5bf362b5262cf34add5d265975de176d613ca05f37096"
                      >>= vrfKeyHashFromBytes
                }
            , MoveInstantaneousRewardsCert $ ToOtherPot
                { pot: one
                , amount: bigNumOne
                }
            , MoveInstantaneousRewardsCert $ ToStakeCreds
                { pot: one
                , amounts: MIRToStakeCredentials $ Map.fromFoldable
                    [ stake1 /\ Int.newPositive bigNumOne ]
                }
            ]
        , withdrawals: Just $ Map.fromFoldable
            [ rewardAddress1 /\ Coin one ]
        , update: Just
            { proposedProtocolParameterUpdates:
                proposedProtocolParameterUpdates1
            , epoch: Epoch one
            }
        , auxiliaryDataHash: Just $ AuxiliaryDataHash
            $ byteArrayFromIntArrayUnsafe
            $ Array.replicate 32 0
        , validityStartInterval: Just $ Slot $ BigNum.fromInt 124
        , mint: Just $ Mint $ mkNonAdaAsset $ Map.fromFoldable
            [ currencySymbol1 /\ Map.fromFoldable [ tokenName1 /\ one ] ]
        , referenceInputs: mempty
        , scriptDataHash: Nothing
        , collateral: Nothing
        , requiredSigners: Just [ RequiredSigner ed25519KeyHashFixture1 ]
        , networkId: Just MainnetId
        , collateralReturn: Just txOutputFixture1
        , totalCollateral: Just $ Coin $ BigInt.fromInt 5_000_000
        }
    , witnessSet: TransactionWitnessSet
        { vkeys: Nothing
        , nativeScripts: Nothing
        , bootstraps: Nothing
        , plutusScripts: Nothing
        , plutusData: Nothing
        , redeemers: Nothing
        }
    , isValid: true
    , auxiliaryData: Nothing
    }

txFixture5 :: Transaction
txFixture5 =
  Transaction
    { body: TxBody
        { inputs: Set.singleton txInputFixture1
        , outputs:
            [ TransactionOutput
                { address: keyHashBaseAddress
                    { stake:
                        "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
                    -- $ T.Bech32 "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
                    , payment:
                        "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
                    }
                , amount: Value (Coin $ BigInt.fromInt 490234098) mempty
                , datum: OutputDatum $ wrap plutusDataFixture1
                , scriptRef: Just $ PlutusScriptRef plutusScriptFixture2
                }
            ]
        , fee: Coin $ BigInt.fromInt 89489324
        , ttl: Nothing
        , certs: Nothing
        , withdrawals: Nothing
        , update: Nothing
        , auxiliaryDataHash: Nothing
        , validityStartInterval: Nothing
        , mint: Nothing
        , referenceInputs: mempty
        , scriptDataHash: Nothing
        , collateral: Nothing
        , requiredSigners: Nothing
        , networkId: Just MainnetId
        , collateralReturn: Nothing
        , totalCollateral: Nothing
        }
    , witnessSet: TransactionWitnessSet
        { vkeys: Nothing
        , nativeScripts: Nothing
        , bootstraps: Nothing
        , plutusScripts: Nothing
        , plutusData: Nothing
        , redeemers: Nothing
        }
    , isValid: true
    , auxiliaryData: Nothing
    }

txFixture6 :: Transaction
txFixture6 =
  Transaction
    { body: TxBody
        { inputs: Set.singleton txInputFixture1
        , outputs: [ txOutputFixture1 ]
        , fee: Coin $ BigInt.fromInt 177513
        , ttl: Nothing
        , certs: Nothing
        , withdrawals: Nothing
        , update: Nothing
        , auxiliaryDataHash: Nothing
        , validityStartInterval: Nothing
        , mint: Nothing
        , referenceInputs: mempty
        , scriptDataHash: Nothing
        , collateral: Nothing
        , requiredSigners: Nothing
        , networkId: Just MainnetId
        , collateralReturn: Nothing
        , totalCollateral: Nothing
        }
    , witnessSet: TransactionWitnessSet
        { vkeys: Nothing
        , nativeScripts: Nothing
        , bootstraps: Nothing
        , plutusScripts: Nothing
        , plutusData: Nothing
        , redeemers: Nothing
        }
    , isValid: true
    , auxiliaryData: Just $ AuxiliaryData
        { metadata: Just $ GeneralTransactionMetadata
            ( Map.fromFoldable
                [ TransactionMetadatumLabel (BigInt.fromInt 8) /\ Text "foo" ]
            )
        , nativeScripts: Nothing
        , plutusScripts: Nothing
        }
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
  , transactionId: TransactionHash
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
      { address: baseAddressToAddress $ baseAddress
          { network: TestnetId
          , paymentCred: keyHashCredential $ unsafePartial $ fromJust
              $ ed25519KeyHashFromBytes
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
          , delegationCred: keyHashCredential $ unsafePartial $ fromJust
              $ ed25519KeyHashFromBytes
              $
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
      , amount: Value (Coin (BigInt.fromInt 5000000)) mempty
      , datum: NoOutputDatum
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
    { bootstraps: Nothing
    , nativeScripts: Nothing
    , plutusData: Nothing
    , plutusScripts: Nothing
    , redeemers: Nothing
    , vkeys: Just
        [ Vkeywitness
            ( Vkey
                ( unsafePartial $ fromJust $ mkPublicKey
                    "ed25519_pk1p9sf9wz3t46u9ghht44203gerxt82kzqaqw74fqrmwjmdy8sjxmqknzq8j"
                )
                /\
                  ( unsafePartial $ fromJust <<< mkEd25519Signature $
                      "ed25519_sig1mr6pm5kanam2wkmae70jx7fjkzepghefj0lmnczu6fra\
                      \6auf2urgrte5axxhunw4x34l3l8tj9c0t4le39tj8lpjdgxmqnujw07t\
                      \kzs9m6t6x"
                  )
            )
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
    { bootstraps: Nothing
    , nativeScripts: Nothing
    , plutusData:
        Just
          [ PD.Bytes
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
    , plutusScripts: Nothing
    , redeemers: Nothing
    , vkeys: Just
        [ Vkeywitness
            ( Vkey
                ( unsafePartial $ fromJust $ mkPublicKey
                    "ed25519_pk1p9sf9wz3t46u9ghht44203gerxt82kzqaqw74fqrmwjmdy8sjxmqknzq8j"
                )
                /\
                  ( unsafePartial $ fromJust <<< mkEd25519Signature $
                      "ed25519_sig1clmhgxx9e9t24wzgkmcsr44uq98j935evsjnrj8nn7ge08\
                      \qrz0mgdxv5qtz8dyghs47q3lxwk4akq3u2ty8v4egeqvtl02ll0nfcqqq\
                      \6faxl6"
                  )
            )
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
    { transactionId: TransactionHash $
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
    $ ed25519KeyHashFromBytes
    $ hexToByteArrayUnsafe
        "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"

ed25519KeyHashFixture2 :: Ed25519KeyHash
ed25519KeyHashFixture2 =
  -- "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
  unsafePartial $ fromJust
    $ ed25519KeyHashFromBytes
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
plutusDataFixture3 = PD.Bytes
  ( hexToByteArrayUnsafe
      "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"
  )

plutusDataFixture4 :: PD.PlutusData
plutusDataFixture4 = PD.Constr BigNum.one
  [ plutusDataFixture2, plutusDataFixture3 ]

plutusDataFixture5 :: PD.PlutusData
plutusDataFixture5 = PD.Integer (BigInt.fromInt 42)

plutusDataFixture6 :: PD.PlutusData
plutusDataFixture6 = PD.Map
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

plutusDataFixture8 :: PD.PlutusData
plutusDataFixture8 = PD.Constr BigNum.zero
  [ PD.Bytes
      ( hexToByteArrayUnsafe
          "da13ed22b9294f1d86bbd530e99b1456884c7364bf16c90edc1ae41e"
      )
  , PD.Integer (BigInt.fromInt 500000000)
  , PD.Bytes
      ( hexToByteArrayUnsafe
          "82325cbfc20b85bd1ca12e5d12b44b83f68662d8395167b45f1ff7fa"
      )
  , PD.Bytes (hexToByteArrayUnsafe "746f6e6573206f6620736b7920")
  , PD.Bytes
      ( hexToByteArrayUnsafe
          "da13ed22b9294f1d86bbd530e99b1456884c7364bf16c90edc1ae41e"
      )
  , PD.Integer (BigInt.fromInt 45)
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

scriptHashFromString :: String -> ScriptHash
scriptHashFromString s = unsafePartial $ fromJust $ scriptHashFromBytes $
  hexToByteArrayUnsafe s

scriptHash1 :: ScriptHash
scriptHash1 = scriptHashFromString
  "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65"

scriptHash4 :: ScriptHash
scriptHash4 = scriptHashFromString
  "1e4409ba69fb38887c23d15a766476384085b66a755530934938abfe"

policyId :: MintingPolicyHash
policyId = MintingPolicyHash scriptHash1

policyId4 :: MintingPolicyHash
policyId4 = MintingPolicyHash scriptHash4

cip25MetadataFilesFixture1 :: Array Cip25MetadataFile
cip25MetadataFilesFixture1 = Cip25MetadataFile <$>
  [ { name: unsafeMkCip25String "file_name_1"
    , mediaType: unsafeMkCip25String "media_type"
    , src: "uri1"
    }
  , { name: unsafeMkCip25String "file_name_2"
    , mediaType: unsafeMkCip25String "media_type_2"
    , src: "uri4"
    }
  ]

cip25MetadataEntryFixture1 :: Cip25MetadataEntry
cip25MetadataEntryFixture1 = Cip25MetadataEntry
  { policyId: policyId
  , assetName: Cip25TokenName tokenName1
  , name: unsafeMkCip25String "ItestToken"
  , image: "image_uri1"
  , mediaType: Just $ unsafeMkCip25String "media_type"
  , description: Just "desc1"
  , files: cip25MetadataFilesFixture1
  }

cip25MetadataEntryFixture2 :: Cip25MetadataEntry
cip25MetadataEntryFixture2 = Cip25MetadataEntry
  { policyId: policyId
  , assetName: Cip25TokenName tokenName2
  , name: unsafeMkCip25String "TestToken2"
  , image: "image_uri1"
  , mediaType: Nothing
  , description: Nothing
  , files: []
  }

cip25MetadataFixture1 :: Cip25Metadata
cip25MetadataFixture1 = Cip25Metadata
  [ cip25MetadataEntryFixture1, cip25MetadataEntryFixture2 ]

cip25MetadataFixture2 :: Cip25Metadata
cip25MetadataFixture2 = Cip25Metadata
  [ Cip25MetadataEntry
      { policyId: policyId
      , assetName: Cip25TokenName tokenName1
      , name: unsafeMkCip25String "ItestToken"
      , image: "image_uri1"
      , mediaType: Nothing
      , description: Nothing
      , files: []
      }
  ]

cip25MetadataFixture3 :: Cip25Metadata
cip25MetadataFixture3 = Cip25Metadata
  [ Cip25MetadataEntry
      { policyId: policyId
      , assetName: Cip25TokenName tokenName1
      , name: unsafeMkCip25String "monkey.jpg"
      , image:
          -- checking long strings
          "https://upload.wikimedia.org/wikipedia/commons/3/35/Olive_baboon_Ngorongoro.jpg?download"
      , mediaType: Nothing
      , description: Nothing
      , files: []
      }
  ]

cip25MetadataFixture4 :: Cip25Metadata
cip25MetadataFixture4 = Cip25Metadata
  [ Cip25MetadataEntry
      { policyId: policyId4
      , assetName: Cip25TokenName tokenName4
      , name: unsafeMkCip25String "Allium"
      , image: "ipfs://k2cwuee3arxg398hwxx6c0iferxitu126xntuzg8t765oo020h5y6npn"
      , mediaType: Nothing
      , description: Just "From pixabay"
      , files: []
      }
  ]

unsafeMkCip25String :: String -> Cip25String
unsafeMkCip25String str = unsafePartial $ fromJust $ mkCip25String str

readJsonFixtureFile :: String -> Effect Aeson
readJsonFixtureFile path =
  readTextFile UTF8 path >>=
    pure <<< fromRight aesonNull <<< parseJsonStringToAeson

cip25MetadataJsonFixture1 :: Effect Aeson
cip25MetadataJsonFixture1 =
  readJsonFixtureFile "test/Fixtures/cip25MetadataJsonFixture1.json"

cip25MetadataJsonFixture2 :: Effect Aeson
cip25MetadataJsonFixture2 =
  readJsonFixtureFile "test/Fixtures/cip25MetadataJsonFixture2.json"

cip25MetadataJsonFixture3 :: Effect Aeson
cip25MetadataJsonFixture3 =
  readJsonFixtureFile "test/Fixtures/cip25MetadataJsonFixture3.json"

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
  , index: BigInt.fromInt 0
  , data: plutusDataFixture7
  , exUnits:
      { mem: BigInt.fromInt 1
      , steps: BigInt.fromInt 1
      }
  }

unappliedScriptFixture :: Validator
unappliedScriptFixture =
  wrap $ plutusV1Script $ hexToByteArrayUnsafe $
    "586f010000333222323233222233222225335300c33225335300e0021001100f333500b223\
    \33573466e1c00800404003c0152002333500b22333573466e3c00800404003c01122010010\
    \091326353008009498cd4015d680119a802bae001120012001120011200112200212200120\
    \0101"

partiallyAppliedScriptFixture :: Validator
partiallyAppliedScriptFixture =
  wrap $ plutusV1Script $ hexToByteArrayUnsafe $
    "58750100003333222323233222233222225335300c33225335300e0021001100f333500b22\
    \333573466e1c00800404003c0152002333500b22333573466e3c00800404003c0112210010\
    \091326353008009498cd4015d680119a802bae001120012001120011200112200212200120\
    \014c010218200001"

fullyAppliedScriptFixture :: Validator
fullyAppliedScriptFixture =
  wrap $ plutusV1Script $ hexToByteArrayUnsafe $
    "587f01000033333222323233222233222225335300c33225335300e0021001100f333500b2\
    \2333573466e1c00800404003c0152002333500b22333573466e3c00800404003c011220100\
    \10091326353008009498cd4015d680119a802bae0011200120011200112001122002122001\
    \20014c01021820004c010544746573740001"

nullPaymentPubKeyHash :: PaymentPubKeyHash
nullPaymentPubKeyHash = PaymentPubKeyHash $ PubKeyHash
  $ unsafePartial
  $ fromJust
  $ fromBytes
  $ CborBytes
  $ hexToByteArrayUnsafe
      "f9dca21a6c826ec8acb4cf395cbc24351937bfe6560b2683ab8b415f"
