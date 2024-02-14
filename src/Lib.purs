module Cardano.Serialization.Lib
  ( ForeignErrorable
  , module X
  , address_toBech32
  , address_fromBech32
  , address_networkId
  , assetName_new
  , assetName_name
  , assetNames_new
  , assets_new
  , auxiliaryData_new
  , auxiliaryData_metadata
  , auxiliaryData_setMetadata
  , auxiliaryData_nativeScripts
  , auxiliaryData_setNativeScripts
  , auxiliaryData_plutusScripts
  , auxiliaryData_setPlutusScripts
  , auxiliaryData_preferAlonzoFormat
  , auxiliaryData_setPreferAlonzoFormat
  , auxiliaryDataHash_toBech32
  , auxiliaryDataHash_fromBech32
  , baseAddress_new
  , baseAddress_paymentCred
  , baseAddress_stakeCred
  , baseAddress_toAddress
  , baseAddress_fromAddress
  , bigInt_isZero
  , bigInt_asU64
  , bigInt_asInt
  , bigInt_fromStr
  , bigInt_toStr
  , bigInt_mul
  , bigInt_one
  , bigInt_increment
  , bigInt_divCeil
  , bigNum_fromStr
  , bigNum_toStr
  , bigNum_zero
  , bigNum_one
  , bigNum_isZero
  , bigNum_divFloor
  , bigNum_checkedMul
  , bigNum_checkedAdd
  , bigNum_checkedSub
  , bigNum_clampedSub
  , bigNum_compare
  , bigNum_lessThan
  , bigNum_maxValue
  , bigNum_max
  , bip32PrivateKey_derive
  , bip32PrivateKey_from128Xprv
  , bip32PrivateKey_to128Xprv
  , bip32PrivateKey_generateEd25519Bip32
  , bip32PrivateKey_toRawKey
  , bip32PrivateKey_toPublic
  , bip32PrivateKey_asBytes
  , bip32PrivateKey_fromBech32
  , bip32PrivateKey_toBech32
  , bip32PrivateKey_fromBip39Entropy
  , bip32PrivateKey_chaincode
  , bip32PublicKey_derive
  , bip32PublicKey_toRawKey
  , bip32PublicKey_asBytes
  , bip32PublicKey_fromBech32
  , bip32PublicKey_toBech32
  , bip32PublicKey_chaincode
  , blockHash_toBech32
  , blockHash_fromBech32
  , bootstrapWitness_vkey
  , bootstrapWitness_signature
  , bootstrapWitness_chainCode
  , bootstrapWitness_attributes
  , bootstrapWitness_new
  , bootstrapWitnesses_new
  , byronAddress_toBase58
  , byronAddress_byronProtocolMagic
  , byronAddress_attributes
  , byronAddress_networkId
  , byronAddress_fromBase58
  , byronAddress_icarusFromKey
  , byronAddress_isValid
  , byronAddress_toAddress
  , byronAddress_fromAddress
  , certificate_newStakeRegistration
  , certificate_newStakeDeregistration
  , certificate_newStakeDelegation
  , certificate_newPoolRegistration
  , certificate_newPoolRetirement
  , certificate_newGenesisKeyDelegation
  , certificate_newMoveInstantaneousRewardsCert
  , certificate_kind
  , certificate_asStakeRegistration
  , certificate_asStakeDeregistration
  , certificate_asStakeDelegation
  , certificate_asPoolRegistration
  , certificate_asPoolRetirement
  , certificate_asGenesisKeyDelegation
  , certificate_asMoveInstantaneousRewardsCert
  , certificates_new
  , constrPlutusData_alternative
  , constrPlutusData_data
  , constrPlutusData_new
  , costModel_free
  , costModel_toBytes
  , costModel_fromBytes
  , costModel_toHex
  , costModel_fromHex
  , costModel_toJson
  , costModel_fromJson
  , costModel_new
  , costModel_set
  , costModel_get
  , costModel_len
  , costmdls_new
  , costmdls_retainLanguageVersions
  , dnsRecordAorAAAA_new
  , dnsRecordAorAAAA_record
  , dnsRecordSRV_new
  , dnsRecordSRV_record
  , dataCost_newCoinsPerWord
  , dataCost_newCoinsPerByte
  , dataCost_coinsPerByte
  , dataHash_toBech32
  , dataHash_fromBech32
  , datumSource_new
  , datumSource_newRefInput
  , ed25519KeyHash_toBech32
  , ed25519KeyHash_fromBech32
  , ed25519KeyHashes_new
  , ed25519KeyHashes_toOption
  , ed25519Signature_toBech32
  , ed25519Signature_fromBech32
  , enterpriseAddress_new
  , enterpriseAddress_paymentCred
  , enterpriseAddress_toAddress
  , enterpriseAddress_fromAddress
  , exUnitPrices_memPrice
  , exUnitPrices_stepPrice
  , exUnitPrices_new
  , exUnits_mem
  , exUnits_steps
  , exUnits_new
  , generalTransactionMetadata_new
  , genesisDelegateHash_toBech32
  , genesisDelegateHash_fromBech32
  , genesisHash_toBech32
  , genesisHash_fromBech32
  , genesisHashes_new
  , genesisKeyDelegation_genesishash
  , genesisKeyDelegation_genesisDelegateHash
  , genesisKeyDelegation_vrfKeyhash
  , genesisKeyDelegation_new
  , inputWithScriptWitness_newWithNativeScriptWitness
  , inputWithScriptWitness_newWithPlutusWitness
  , inputWithScriptWitness_input
  , inputsWithScriptWitness_new
  , int_new
  , int_newNegative
  , int_newI32
  , int_isPositive
  , int_asPositive
  , int_asNegative
  , int_asI32
  , int_asI32OrNothing
  , int_asI32OrFail
  , int_toStr
  , int_fromStr
  , ipv4_new
  , ipv4_ip
  , ipv6_new
  , ipv6_ip
  , kesvKey_toBech32
  , kesvKey_fromBech32
  , language_newPlutusV1
  , language_newPlutusV2
  , language_kind
  , languages_new
  , languages_list
  , legacyDaedalusPrivateKey_asBytes
  , legacyDaedalusPrivateKey_chaincode
  , linearFee_constant
  , linearFee_coefficient
  , linearFee_new
  , mirToStakeCredentials_new
  , metadataList_new
  , metadataMap_new
  , metadataMap_insertStr
  , metadataMap_insertI32
  , metadataMap_getStr
  , metadataMap_getI32
  , metadataMap_has
  , mint_new
  , mint_newFromEntry
  , mint_getAll
  , mint_asPositiveMultiasset
  , mint_asNegativeMultiasset
  , mintAssets_new
  , mintAssets_newFromEntry
  , mintWitness_newNativeScript
  , mintWitness_newPlutusScript
  , moveInstantaneousReward_newToOtherPot
  , moveInstantaneousReward_newToStakeCreds
  , moveInstantaneousReward_pot
  , moveInstantaneousReward_kind
  , moveInstantaneousReward_asToOtherPot
  , moveInstantaneousReward_asToStakeCreds
  , moveInstantaneousRewardsCert_moveInstantaneousReward
  , moveInstantaneousRewardsCert_new
  , multiAsset_new
  , multiAsset_setAsset
  , multiAsset_getAsset
  , multiAsset_sub
  , multiHostName_dnsName
  , multiHostName_new
  , nativeScript_hash
  , nativeScript_newScriptPubkey
  , nativeScript_newScriptAll
  , nativeScript_newScriptAny
  , nativeScript_newScriptNOfK
  , nativeScript_newTimelockStart
  , nativeScript_newTimelockExpiry
  , nativeScript_kind
  , nativeScript_asScriptPubkey
  , nativeScript_asScriptAll
  , nativeScript_asScriptAny
  , nativeScript_asScriptNOfK
  , nativeScript_asTimelockStart
  , nativeScript_asTimelockExpiry
  , nativeScript_getRequiredSigners
  , nativeScripts_new
  , networkId_testnet
  , networkId_mainnet
  , networkId_kind
  , networkInfo_new
  , networkInfo_networkId
  , networkInfo_protocolMagic
  , networkInfo_testnetPreview
  , networkInfo_testnetPreprod
  , networkInfo_testnet
  , networkInfo_mainnet
  , nonce_newIdentity
  , nonce_newFromHash
  , nonce_getHash
  , operationalCert_hotVkey
  , operationalCert_sequenceNumber
  , operationalCert_kesPeriod
  , operationalCert_sigma
  , operationalCert_new
  , outputDatum_newDataHash
  , outputDatum_newData
  , outputDatum_dataHash
  , outputDatum_data
  , plutusData_newConstrPlutusData
  , plutusData_newEmptyConstrPlutusData
  , plutusData_newSingleValueConstrPlutusData
  , plutusData_newMap
  , plutusData_newList
  , plutusData_newInteger
  , plutusData_newBytes
  , plutusData_kind
  , plutusData_asConstrPlutusData
  , plutusData_asMap
  , plutusData_asList
  , plutusData_asInteger
  , plutusData_asBytes
  , plutusData_fromAddress
  , plutusList_new
  , plutusMap_new
  , plutusScript_new
  , plutusScript_newV2
  , plutusScript_newWithVersion
  , plutusScript_bytes
  , plutusScript_fromBytesV2
  , plutusScript_fromBytesWithVersion
  , plutusScript_fromHexWithVersion
  , plutusScript_hash
  , plutusScript_languageVersion
  , plutusScriptSource_new
  , plutusScriptSource_newRefInput
  , plutusScriptSource_newRefInputWithLangVer
  , plutusScripts_new
  , plutusWitness_new
  , plutusWitness_newWithRef
  , plutusWitness_newWithoutDatum
  , plutusWitness_newWithRefWithoutDatum
  , plutusWitness_script
  , plutusWitness_datum
  , plutusWitness_redeemer
  , plutusWitnesses_new
  , pointer_new
  , pointer_newPointer
  , pointer_slot
  , pointer_txIndex
  , pointer_certIndex
  , pointer_slotBignum
  , pointer_txIndexBignum
  , pointer_certIndexBignum
  , pointerAddress_new
  , pointerAddress_paymentCred
  , pointerAddress_stakePointer
  , pointerAddress_toAddress
  , pointerAddress_fromAddress
  , poolMetadata_url
  , poolMetadata_poolMetadataHash
  , poolMetadata_new
  , poolMetadataHash_toBech32
  , poolMetadataHash_fromBech32
  , poolParams_operator
  , poolParams_vrfKeyhash
  , poolParams_pledge
  , poolParams_cost
  , poolParams_margin
  , poolParams_rewardAccount
  , poolParams_poolOwners
  , poolParams_relays
  , poolParams_poolMetadata
  , poolParams_new
  , poolRegistration_poolParams
  , poolRegistration_new
  , poolRetirement_poolKeyhash
  , poolRetirement_epoch
  , poolRetirement_new
  , privateKey_free
  , privateKey_toPublic
  , privateKey_generateEd25519
  , privateKey_generateEd25519extended
  , privateKey_fromBech32
  , privateKey_toBech32
  , privateKey_asBytes
  , privateKey_fromExtendedBytes
  , privateKey_fromNormalBytes
  , privateKey_sign
  , privateKey_toHex
  , privateKey_fromHex
  , proposedProtocolParameterUpdates_new
  , protocolParamUpdate_setMinfeeA
  , protocolParamUpdate_minfeeA
  , protocolParamUpdate_setMinfeeB
  , protocolParamUpdate_minfeeB
  , protocolParamUpdate_setMaxBlockBodySize
  , protocolParamUpdate_maxBlockBodySize
  , protocolParamUpdate_setMaxTxSize
  , protocolParamUpdate_maxTxSize
  , protocolParamUpdate_setMaxBlockHeaderSize
  , protocolParamUpdate_maxBlockHeaderSize
  , protocolParamUpdate_setKeyDeposit
  , protocolParamUpdate_keyDeposit
  , protocolParamUpdate_setPoolDeposit
  , protocolParamUpdate_poolDeposit
  , protocolParamUpdate_setMaxEpoch
  , protocolParamUpdate_maxEpoch
  , protocolParamUpdate_setNOpt
  , protocolParamUpdate_nOpt
  , protocolParamUpdate_setPoolPledgeInfluence
  , protocolParamUpdate_poolPledgeInfluence
  , protocolParamUpdate_setExpansionRate
  , protocolParamUpdate_expansionRate
  , protocolParamUpdate_setTreasuryGrowthRate
  , protocolParamUpdate_treasuryGrowthRate
  , protocolParamUpdate_d
  , protocolParamUpdate_extraEntropy
  , protocolParamUpdate_setProtocolVersion
  , protocolParamUpdate_protocolVersion
  , protocolParamUpdate_setMinPoolCost
  , protocolParamUpdate_minPoolCost
  , protocolParamUpdate_setAdaPerUtxoByte
  , protocolParamUpdate_adaPerUtxoByte
  , protocolParamUpdate_setCostModels
  , protocolParamUpdate_costModels
  , protocolParamUpdate_setExecutionCosts
  , protocolParamUpdate_executionCosts
  , protocolParamUpdate_setMaxTxExUnits
  , protocolParamUpdate_maxTxExUnits
  , protocolParamUpdate_setMaxBlockExUnits
  , protocolParamUpdate_maxBlockExUnits
  , protocolParamUpdate_setMaxValueSize
  , protocolParamUpdate_maxValueSize
  , protocolParamUpdate_setCollateralPercentage
  , protocolParamUpdate_collateralPercentage
  , protocolParamUpdate_setMaxCollateralInputs
  , protocolParamUpdate_maxCollateralInputs
  , protocolParamUpdate_new
  , protocolVersion_major
  , protocolVersion_minor
  , protocolVersion_new
  , publicKey_free
  , publicKey_fromBech32
  , publicKey_toBech32
  , publicKey_asBytes
  , publicKey_fromBytes
  , publicKey_verify
  , publicKey_hash
  , publicKey_toHex
  , publicKey_fromHex
  , redeemer_tag
  , redeemer_index
  , redeemer_data
  , redeemer_exUnits
  , redeemer_new
  , redeemerTag_newSpend
  , redeemerTag_newMint
  , redeemerTag_newCert
  , redeemerTag_newReward
  , redeemerTag_kind
  , redeemers_new
  , redeemers_totalExUnits
  , relay_newSingleHostAddr
  , relay_newSingleHostName
  , relay_newMultiHostName
  , relay_kind
  , relay_asSingleHostAddr
  , relay_asSingleHostName
  , relay_asMultiHostName
  , relays_new
  , rewardAddress_new
  , rewardAddress_paymentCred
  , rewardAddress_toAddress
  , rewardAddress_fromAddress
  , rewardAddresses_new
  , scriptAll_nativeScripts
  , scriptAll_new
  , scriptAny_nativeScripts
  , scriptAny_new
  , scriptDataHash_toBech32
  , scriptDataHash_fromBech32
  , scriptHash_toBech32
  , scriptHash_fromBech32
  , scriptHashes_new
  , scriptNOfK_n
  , scriptNOfK_nativeScripts
  , scriptNOfK_new
  , scriptPubkey_addrKeyhash
  , scriptPubkey_new
  , scriptRef_newNativeScript
  , scriptRef_newPlutusScript
  , scriptRef_isNativeScript
  , scriptRef_isPlutusScript
  , scriptRef_nativeScript
  , scriptRef_plutusScript
  , singleHostAddr_port
  , singleHostAddr_ipv4
  , singleHostAddr_ipv6
  , singleHostAddr_new
  , singleHostName_port
  , singleHostName_dnsName
  , singleHostName_new
  , stakeCredential_fromKeyhash
  , stakeCredential_fromScripthash
  , stakeCredential_toKeyhash
  , stakeCredential_toScripthash
  , stakeCredential_kind
  , stakeCredentials_new
  , stakeDelegation_stakeCredential
  , stakeDelegation_poolKeyhash
  , stakeDelegation_new
  , stakeDeregistration_stakeCredential
  , stakeDeregistration_new
  , stakeRegistration_stakeCredential
  , stakeRegistration_new
  , timelockExpiry_slot
  , timelockExpiry_slotBignum
  , timelockExpiry_new
  , timelockExpiry_newTimelockexpiry
  , timelockStart_slot
  , timelockStart_slotBignum
  , timelockStart_new
  , timelockStart_newTimelockstart
  , transaction_body
  , transaction_witnessSet
  , transaction_isValid
  , transaction_auxiliaryData
  , transaction_setIsValid
  , transaction_new
  , transactionBody_inputs
  , transactionBody_outputs
  , transactionBody_fee
  , transactionBody_ttl
  , transactionBody_ttlBignum
  , transactionBody_setTtl
  , transactionBody_removeTtl
  , transactionBody_setCerts
  , transactionBody_certs
  , transactionBody_setWithdrawals
  , transactionBody_withdrawals
  , transactionBody_setUpdate
  , transactionBody_update
  , transactionBody_setAuxiliaryDataHash
  , transactionBody_auxiliaryDataHash
  , transactionBody_setValidityStartInterval
  , transactionBody_setValidityStartIntervalBignum
  , transactionBody_validityStartIntervalBignum
  , transactionBody_validityStartInterval
  , transactionBody_setMint
  , transactionBody_mint
  , transactionBody_multiassets
  , transactionBody_setReferenceInputs
  , transactionBody_referenceInputs
  , transactionBody_setScriptDataHash
  , transactionBody_scriptDataHash
  , transactionBody_setCollateral
  , transactionBody_collateral
  , transactionBody_setRequiredSigners
  , transactionBody_requiredSigners
  , transactionBody_setNetworkId
  , transactionBody_networkId
  , transactionBody_setCollateralReturn
  , transactionBody_collateralReturn
  , transactionBody_setTotalCollateral
  , transactionBody_totalCollateral
  , transactionBody_new
  , transactionBody_newTxBody
  , transactionHash_toBech32
  , transactionHash_fromBech32
  , transactionInput_transactionId
  , transactionInput_index
  , transactionInput_new
  , transactionInputs_new
  , transactionInputs_toOption
  , transactionMetadatum_newMap
  , transactionMetadatum_newList
  , transactionMetadatum_newInt
  , transactionMetadatum_newBytes
  , transactionMetadatum_newText
  , transactionMetadatum_kind
  , transactionMetadatum_asMap
  , transactionMetadatum_asList
  , transactionMetadatum_asInt
  , transactionMetadatum_asBytes
  , transactionMetadatum_asText
  , transactionMetadatumLabels_new
  , transactionOutput_address
  , transactionOutput_amount
  , transactionOutput_dataHash
  , transactionOutput_plutusData
  , transactionOutput_scriptRef
  , transactionOutput_setScriptRef
  , transactionOutput_setPlutusData
  , transactionOutput_setDataHash
  , transactionOutput_hasPlutusData
  , transactionOutput_hasDataHash
  , transactionOutput_hasScriptRef
  , transactionOutput_new
  , transactionOutput_serializationFormat
  , transactionOutputs_new
  , transactionUnspentOutput_new
  , transactionUnspentOutput_input
  , transactionUnspentOutput_output
  , transactionUnspentOutputs_new
  , transactionWitnessSet_setVkeys
  , transactionWitnessSet_vkeys
  , transactionWitnessSet_setNativeScripts
  , transactionWitnessSet_nativeScripts
  , transactionWitnessSet_setBootstraps
  , transactionWitnessSet_bootstraps
  , transactionWitnessSet_setPlutusScripts
  , transactionWitnessSet_plutusScripts
  , transactionWitnessSet_setPlutusData
  , transactionWitnessSet_plutusData
  , transactionWitnessSet_setRedeemers
  , transactionWitnessSet_redeemers
  , transactionWitnessSet_new
  , url_new
  , url_url
  , unitInterval_numerator
  , unitInterval_denominator
  , unitInterval_new
  , update_proposedProtocolParameterUpdates
  , update_epoch
  , update_new
  , vrfCert_output
  , vrfCert_proof
  , vrfCert_new
  , vrfKeyHash_toBech32
  , vrfKeyHash_fromBech32
  , vrfvKey_toBech32
  , vrfvKey_fromBech32
  , value_new
  , value_newFromAssets
  , value_newWithAssets
  , value_zero
  , value_isZero
  , value_coin
  , value_setCoin
  , value_multiasset
  , value_setMultiasset
  , value_checkedAdd
  , value_checkedSub
  , value_clampedSub
  , value_compare
  , vkey_new
  , vkey_publicKey
  , vkeys_new
  , vkeywitness_new
  , vkeywitness_vkey
  , vkeywitness_signature
  , vkeywitnesses_new
  , withdrawals_new
  , hashTransaction
  , hashPlutusData
  , minAdaForOutput
  , Address
  , AssetName
  , AssetNames
  , Assets
  , AuxiliaryData
  , AuxiliaryDataHash
  , BaseAddress
  , BigInt
  , BigNum
  , Bip32PrivateKey
  , Bip32PublicKey
  , BlockHash
  , BootstrapWitness
  , BootstrapWitnesses
  , ByronAddress
  , Certificate
  , Certificates
  , ConstrPlutusData
  , CostModel
  , Costmdls
  , DNSRecordAorAAAA
  , DNSRecordSRV
  , DataCost
  , DataHash
  , DatumSource
  , Ed25519KeyHash
  , Ed25519KeyHashes
  , Ed25519Signature
  , EnterpriseAddress
  , ExUnitPrices
  , ExUnits
  , GeneralTransactionMetadata
  , GenesisDelegateHash
  , GenesisHash
  , GenesisHashes
  , GenesisKeyDelegation
  , InputWithScriptWitness
  , InputsWithScriptWitness
  , Int
  , Ipv4
  , Ipv6
  , KESSignature
  , KESVKey
  , Language
  , Languages
  , LegacyDaedalusPrivateKey
  , LinearFee
  , MIRToStakeCredentials
  , MetadataList
  , MetadataMap
  , Mint
  , MintAssets
  , MintWitness
  , MintsAssets
  , MoveInstantaneousReward
  , MoveInstantaneousRewardsCert
  , MultiAsset
  , MultiHostName
  , NativeScript
  , NativeScripts
  , NetworkId
  , NetworkInfo
  , Nonce
  , OperationalCert
  , OutputDatum
  , PlutusData
  , PlutusList
  , PlutusMap
  , PlutusScript
  , PlutusScriptSource
  , PlutusScripts
  , PlutusWitness
  , PlutusWitnesses
  , Pointer
  , PointerAddress
  , PoolMetadata
  , PoolMetadataHash
  , PoolParams
  , PoolRegistration
  , PoolRetirement
  , PrivateKey
  , ProposedProtocolParameterUpdates
  , ProtocolParamUpdate
  , ProtocolVersion
  , PublicKey
  , Redeemer
  , RedeemerTag
  , Redeemers
  , Relay
  , Relays
  , RewardAddress
  , RewardAddresses
  , ScriptAll
  , ScriptAny
  , ScriptDataHash
  , ScriptHash
  , ScriptHashes
  , ScriptNOfK
  , ScriptPubkey
  , ScriptRef
  , SingleHostAddr
  , SingleHostName
  , StakeCredential
  , StakeCredentials
  , StakeDelegation
  , StakeDeregistration
  , StakeRegistration
  , TimelockExpiry
  , TimelockStart
  , Transaction
  , TransactionBatch
  , TransactionBatchList
  , TransactionBody
  , TransactionHash
  , TransactionInput
  , TransactionInputs
  , TransactionMetadatum
  , TransactionMetadatumLabels
  , TransactionOutput
  , TransactionOutputs
  , TransactionUnspentOutput
  , TransactionUnspentOutputs
  , TransactionWitnessSet
  , URL
  , UnitInterval
  , Update
  , VRFCert
  , VRFKeyHash
  , VRFVKey
  , Value
  , Vkey
  , Vkeys
  , Vkeywitness
  , Vkeywitnesses
  , Withdrawals
  ) where
import Prelude

import Cardano.Serialization.Lib.Internal
import Cardano.Serialization.Lib.Internal
  ( class IsBytes
  , class IsCsl
  , class IsJson
  , toBytes
  , fromBytes
  , packListContainer
  , packMapContainer
  , packMapContainerFromMap
  , unpackMapContainerToMapWith
  , unpackMapContainer
  , unpackListContainer
  , cslFromAeson
  , cslToAeson
  , cslFromAesonViaBytes
  , cslToAesonViaBytes
  ) as X
import Effect
import Data.Nullable
import Aeson (Aeson, class DecodeAeson, encodeAeson, decodeAeson, class EncodeAeson, jsonToAeson, stringifyAeson)
import Data.ByteArray (ByteArray)
import Data.Argonaut (Json, JsonDecodeError(TypeMismatch), jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import Type.Proxy (Proxy(Proxy))

-- Utils for type conversions
type ForeignErrorable a =
  (String -> Either String a) -> (a -> Either String a) -> Either String a

runForeignErrorable :: forall (a :: Type). ForeignErrorable a -> Either String a
runForeignErrorable f = f Left Right

class IsStr a where
  fromStr :: String -> Maybe a
  toStr :: a -> String


-- functions
-- | Hash transaction
-- > hashTransaction txBody
foreign import hashTransaction :: TransactionBody -> TransactionHash

-- | Hash plutus data
-- > hashPlutusData plutusData
foreign import hashPlutusData :: PlutusData -> DataHash

-- | Min ada for output
-- > minAdaForOutput output dataCost
foreign import minAdaForOutput :: TransactionOutput -> DataCost -> BigNum



-- classes


-------------------------------------------------------------------------------------
-- Address

foreign import data Address :: Type

foreign import address_toBech32 :: Address -> String -> String
foreign import address_fromBech32 :: String -> Nullable Address
foreign import address_networkId :: Address -> Number

instance IsCsl Address where
  className _ = "Address"
instance IsBytes Address
instance IsJson Address
instance EncodeAeson Address where encodeAeson = cslToAeson
instance DecodeAeson Address where decodeAeson = cslFromAeson
instance Show Address where show = showViaJson

-------------------------------------------------------------------------------------
-- Asset name

foreign import data AssetName :: Type

foreign import assetName_new :: ByteArray -> AssetName
foreign import assetName_name :: AssetName -> ByteArray

instance IsCsl AssetName where
  className _ = "AssetName"
instance IsBytes AssetName
instance IsJson AssetName
instance EncodeAeson AssetName where encodeAeson = cslToAeson
instance DecodeAeson AssetName where decodeAeson = cslFromAeson
instance Show AssetName where show = showViaJson

-------------------------------------------------------------------------------------
-- Asset names

foreign import data AssetNames :: Type

foreign import assetNames_new :: Effect AssetNames

instance IsCsl AssetNames where
  className _ = "AssetNames"
instance IsBytes AssetNames
instance IsJson AssetNames
instance EncodeAeson AssetNames where encodeAeson = cslToAeson
instance DecodeAeson AssetNames where decodeAeson = cslFromAeson
instance Show AssetNames where show = showViaJson

instance IsListContainer AssetNames AssetName

-------------------------------------------------------------------------------------
-- Assets

foreign import data Assets :: Type

foreign import assets_new :: Effect Assets

instance IsCsl Assets where
  className _ = "Assets"
instance IsBytes Assets
instance IsJson Assets
instance EncodeAeson Assets where encodeAeson = cslToAeson
instance DecodeAeson Assets where decodeAeson = cslFromAeson
instance Show Assets where show = showViaJson

instance IsMapContainer Assets AssetName BigNum

-------------------------------------------------------------------------------------
-- Auxiliary data

foreign import data AuxiliaryData :: Type

foreign import auxiliaryData_new :: Effect AuxiliaryData
foreign import auxiliaryData_metadata :: AuxiliaryData -> Nullable GeneralTransactionMetadata
foreign import auxiliaryData_setMetadata :: AuxiliaryData -> GeneralTransactionMetadata -> Effect Unit
foreign import auxiliaryData_nativeScripts :: AuxiliaryData -> Nullable NativeScripts
foreign import auxiliaryData_setNativeScripts :: AuxiliaryData -> NativeScripts -> Effect Unit
foreign import auxiliaryData_plutusScripts :: AuxiliaryData -> Nullable PlutusScripts
foreign import auxiliaryData_setPlutusScripts :: AuxiliaryData -> PlutusScripts -> Effect Unit
foreign import auxiliaryData_preferAlonzoFormat :: AuxiliaryData -> Boolean
foreign import auxiliaryData_setPreferAlonzoFormat :: AuxiliaryData -> Boolean -> Effect Unit

instance IsCsl AuxiliaryData where
  className _ = "AuxiliaryData"
instance IsBytes AuxiliaryData
instance IsJson AuxiliaryData
instance EncodeAeson AuxiliaryData where encodeAeson = cslToAeson
instance DecodeAeson AuxiliaryData where decodeAeson = cslFromAeson
instance Show AuxiliaryData where show = showViaJson

-------------------------------------------------------------------------------------
-- Auxiliary data hash

foreign import data AuxiliaryDataHash :: Type

foreign import auxiliaryDataHash_toBech32 :: AuxiliaryDataHash -> String -> String
foreign import auxiliaryDataHash_fromBech32 :: String -> Nullable AuxiliaryDataHash

instance IsCsl AuxiliaryDataHash where
  className _ = "AuxiliaryDataHash"
instance IsBytes AuxiliaryDataHash
instance EncodeAeson AuxiliaryDataHash where encodeAeson = cslToAesonViaBytes
instance DecodeAeson AuxiliaryDataHash where decodeAeson = cslFromAesonViaBytes
instance Show AuxiliaryDataHash where show = showViaBytes

-------------------------------------------------------------------------------------
-- Base address

foreign import data BaseAddress :: Type

foreign import baseAddress_new :: Number -> StakeCredential -> StakeCredential -> BaseAddress
foreign import baseAddress_paymentCred :: BaseAddress -> StakeCredential
foreign import baseAddress_stakeCred :: BaseAddress -> StakeCredential
foreign import baseAddress_toAddress :: BaseAddress -> Address
foreign import baseAddress_fromAddress :: Address -> Nullable BaseAddress

instance IsCsl BaseAddress where
  className _ = "BaseAddress"

-------------------------------------------------------------------------------------
-- Big int

foreign import data BigInt :: Type

foreign import bigInt_isZero :: BigInt -> Boolean
foreign import bigInt_asU64 :: BigInt -> Nullable BigNum
foreign import bigInt_asInt :: BigInt -> Nullable Int
foreign import bigInt_fromStr :: String -> Nullable BigInt
foreign import bigInt_toStr :: BigInt -> String
foreign import bigInt_mul :: BigInt -> BigInt -> BigInt
foreign import bigInt_one :: BigInt
foreign import bigInt_increment :: BigInt -> BigInt
foreign import bigInt_divCeil :: BigInt -> BigInt -> BigInt

instance IsCsl BigInt where
  className _ = "BigInt"
instance IsBytes BigInt
instance IsJson BigInt
instance EncodeAeson BigInt where encodeAeson = cslToAeson
instance DecodeAeson BigInt where decodeAeson = cslFromAeson
instance Show BigInt where show = showViaJson

-------------------------------------------------------------------------------------
-- Big num

foreign import data BigNum :: Type

foreign import bigNum_fromStr :: String -> Nullable BigNum
foreign import bigNum_toStr :: BigNum -> String
foreign import bigNum_zero :: BigNum
foreign import bigNum_one :: BigNum
foreign import bigNum_isZero :: BigNum -> Boolean
foreign import bigNum_divFloor :: BigNum -> BigNum -> BigNum
foreign import bigNum_checkedMul :: BigNum -> BigNum -> Nullable BigNum
foreign import bigNum_checkedAdd :: BigNum -> BigNum -> Nullable BigNum
foreign import bigNum_checkedSub :: BigNum -> BigNum -> Nullable BigNum
foreign import bigNum_clampedSub :: BigNum -> BigNum -> BigNum
foreign import bigNum_compare :: BigNum -> BigNum -> Number
foreign import bigNum_lessThan :: BigNum -> BigNum -> Boolean
foreign import bigNum_maxValue :: BigNum
foreign import bigNum_max :: BigNum -> BigNum -> BigNum

instance IsCsl BigNum where
  className _ = "BigNum"
instance IsBytes BigNum
instance IsJson BigNum
instance EncodeAeson BigNum where encodeAeson = cslToAeson
instance DecodeAeson BigNum where decodeAeson = cslFromAeson
instance Show BigNum where show = showViaJson

-------------------------------------------------------------------------------------
-- Bip32 private key

foreign import data Bip32PrivateKey :: Type

foreign import bip32PrivateKey_derive :: Bip32PrivateKey -> Number -> Bip32PrivateKey
foreign import bip32PrivateKey_from128Xprv :: ByteArray -> Bip32PrivateKey
foreign import bip32PrivateKey_to128Xprv :: Bip32PrivateKey -> ByteArray
foreign import bip32PrivateKey_generateEd25519Bip32 :: Bip32PrivateKey
foreign import bip32PrivateKey_toRawKey :: Bip32PrivateKey -> PrivateKey
foreign import bip32PrivateKey_toPublic :: Bip32PrivateKey -> Bip32PublicKey
foreign import bip32PrivateKey_asBytes :: Bip32PrivateKey -> ByteArray
foreign import bip32PrivateKey_fromBech32 :: String -> Nullable Bip32PrivateKey
foreign import bip32PrivateKey_toBech32 :: Bip32PrivateKey -> String
foreign import bip32PrivateKey_fromBip39Entropy :: ByteArray -> ByteArray -> Bip32PrivateKey
foreign import bip32PrivateKey_chaincode :: Bip32PrivateKey -> ByteArray

instance IsCsl Bip32PrivateKey where
  className _ = "Bip32PrivateKey"

-------------------------------------------------------------------------------------
-- Bip32 public key

foreign import data Bip32PublicKey :: Type

foreign import bip32PublicKey_derive :: Bip32PublicKey -> Number -> Bip32PublicKey
foreign import bip32PublicKey_toRawKey :: Bip32PublicKey -> PublicKey
foreign import bip32PublicKey_asBytes :: Bip32PublicKey -> ByteArray
foreign import bip32PublicKey_fromBech32 :: String -> Nullable Bip32PublicKey
foreign import bip32PublicKey_toBech32 :: Bip32PublicKey -> String
foreign import bip32PublicKey_chaincode :: Bip32PublicKey -> ByteArray

instance IsCsl Bip32PublicKey where
  className _ = "Bip32PublicKey"

-------------------------------------------------------------------------------------
-- Block hash

foreign import data BlockHash :: Type

foreign import blockHash_toBech32 :: BlockHash -> String -> String
foreign import blockHash_fromBech32 :: String -> Nullable BlockHash

instance IsCsl BlockHash where
  className _ = "BlockHash"
instance IsBytes BlockHash
instance EncodeAeson BlockHash where encodeAeson = cslToAesonViaBytes
instance DecodeAeson BlockHash where decodeAeson = cslFromAesonViaBytes
instance Show BlockHash where show = showViaBytes

-------------------------------------------------------------------------------------
-- Bootstrap witness

foreign import data BootstrapWitness :: Type

foreign import bootstrapWitness_vkey :: BootstrapWitness -> Vkey
foreign import bootstrapWitness_signature :: BootstrapWitness -> Ed25519Signature
foreign import bootstrapWitness_chainCode :: BootstrapWitness -> ByteArray
foreign import bootstrapWitness_attributes :: BootstrapWitness -> ByteArray
foreign import bootstrapWitness_new :: Vkey -> Ed25519Signature -> ByteArray -> ByteArray -> BootstrapWitness

instance IsCsl BootstrapWitness where
  className _ = "BootstrapWitness"
instance IsBytes BootstrapWitness
instance IsJson BootstrapWitness
instance EncodeAeson BootstrapWitness where encodeAeson = cslToAeson
instance DecodeAeson BootstrapWitness where decodeAeson = cslFromAeson
instance Show BootstrapWitness where show = showViaJson

-------------------------------------------------------------------------------------
-- Bootstrap witnesses

foreign import data BootstrapWitnesses :: Type

foreign import bootstrapWitnesses_new :: Effect BootstrapWitnesses

instance IsCsl BootstrapWitnesses where
  className _ = "BootstrapWitnesses"

instance IsListContainer BootstrapWitnesses BootstrapWitness

-------------------------------------------------------------------------------------
-- Byron address

foreign import data ByronAddress :: Type

foreign import byronAddress_toBase58 :: ByronAddress -> String
foreign import byronAddress_byronProtocolMagic :: ByronAddress -> Number
foreign import byronAddress_attributes :: ByronAddress -> ByteArray
foreign import byronAddress_networkId :: ByronAddress -> Number
foreign import byronAddress_fromBase58 :: String -> Nullable ByronAddress
foreign import byronAddress_icarusFromKey :: Bip32PublicKey -> Number -> ByronAddress
foreign import byronAddress_isValid :: String -> Boolean
foreign import byronAddress_toAddress :: ByronAddress -> Address
foreign import byronAddress_fromAddress :: Address -> Nullable ByronAddress

instance IsCsl ByronAddress where
  className _ = "ByronAddress"
instance IsBytes ByronAddress
instance EncodeAeson ByronAddress where encodeAeson = cslToAesonViaBytes
instance DecodeAeson ByronAddress where decodeAeson = cslFromAesonViaBytes
instance Show ByronAddress where show = showViaBytes

-------------------------------------------------------------------------------------
-- Certificate

foreign import data Certificate :: Type

foreign import certificate_newStakeRegistration :: StakeRegistration -> Certificate
foreign import certificate_newStakeDeregistration :: StakeDeregistration -> Certificate
foreign import certificate_newStakeDelegation :: StakeDelegation -> Certificate
foreign import certificate_newPoolRegistration :: PoolRegistration -> Certificate
foreign import certificate_newPoolRetirement :: PoolRetirement -> Certificate
foreign import certificate_newGenesisKeyDelegation :: GenesisKeyDelegation -> Certificate
foreign import certificate_newMoveInstantaneousRewardsCert :: MoveInstantaneousRewardsCert -> Certificate
foreign import certificate_kind :: Certificate -> Number
foreign import certificate_asStakeRegistration :: Certificate -> Nullable StakeRegistration
foreign import certificate_asStakeDeregistration :: Certificate -> Nullable StakeDeregistration
foreign import certificate_asStakeDelegation :: Certificate -> Nullable StakeDelegation
foreign import certificate_asPoolRegistration :: Certificate -> Nullable PoolRegistration
foreign import certificate_asPoolRetirement :: Certificate -> Nullable PoolRetirement
foreign import certificate_asGenesisKeyDelegation :: Certificate -> Nullable GenesisKeyDelegation
foreign import certificate_asMoveInstantaneousRewardsCert :: Certificate -> Nullable MoveInstantaneousRewardsCert

instance IsCsl Certificate where
  className _ = "Certificate"
instance IsBytes Certificate
instance IsJson Certificate
instance EncodeAeson Certificate where encodeAeson = cslToAeson
instance DecodeAeson Certificate where decodeAeson = cslFromAeson
instance Show Certificate where show = showViaJson

-------------------------------------------------------------------------------------
-- Certificates

foreign import data Certificates :: Type

foreign import certificates_new :: Effect Certificates

instance IsCsl Certificates where
  className _ = "Certificates"
instance IsBytes Certificates
instance IsJson Certificates
instance EncodeAeson Certificates where encodeAeson = cslToAeson
instance DecodeAeson Certificates where decodeAeson = cslFromAeson
instance Show Certificates where show = showViaJson

instance IsListContainer Certificates Certificate

-------------------------------------------------------------------------------------
-- Constr plutus data

foreign import data ConstrPlutusData :: Type

foreign import constrPlutusData_alternative :: ConstrPlutusData -> BigNum
foreign import constrPlutusData_data :: ConstrPlutusData -> PlutusList
foreign import constrPlutusData_new :: BigNum -> PlutusList -> ConstrPlutusData

instance IsCsl ConstrPlutusData where
  className _ = "ConstrPlutusData"
instance IsBytes ConstrPlutusData
instance EncodeAeson ConstrPlutusData where encodeAeson = cslToAesonViaBytes
instance DecodeAeson ConstrPlutusData where decodeAeson = cslFromAesonViaBytes
instance Show ConstrPlutusData where show = showViaBytes

-------------------------------------------------------------------------------------
-- Cost model

foreign import data CostModel :: Type

foreign import costModel_free :: CostModel -> Nullable Unit
foreign import costModel_toBytes :: CostModel -> ByteArray
foreign import costModel_fromBytes :: ByteArray -> Nullable CostModel
foreign import costModel_toHex :: CostModel -> String
foreign import costModel_fromHex :: String -> Nullable CostModel
foreign import costModel_toJson :: CostModel -> String
foreign import costModel_fromJson :: String -> Nullable CostModel
foreign import costModel_new :: Effect CostModel
foreign import costModel_set :: CostModel -> Number -> Int -> Effect Int
foreign import costModel_get :: CostModel -> Number -> Effect Int
foreign import costModel_len :: CostModel -> Effect Number

instance IsCsl CostModel where
  className _ = "CostModel"
instance IsBytes CostModel
instance IsJson CostModel
instance EncodeAeson CostModel where encodeAeson = cslToAeson
instance DecodeAeson CostModel where decodeAeson = cslFromAeson
instance Show CostModel where show = showViaJson

-------------------------------------------------------------------------------------
-- Costmdls

foreign import data Costmdls :: Type

foreign import costmdls_new :: Effect Costmdls
foreign import costmdls_retainLanguageVersions :: Costmdls -> Languages -> Costmdls

instance IsCsl Costmdls where
  className _ = "Costmdls"
instance IsBytes Costmdls
instance IsJson Costmdls
instance EncodeAeson Costmdls where encodeAeson = cslToAeson
instance DecodeAeson Costmdls where decodeAeson = cslFromAeson
instance Show Costmdls where show = showViaJson

instance IsMapContainer Costmdls Language CostModel

-------------------------------------------------------------------------------------
-- DNSRecord aor aaaa

foreign import data DNSRecordAorAAAA :: Type

foreign import dnsRecordAorAAAA_new :: String -> DNSRecordAorAAAA
foreign import dnsRecordAorAAAA_record :: DNSRecordAorAAAA -> String

instance IsCsl DNSRecordAorAAAA where
  className _ = "DNSRecordAorAAAA"
instance IsBytes DNSRecordAorAAAA
instance IsJson DNSRecordAorAAAA
instance EncodeAeson DNSRecordAorAAAA where encodeAeson = cslToAeson
instance DecodeAeson DNSRecordAorAAAA where decodeAeson = cslFromAeson
instance Show DNSRecordAorAAAA where show = showViaJson

-------------------------------------------------------------------------------------
-- DNSRecord srv

foreign import data DNSRecordSRV :: Type

foreign import dnsRecordSRV_new :: String -> DNSRecordSRV
foreign import dnsRecordSRV_record :: DNSRecordSRV -> String

instance IsCsl DNSRecordSRV where
  className _ = "DNSRecordSRV"
instance IsBytes DNSRecordSRV
instance IsJson DNSRecordSRV
instance EncodeAeson DNSRecordSRV where encodeAeson = cslToAeson
instance DecodeAeson DNSRecordSRV where decodeAeson = cslFromAeson
instance Show DNSRecordSRV where show = showViaJson

-------------------------------------------------------------------------------------
-- Data cost

foreign import data DataCost :: Type

foreign import dataCost_newCoinsPerWord :: BigNum -> DataCost
foreign import dataCost_newCoinsPerByte :: BigNum -> DataCost
foreign import dataCost_coinsPerByte :: DataCost -> BigNum

instance IsCsl DataCost where
  className _ = "DataCost"

-------------------------------------------------------------------------------------
-- Data hash

foreign import data DataHash :: Type

foreign import dataHash_toBech32 :: DataHash -> String -> String
foreign import dataHash_fromBech32 :: String -> Nullable DataHash

instance IsCsl DataHash where
  className _ = "DataHash"
instance IsBytes DataHash
instance EncodeAeson DataHash where encodeAeson = cslToAesonViaBytes
instance DecodeAeson DataHash where decodeAeson = cslFromAesonViaBytes
instance Show DataHash where show = showViaBytes

-------------------------------------------------------------------------------------
-- Datum source

foreign import data DatumSource :: Type

foreign import datumSource_new :: PlutusData -> DatumSource
foreign import datumSource_newRefInput :: TransactionInput -> DatumSource

instance IsCsl DatumSource where
  className _ = "DatumSource"

-------------------------------------------------------------------------------------
-- Ed25519 key hash

foreign import data Ed25519KeyHash :: Type

foreign import ed25519KeyHash_toBech32 :: Ed25519KeyHash -> String -> String
foreign import ed25519KeyHash_fromBech32 :: String -> Nullable Ed25519KeyHash

instance IsCsl Ed25519KeyHash where
  className _ = "Ed25519KeyHash"
instance IsBytes Ed25519KeyHash
instance EncodeAeson Ed25519KeyHash where encodeAeson = cslToAesonViaBytes
instance DecodeAeson Ed25519KeyHash where decodeAeson = cslFromAesonViaBytes
instance Show Ed25519KeyHash where show = showViaBytes

-------------------------------------------------------------------------------------
-- Ed25519 key hashes

foreign import data Ed25519KeyHashes :: Type

foreign import ed25519KeyHashes_new :: Ed25519KeyHashes
foreign import ed25519KeyHashes_toOption :: Ed25519KeyHashes -> Nullable Ed25519KeyHashes

instance IsCsl Ed25519KeyHashes where
  className _ = "Ed25519KeyHashes"
instance IsBytes Ed25519KeyHashes
instance IsJson Ed25519KeyHashes
instance EncodeAeson Ed25519KeyHashes where encodeAeson = cslToAeson
instance DecodeAeson Ed25519KeyHashes where decodeAeson = cslFromAeson
instance Show Ed25519KeyHashes where show = showViaJson

instance IsListContainer Ed25519KeyHashes Ed25519KeyHash

-------------------------------------------------------------------------------------
-- Ed25519 signature

foreign import data Ed25519Signature :: Type

foreign import ed25519Signature_toBech32 :: Ed25519Signature -> String
foreign import ed25519Signature_fromBech32 :: String -> Nullable Ed25519Signature

instance IsCsl Ed25519Signature where
  className _ = "Ed25519Signature"
instance IsBytes Ed25519Signature
instance EncodeAeson Ed25519Signature where encodeAeson = cslToAesonViaBytes
instance DecodeAeson Ed25519Signature where decodeAeson = cslFromAesonViaBytes
instance Show Ed25519Signature where show = showViaBytes

-------------------------------------------------------------------------------------
-- Enterprise address

foreign import data EnterpriseAddress :: Type

foreign import enterpriseAddress_new :: Number -> StakeCredential -> EnterpriseAddress
foreign import enterpriseAddress_paymentCred :: EnterpriseAddress -> StakeCredential
foreign import enterpriseAddress_toAddress :: EnterpriseAddress -> Address
foreign import enterpriseAddress_fromAddress :: Address -> Nullable EnterpriseAddress

instance IsCsl EnterpriseAddress where
  className _ = "EnterpriseAddress"

-------------------------------------------------------------------------------------
-- Ex unit prices

foreign import data ExUnitPrices :: Type

foreign import exUnitPrices_memPrice :: ExUnitPrices -> UnitInterval
foreign import exUnitPrices_stepPrice :: ExUnitPrices -> UnitInterval
foreign import exUnitPrices_new :: UnitInterval -> UnitInterval -> ExUnitPrices

instance IsCsl ExUnitPrices where
  className _ = "ExUnitPrices"
instance IsBytes ExUnitPrices
instance IsJson ExUnitPrices
instance EncodeAeson ExUnitPrices where encodeAeson = cslToAeson
instance DecodeAeson ExUnitPrices where decodeAeson = cslFromAeson
instance Show ExUnitPrices where show = showViaJson

-------------------------------------------------------------------------------------
-- Ex units

foreign import data ExUnits :: Type

foreign import exUnits_mem :: ExUnits -> BigNum
foreign import exUnits_steps :: ExUnits -> BigNum
foreign import exUnits_new :: BigNum -> BigNum -> ExUnits

instance IsCsl ExUnits where
  className _ = "ExUnits"
instance IsBytes ExUnits
instance IsJson ExUnits
instance EncodeAeson ExUnits where encodeAeson = cslToAeson
instance DecodeAeson ExUnits where decodeAeson = cslFromAeson
instance Show ExUnits where show = showViaJson

-------------------------------------------------------------------------------------
-- General transaction metadata

foreign import data GeneralTransactionMetadata :: Type

foreign import generalTransactionMetadata_new :: Effect GeneralTransactionMetadata

instance IsCsl GeneralTransactionMetadata where
  className _ = "GeneralTransactionMetadata"
instance IsBytes GeneralTransactionMetadata
instance IsJson GeneralTransactionMetadata
instance EncodeAeson GeneralTransactionMetadata where encodeAeson = cslToAeson
instance DecodeAeson GeneralTransactionMetadata where decodeAeson = cslFromAeson
instance Show GeneralTransactionMetadata where show = showViaJson

instance IsMapContainer GeneralTransactionMetadata BigNum TransactionMetadatum

-------------------------------------------------------------------------------------
-- Genesis delegate hash

foreign import data GenesisDelegateHash :: Type

foreign import genesisDelegateHash_toBech32 :: GenesisDelegateHash -> String -> String
foreign import genesisDelegateHash_fromBech32 :: String -> Nullable GenesisDelegateHash

instance IsCsl GenesisDelegateHash where
  className _ = "GenesisDelegateHash"
instance IsBytes GenesisDelegateHash
instance EncodeAeson GenesisDelegateHash where encodeAeson = cslToAesonViaBytes
instance DecodeAeson GenesisDelegateHash where decodeAeson = cslFromAesonViaBytes
instance Show GenesisDelegateHash where show = showViaBytes

-------------------------------------------------------------------------------------
-- Genesis hash

foreign import data GenesisHash :: Type

foreign import genesisHash_toBech32 :: GenesisHash -> String -> String
foreign import genesisHash_fromBech32 :: String -> Nullable GenesisHash

instance IsCsl GenesisHash where
  className _ = "GenesisHash"
instance IsBytes GenesisHash
instance EncodeAeson GenesisHash where encodeAeson = cslToAesonViaBytes
instance DecodeAeson GenesisHash where decodeAeson = cslFromAesonViaBytes
instance Show GenesisHash where show = showViaBytes

-------------------------------------------------------------------------------------
-- Genesis hashes

foreign import data GenesisHashes :: Type

foreign import genesisHashes_new :: Effect GenesisHashes

instance IsCsl GenesisHashes where
  className _ = "GenesisHashes"
instance IsBytes GenesisHashes
instance IsJson GenesisHashes
instance EncodeAeson GenesisHashes where encodeAeson = cslToAeson
instance DecodeAeson GenesisHashes where decodeAeson = cslFromAeson
instance Show GenesisHashes where show = showViaJson

instance IsListContainer GenesisHashes GenesisHash

-------------------------------------------------------------------------------------
-- Genesis key delegation

foreign import data GenesisKeyDelegation :: Type

foreign import genesisKeyDelegation_genesishash :: GenesisKeyDelegation -> GenesisHash
foreign import genesisKeyDelegation_genesisDelegateHash :: GenesisKeyDelegation -> GenesisDelegateHash
foreign import genesisKeyDelegation_vrfKeyhash :: GenesisKeyDelegation -> VRFKeyHash
foreign import genesisKeyDelegation_new :: GenesisHash -> GenesisDelegateHash -> VRFKeyHash -> GenesisKeyDelegation

instance IsCsl GenesisKeyDelegation where
  className _ = "GenesisKeyDelegation"
instance IsBytes GenesisKeyDelegation
instance IsJson GenesisKeyDelegation
instance EncodeAeson GenesisKeyDelegation where encodeAeson = cslToAeson
instance DecodeAeson GenesisKeyDelegation where decodeAeson = cslFromAeson
instance Show GenesisKeyDelegation where show = showViaJson

-------------------------------------------------------------------------------------
-- Input with script witness

foreign import data InputWithScriptWitness :: Type

foreign import inputWithScriptWitness_newWithNativeScriptWitness :: TransactionInput -> NativeScript -> InputWithScriptWitness
foreign import inputWithScriptWitness_newWithPlutusWitness :: TransactionInput -> PlutusWitness -> InputWithScriptWitness
foreign import inputWithScriptWitness_input :: InputWithScriptWitness -> TransactionInput

instance IsCsl InputWithScriptWitness where
  className _ = "InputWithScriptWitness"

-------------------------------------------------------------------------------------
-- Inputs with script witness

foreign import data InputsWithScriptWitness :: Type

foreign import inputsWithScriptWitness_new :: InputsWithScriptWitness

instance IsCsl InputsWithScriptWitness where
  className _ = "InputsWithScriptWitness"

instance IsListContainer InputsWithScriptWitness InputWithScriptWitness

-------------------------------------------------------------------------------------
-- Int

foreign import data Int :: Type

foreign import int_new :: BigNum -> Int
foreign import int_newNegative :: BigNum -> Int
foreign import int_newI32 :: Number -> Int
foreign import int_isPositive :: Int -> Boolean
foreign import int_asPositive :: Int -> Nullable BigNum
foreign import int_asNegative :: Int -> Nullable BigNum
foreign import int_asI32 :: Int -> Nullable Number
foreign import int_asI32OrNothing :: Int -> Nullable Number
foreign import int_asI32OrFail :: Int -> Number
foreign import int_toStr :: Int -> String
foreign import int_fromStr :: String -> Nullable Int

instance IsCsl Int where
  className _ = "Int"
instance IsBytes Int
instance IsJson Int
instance EncodeAeson Int where encodeAeson = cslToAeson
instance DecodeAeson Int where decodeAeson = cslFromAeson
instance Show Int where show = showViaJson

-------------------------------------------------------------------------------------
-- Ipv4

foreign import data Ipv4 :: Type

foreign import ipv4_new :: ByteArray -> Ipv4
foreign import ipv4_ip :: Ipv4 -> ByteArray

instance IsCsl Ipv4 where
  className _ = "Ipv4"
instance IsBytes Ipv4
instance IsJson Ipv4
instance EncodeAeson Ipv4 where encodeAeson = cslToAeson
instance DecodeAeson Ipv4 where decodeAeson = cslFromAeson
instance Show Ipv4 where show = showViaJson

-------------------------------------------------------------------------------------
-- Ipv6

foreign import data Ipv6 :: Type

foreign import ipv6_new :: ByteArray -> Ipv6
foreign import ipv6_ip :: Ipv6 -> ByteArray

instance IsCsl Ipv6 where
  className _ = "Ipv6"
instance IsBytes Ipv6
instance IsJson Ipv6
instance EncodeAeson Ipv6 where encodeAeson = cslToAeson
instance DecodeAeson Ipv6 where decodeAeson = cslFromAeson
instance Show Ipv6 where show = showViaJson

-------------------------------------------------------------------------------------
-- KESSignature

foreign import data KESSignature :: Type



instance IsCsl KESSignature where
  className _ = "KESSignature"
instance IsBytes KESSignature
instance EncodeAeson KESSignature where encodeAeson = cslToAesonViaBytes
instance DecodeAeson KESSignature where decodeAeson = cslFromAesonViaBytes
instance Show KESSignature where show = showViaBytes

-------------------------------------------------------------------------------------
-- KESVKey

foreign import data KESVKey :: Type

foreign import kesvKey_toBech32 :: KESVKey -> String -> String
foreign import kesvKey_fromBech32 :: String -> Nullable KESVKey

instance IsCsl KESVKey where
  className _ = "KESVKey"
instance IsBytes KESVKey
instance EncodeAeson KESVKey where encodeAeson = cslToAesonViaBytes
instance DecodeAeson KESVKey where decodeAeson = cslFromAesonViaBytes
instance Show KESVKey where show = showViaBytes

-------------------------------------------------------------------------------------
-- Language

foreign import data Language :: Type

foreign import language_newPlutusV1 :: Language
foreign import language_newPlutusV2 :: Language
foreign import language_kind :: Language -> Number

instance IsCsl Language where
  className _ = "Language"
instance IsBytes Language
instance IsJson Language
instance EncodeAeson Language where encodeAeson = cslToAeson
instance DecodeAeson Language where decodeAeson = cslFromAeson
instance Show Language where show = showViaJson

-------------------------------------------------------------------------------------
-- Languages

foreign import data Languages :: Type

foreign import languages_new :: Effect Languages
foreign import languages_list :: Languages

instance IsCsl Languages where
  className _ = "Languages"

instance IsListContainer Languages Language

-------------------------------------------------------------------------------------
-- Legacy daedalus private key

foreign import data LegacyDaedalusPrivateKey :: Type

foreign import legacyDaedalusPrivateKey_asBytes :: LegacyDaedalusPrivateKey -> ByteArray
foreign import legacyDaedalusPrivateKey_chaincode :: LegacyDaedalusPrivateKey -> ByteArray

instance IsCsl LegacyDaedalusPrivateKey where
  className _ = "LegacyDaedalusPrivateKey"

-------------------------------------------------------------------------------------
-- Linear fee

foreign import data LinearFee :: Type

foreign import linearFee_constant :: LinearFee -> BigNum
foreign import linearFee_coefficient :: LinearFee -> BigNum
foreign import linearFee_new :: BigNum -> BigNum -> LinearFee

instance IsCsl LinearFee where
  className _ = "LinearFee"

-------------------------------------------------------------------------------------
-- MIRTo stake credentials

foreign import data MIRToStakeCredentials :: Type

foreign import mirToStakeCredentials_new :: Effect MIRToStakeCredentials

instance IsCsl MIRToStakeCredentials where
  className _ = "MIRToStakeCredentials"
instance IsBytes MIRToStakeCredentials
instance IsJson MIRToStakeCredentials
instance EncodeAeson MIRToStakeCredentials where encodeAeson = cslToAeson
instance DecodeAeson MIRToStakeCredentials where decodeAeson = cslFromAeson
instance Show MIRToStakeCredentials where show = showViaJson

instance IsMapContainer MIRToStakeCredentials StakeCredential Int

-------------------------------------------------------------------------------------
-- Metadata list

foreign import data MetadataList :: Type

foreign import metadataList_new :: Effect MetadataList

instance IsCsl MetadataList where
  className _ = "MetadataList"
instance IsBytes MetadataList
instance EncodeAeson MetadataList where encodeAeson = cslToAesonViaBytes
instance DecodeAeson MetadataList where decodeAeson = cslFromAesonViaBytes
instance Show MetadataList where show = showViaBytes

instance IsListContainer MetadataList TransactionMetadatum

-------------------------------------------------------------------------------------
-- Metadata map

foreign import data MetadataMap :: Type

foreign import metadataMap_new :: Effect MetadataMap
foreign import metadataMap_insertStr :: MetadataMap -> String -> TransactionMetadatum -> Effect ((Nullable TransactionMetadatum))
foreign import metadataMap_insertI32 :: MetadataMap -> Number -> TransactionMetadatum -> Effect ((Nullable TransactionMetadatum))
foreign import metadataMap_getStr :: MetadataMap -> String -> Effect TransactionMetadatum
foreign import metadataMap_getI32 :: MetadataMap -> Number -> Effect TransactionMetadatum
foreign import metadataMap_has :: MetadataMap -> TransactionMetadatum -> Effect Boolean

instance IsCsl MetadataMap where
  className _ = "MetadataMap"
instance IsBytes MetadataMap
instance EncodeAeson MetadataMap where encodeAeson = cslToAesonViaBytes
instance DecodeAeson MetadataMap where decodeAeson = cslFromAesonViaBytes
instance Show MetadataMap where show = showViaBytes

instance IsMapContainer MetadataMap TransactionMetadatum TransactionMetadatum

-------------------------------------------------------------------------------------
-- Mint

foreign import data Mint :: Type

foreign import mint_new :: Effect Mint
foreign import mint_newFromEntry :: ScriptHash -> MintAssets -> Effect Mint
foreign import mint_getAll :: Mint -> ScriptHash -> Nullable MintsAssets
foreign import mint_asPositiveMultiasset :: Mint -> Effect MultiAsset
foreign import mint_asNegativeMultiasset :: Mint -> Effect MultiAsset

instance IsCsl Mint where
  className _ = "Mint"
instance IsBytes Mint
instance IsJson Mint
instance EncodeAeson Mint where encodeAeson = cslToAeson
instance DecodeAeson Mint where decodeAeson = cslFromAeson
instance Show Mint where show = showViaJson

instance IsMapContainer Mint ScriptHash MintAssets

-------------------------------------------------------------------------------------
-- Mint assets

foreign import data MintAssets :: Type

foreign import mintAssets_new :: Effect MintAssets
foreign import mintAssets_newFromEntry :: AssetName -> Int -> MintAssets

instance IsCsl MintAssets where
  className _ = "MintAssets"

instance IsMapContainer MintAssets AssetName Int

-------------------------------------------------------------------------------------
-- Mint witness

foreign import data MintWitness :: Type

foreign import mintWitness_newNativeScript :: NativeScript -> MintWitness
foreign import mintWitness_newPlutusScript :: PlutusScriptSource -> Redeemer -> MintWitness

instance IsCsl MintWitness where
  className _ = "MintWitness"

-------------------------------------------------------------------------------------
-- Mints assets

foreign import data MintsAssets :: Type



instance IsCsl MintsAssets where
  className _ = "MintsAssets"

-------------------------------------------------------------------------------------
-- Move instantaneous reward

foreign import data MoveInstantaneousReward :: Type

foreign import moveInstantaneousReward_newToOtherPot :: Number -> BigNum -> MoveInstantaneousReward
foreign import moveInstantaneousReward_newToStakeCreds :: Number -> MIRToStakeCredentials -> MoveInstantaneousReward
foreign import moveInstantaneousReward_pot :: MoveInstantaneousReward -> Number
foreign import moveInstantaneousReward_kind :: MoveInstantaneousReward -> Number
foreign import moveInstantaneousReward_asToOtherPot :: MoveInstantaneousReward -> Nullable BigNum
foreign import moveInstantaneousReward_asToStakeCreds :: MoveInstantaneousReward -> Nullable MIRToStakeCredentials

instance IsCsl MoveInstantaneousReward where
  className _ = "MoveInstantaneousReward"
instance IsBytes MoveInstantaneousReward
instance IsJson MoveInstantaneousReward
instance EncodeAeson MoveInstantaneousReward where encodeAeson = cslToAeson
instance DecodeAeson MoveInstantaneousReward where decodeAeson = cslFromAeson
instance Show MoveInstantaneousReward where show = showViaJson

-------------------------------------------------------------------------------------
-- Move instantaneous rewards cert

foreign import data MoveInstantaneousRewardsCert :: Type

foreign import moveInstantaneousRewardsCert_moveInstantaneousReward :: MoveInstantaneousRewardsCert -> MoveInstantaneousReward
foreign import moveInstantaneousRewardsCert_new :: MoveInstantaneousReward -> MoveInstantaneousRewardsCert

instance IsCsl MoveInstantaneousRewardsCert where
  className _ = "MoveInstantaneousRewardsCert"
instance IsBytes MoveInstantaneousRewardsCert
instance IsJson MoveInstantaneousRewardsCert
instance EncodeAeson MoveInstantaneousRewardsCert where encodeAeson = cslToAeson
instance DecodeAeson MoveInstantaneousRewardsCert where decodeAeson = cslFromAeson
instance Show MoveInstantaneousRewardsCert where show = showViaJson

-------------------------------------------------------------------------------------
-- Multi asset

foreign import data MultiAsset :: Type

foreign import multiAsset_new :: Effect MultiAsset
foreign import multiAsset_setAsset :: MultiAsset -> ScriptHash -> AssetName -> BigNum -> Effect ((Nullable BigNum))
foreign import multiAsset_getAsset :: MultiAsset -> ScriptHash -> AssetName -> Effect BigNum
foreign import multiAsset_sub :: MultiAsset -> MultiAsset -> Effect MultiAsset

instance IsCsl MultiAsset where
  className _ = "MultiAsset"
instance IsBytes MultiAsset
instance IsJson MultiAsset
instance EncodeAeson MultiAsset where encodeAeson = cslToAeson
instance DecodeAeson MultiAsset where decodeAeson = cslFromAeson
instance Show MultiAsset where show = showViaJson

instance IsMapContainer MultiAsset ScriptHash Assets

-------------------------------------------------------------------------------------
-- Multi host name

foreign import data MultiHostName :: Type

foreign import multiHostName_dnsName :: MultiHostName -> DNSRecordSRV
foreign import multiHostName_new :: DNSRecordSRV -> MultiHostName

instance IsCsl MultiHostName where
  className _ = "MultiHostName"
instance IsBytes MultiHostName
instance IsJson MultiHostName
instance EncodeAeson MultiHostName where encodeAeson = cslToAeson
instance DecodeAeson MultiHostName where decodeAeson = cslFromAeson
instance Show MultiHostName where show = showViaJson

-------------------------------------------------------------------------------------
-- Native script

foreign import data NativeScript :: Type

foreign import nativeScript_hash :: NativeScript -> ScriptHash
foreign import nativeScript_newScriptPubkey :: ScriptPubkey -> NativeScript
foreign import nativeScript_newScriptAll :: ScriptAll -> NativeScript
foreign import nativeScript_newScriptAny :: ScriptAny -> NativeScript
foreign import nativeScript_newScriptNOfK :: ScriptNOfK -> NativeScript
foreign import nativeScript_newTimelockStart :: TimelockStart -> NativeScript
foreign import nativeScript_newTimelockExpiry :: TimelockExpiry -> NativeScript
foreign import nativeScript_kind :: NativeScript -> Number
foreign import nativeScript_asScriptPubkey :: NativeScript -> Nullable ScriptPubkey
foreign import nativeScript_asScriptAll :: NativeScript -> Nullable ScriptAll
foreign import nativeScript_asScriptAny :: NativeScript -> Nullable ScriptAny
foreign import nativeScript_asScriptNOfK :: NativeScript -> Nullable ScriptNOfK
foreign import nativeScript_asTimelockStart :: NativeScript -> Nullable TimelockStart
foreign import nativeScript_asTimelockExpiry :: NativeScript -> Nullable TimelockExpiry
foreign import nativeScript_getRequiredSigners :: NativeScript -> Ed25519KeyHashes

instance IsCsl NativeScript where
  className _ = "NativeScript"
instance IsBytes NativeScript
instance IsJson NativeScript
instance EncodeAeson NativeScript where encodeAeson = cslToAeson
instance DecodeAeson NativeScript where decodeAeson = cslFromAeson
instance Show NativeScript where show = showViaJson

-------------------------------------------------------------------------------------
-- Native scripts

foreign import data NativeScripts :: Type

foreign import nativeScripts_new :: Effect NativeScripts

instance IsCsl NativeScripts where
  className _ = "NativeScripts"

instance IsListContainer NativeScripts NativeScript

-------------------------------------------------------------------------------------
-- Network id

foreign import data NetworkId :: Type

foreign import networkId_testnet :: NetworkId
foreign import networkId_mainnet :: NetworkId
foreign import networkId_kind :: NetworkId -> Number

instance IsCsl NetworkId where
  className _ = "NetworkId"
instance IsBytes NetworkId
instance IsJson NetworkId
instance EncodeAeson NetworkId where encodeAeson = cslToAeson
instance DecodeAeson NetworkId where decodeAeson = cslFromAeson
instance Show NetworkId where show = showViaJson

-------------------------------------------------------------------------------------
-- Network info

foreign import data NetworkInfo :: Type

foreign import networkInfo_new :: Number -> Number -> NetworkInfo
foreign import networkInfo_networkId :: NetworkInfo -> Number
foreign import networkInfo_protocolMagic :: NetworkInfo -> Number
foreign import networkInfo_testnetPreview :: NetworkInfo
foreign import networkInfo_testnetPreprod :: NetworkInfo
foreign import networkInfo_testnet :: NetworkInfo
foreign import networkInfo_mainnet :: NetworkInfo

instance IsCsl NetworkInfo where
  className _ = "NetworkInfo"

-------------------------------------------------------------------------------------
-- Nonce

foreign import data Nonce :: Type

foreign import nonce_newIdentity :: Nonce
foreign import nonce_newFromHash :: ByteArray -> Nonce
foreign import nonce_getHash :: Nonce -> Nullable ByteArray

instance IsCsl Nonce where
  className _ = "Nonce"
instance IsBytes Nonce
instance IsJson Nonce
instance EncodeAeson Nonce where encodeAeson = cslToAeson
instance DecodeAeson Nonce where decodeAeson = cslFromAeson
instance Show Nonce where show = showViaJson

-------------------------------------------------------------------------------------
-- Operational cert

foreign import data OperationalCert :: Type

foreign import operationalCert_hotVkey :: OperationalCert -> KESVKey
foreign import operationalCert_sequenceNumber :: OperationalCert -> Number
foreign import operationalCert_kesPeriod :: OperationalCert -> Number
foreign import operationalCert_sigma :: OperationalCert -> Ed25519Signature
foreign import operationalCert_new :: KESVKey -> Number -> Number -> Ed25519Signature -> OperationalCert

instance IsCsl OperationalCert where
  className _ = "OperationalCert"
instance IsBytes OperationalCert
instance IsJson OperationalCert
instance EncodeAeson OperationalCert where encodeAeson = cslToAeson
instance DecodeAeson OperationalCert where decodeAeson = cslFromAeson
instance Show OperationalCert where show = showViaJson

-------------------------------------------------------------------------------------
-- Output datum

foreign import data OutputDatum :: Type

foreign import outputDatum_newDataHash :: DataHash -> OutputDatum
foreign import outputDatum_newData :: PlutusData -> OutputDatum
foreign import outputDatum_dataHash :: OutputDatum -> Nullable DataHash
foreign import outputDatum_data :: OutputDatum -> Nullable PlutusData

instance IsCsl OutputDatum where
  className _ = "OutputDatum"

-------------------------------------------------------------------------------------
-- Plutus data

foreign import data PlutusData :: Type

foreign import plutusData_newConstrPlutusData :: ConstrPlutusData -> PlutusData
foreign import plutusData_newEmptyConstrPlutusData :: BigNum -> PlutusData
foreign import plutusData_newSingleValueConstrPlutusData :: BigNum -> PlutusData -> PlutusData
foreign import plutusData_newMap :: PlutusMap -> PlutusData
foreign import plutusData_newList :: PlutusList -> PlutusData
foreign import plutusData_newInteger :: BigInt -> PlutusData
foreign import plutusData_newBytes :: ByteArray -> PlutusData
foreign import plutusData_kind :: PlutusData -> Number
foreign import plutusData_asConstrPlutusData :: PlutusData -> Nullable ConstrPlutusData
foreign import plutusData_asMap :: PlutusData -> Nullable PlutusMap
foreign import plutusData_asList :: PlutusData -> Nullable PlutusList
foreign import plutusData_asInteger :: PlutusData -> Nullable BigInt
foreign import plutusData_asBytes :: PlutusData -> Nullable ByteArray
foreign import plutusData_fromAddress :: Address -> PlutusData

instance IsCsl PlutusData where
  className _ = "PlutusData"
instance IsBytes PlutusData
instance IsJson PlutusData
instance EncodeAeson PlutusData where encodeAeson = cslToAeson
instance DecodeAeson PlutusData where decodeAeson = cslFromAeson
instance Show PlutusData where show = showViaJson

-------------------------------------------------------------------------------------
-- Plutus list

foreign import data PlutusList :: Type

foreign import plutusList_new :: Effect PlutusList

instance IsCsl PlutusList where
  className _ = "PlutusList"
instance IsBytes PlutusList
instance EncodeAeson PlutusList where encodeAeson = cslToAesonViaBytes
instance DecodeAeson PlutusList where decodeAeson = cslFromAesonViaBytes
instance Show PlutusList where show = showViaBytes

instance IsListContainer PlutusList PlutusData

-------------------------------------------------------------------------------------
-- Plutus map

foreign import data PlutusMap :: Type

foreign import plutusMap_new :: Effect PlutusMap

instance IsCsl PlutusMap where
  className _ = "PlutusMap"
instance IsBytes PlutusMap
instance EncodeAeson PlutusMap where encodeAeson = cslToAesonViaBytes
instance DecodeAeson PlutusMap where decodeAeson = cslFromAesonViaBytes
instance Show PlutusMap where show = showViaBytes

instance IsMapContainer PlutusMap PlutusData PlutusData

-------------------------------------------------------------------------------------
-- Plutus script

foreign import data PlutusScript :: Type

foreign import plutusScript_new :: ByteArray -> PlutusScript
foreign import plutusScript_newV2 :: ByteArray -> PlutusScript
foreign import plutusScript_newWithVersion :: ByteArray -> Language -> PlutusScript
foreign import plutusScript_bytes :: PlutusScript -> ByteArray
foreign import plutusScript_fromBytesV2 :: ByteArray -> PlutusScript
foreign import plutusScript_fromBytesWithVersion :: ByteArray -> Language -> PlutusScript
foreign import plutusScript_fromHexWithVersion :: String -> Language -> PlutusScript
foreign import plutusScript_hash :: PlutusScript -> ScriptHash
foreign import plutusScript_languageVersion :: PlutusScript -> Language

instance IsCsl PlutusScript where
  className _ = "PlutusScript"
instance IsBytes PlutusScript
instance EncodeAeson PlutusScript where encodeAeson = cslToAesonViaBytes
instance DecodeAeson PlutusScript where decodeAeson = cslFromAesonViaBytes
instance Show PlutusScript where show = showViaBytes

-------------------------------------------------------------------------------------
-- Plutus script source

foreign import data PlutusScriptSource :: Type

foreign import plutusScriptSource_new :: PlutusScript -> PlutusScriptSource
foreign import plutusScriptSource_newRefInput :: ScriptHash -> TransactionInput -> PlutusScriptSource
foreign import plutusScriptSource_newRefInputWithLangVer :: ScriptHash -> TransactionInput -> Language -> PlutusScriptSource

instance IsCsl PlutusScriptSource where
  className _ = "PlutusScriptSource"

-------------------------------------------------------------------------------------
-- Plutus scripts

foreign import data PlutusScripts :: Type

foreign import plutusScripts_new :: Effect PlutusScripts

instance IsCsl PlutusScripts where
  className _ = "PlutusScripts"
instance IsBytes PlutusScripts
instance IsJson PlutusScripts
instance EncodeAeson PlutusScripts where encodeAeson = cslToAeson
instance DecodeAeson PlutusScripts where decodeAeson = cslFromAeson
instance Show PlutusScripts where show = showViaJson

instance IsListContainer PlutusScripts PlutusScript

-------------------------------------------------------------------------------------
-- Plutus witness

foreign import data PlutusWitness :: Type

foreign import plutusWitness_new :: PlutusScript -> PlutusData -> Redeemer -> PlutusWitness
foreign import plutusWitness_newWithRef :: PlutusScriptSource -> DatumSource -> Redeemer -> PlutusWitness
foreign import plutusWitness_newWithoutDatum :: PlutusScript -> Redeemer -> PlutusWitness
foreign import plutusWitness_newWithRefWithoutDatum :: PlutusScriptSource -> Redeemer -> PlutusWitness
foreign import plutusWitness_script :: PlutusWitness -> Nullable PlutusScript
foreign import plutusWitness_datum :: PlutusWitness -> Nullable PlutusData
foreign import plutusWitness_redeemer :: PlutusWitness -> Redeemer

instance IsCsl PlutusWitness where
  className _ = "PlutusWitness"

-------------------------------------------------------------------------------------
-- Plutus witnesses

foreign import data PlutusWitnesses :: Type

foreign import plutusWitnesses_new :: Effect PlutusWitnesses

instance IsCsl PlutusWitnesses where
  className _ = "PlutusWitnesses"

instance IsListContainer PlutusWitnesses PlutusWitness

-------------------------------------------------------------------------------------
-- Pointer

foreign import data Pointer :: Type

foreign import pointer_new :: Number -> Number -> Number -> Pointer
foreign import pointer_newPointer :: BigNum -> BigNum -> BigNum -> Pointer
foreign import pointer_slot :: Pointer -> Number
foreign import pointer_txIndex :: Pointer -> Number
foreign import pointer_certIndex :: Pointer -> Number
foreign import pointer_slotBignum :: Pointer -> BigNum
foreign import pointer_txIndexBignum :: Pointer -> BigNum
foreign import pointer_certIndexBignum :: Pointer -> BigNum

instance IsCsl Pointer where
  className _ = "Pointer"

-------------------------------------------------------------------------------------
-- Pointer address

foreign import data PointerAddress :: Type

foreign import pointerAddress_new :: Number -> StakeCredential -> Pointer -> PointerAddress
foreign import pointerAddress_paymentCred :: PointerAddress -> StakeCredential
foreign import pointerAddress_stakePointer :: PointerAddress -> Pointer
foreign import pointerAddress_toAddress :: PointerAddress -> Address
foreign import pointerAddress_fromAddress :: Address -> Nullable PointerAddress

instance IsCsl PointerAddress where
  className _ = "PointerAddress"

-------------------------------------------------------------------------------------
-- Pool metadata

foreign import data PoolMetadata :: Type

foreign import poolMetadata_url :: PoolMetadata -> URL
foreign import poolMetadata_poolMetadataHash :: PoolMetadata -> PoolMetadataHash
foreign import poolMetadata_new :: URL -> PoolMetadataHash -> PoolMetadata

instance IsCsl PoolMetadata where
  className _ = "PoolMetadata"
instance IsBytes PoolMetadata
instance IsJson PoolMetadata
instance EncodeAeson PoolMetadata where encodeAeson = cslToAeson
instance DecodeAeson PoolMetadata where decodeAeson = cslFromAeson
instance Show PoolMetadata where show = showViaJson

-------------------------------------------------------------------------------------
-- Pool metadata hash

foreign import data PoolMetadataHash :: Type

foreign import poolMetadataHash_toBech32 :: PoolMetadataHash -> String -> String
foreign import poolMetadataHash_fromBech32 :: String -> Nullable PoolMetadataHash

instance IsCsl PoolMetadataHash where
  className _ = "PoolMetadataHash"
instance IsBytes PoolMetadataHash
instance EncodeAeson PoolMetadataHash where encodeAeson = cslToAesonViaBytes
instance DecodeAeson PoolMetadataHash where decodeAeson = cslFromAesonViaBytes
instance Show PoolMetadataHash where show = showViaBytes

-------------------------------------------------------------------------------------
-- Pool params

foreign import data PoolParams :: Type

foreign import poolParams_operator :: PoolParams -> Ed25519KeyHash
foreign import poolParams_vrfKeyhash :: PoolParams -> VRFKeyHash
foreign import poolParams_pledge :: PoolParams -> BigNum
foreign import poolParams_cost :: PoolParams -> BigNum
foreign import poolParams_margin :: PoolParams -> UnitInterval
foreign import poolParams_rewardAccount :: PoolParams -> RewardAddress
foreign import poolParams_poolOwners :: PoolParams -> Ed25519KeyHashes
foreign import poolParams_relays :: PoolParams -> Relays
foreign import poolParams_poolMetadata :: PoolParams -> Nullable PoolMetadata
foreign import poolParams_new :: Ed25519KeyHash -> VRFKeyHash -> BigNum -> BigNum -> UnitInterval -> RewardAddress -> Ed25519KeyHashes -> Relays -> PoolMetadata -> PoolParams

instance IsCsl PoolParams where
  className _ = "PoolParams"
instance IsBytes PoolParams
instance IsJson PoolParams
instance EncodeAeson PoolParams where encodeAeson = cslToAeson
instance DecodeAeson PoolParams where decodeAeson = cslFromAeson
instance Show PoolParams where show = showViaJson

-------------------------------------------------------------------------------------
-- Pool registration

foreign import data PoolRegistration :: Type

foreign import poolRegistration_poolParams :: PoolRegistration -> PoolParams
foreign import poolRegistration_new :: PoolParams -> PoolRegistration

instance IsCsl PoolRegistration where
  className _ = "PoolRegistration"
instance IsBytes PoolRegistration
instance IsJson PoolRegistration
instance EncodeAeson PoolRegistration where encodeAeson = cslToAeson
instance DecodeAeson PoolRegistration where decodeAeson = cslFromAeson
instance Show PoolRegistration where show = showViaJson

-------------------------------------------------------------------------------------
-- Pool retirement

foreign import data PoolRetirement :: Type

foreign import poolRetirement_poolKeyhash :: PoolRetirement -> Ed25519KeyHash
foreign import poolRetirement_epoch :: PoolRetirement -> Number
foreign import poolRetirement_new :: Ed25519KeyHash -> Number -> PoolRetirement

instance IsCsl PoolRetirement where
  className _ = "PoolRetirement"
instance IsBytes PoolRetirement
instance IsJson PoolRetirement
instance EncodeAeson PoolRetirement where encodeAeson = cslToAeson
instance DecodeAeson PoolRetirement where decodeAeson = cslFromAeson
instance Show PoolRetirement where show = showViaJson

-------------------------------------------------------------------------------------
-- Private key

foreign import data PrivateKey :: Type

foreign import privateKey_free :: PrivateKey -> Nullable Unit
foreign import privateKey_toPublic :: PrivateKey -> PublicKey
foreign import privateKey_generateEd25519 :: PrivateKey
foreign import privateKey_generateEd25519extended :: PrivateKey
foreign import privateKey_fromBech32 :: String -> Nullable PrivateKey
foreign import privateKey_toBech32 :: PrivateKey -> String
foreign import privateKey_asBytes :: PrivateKey -> ByteArray
foreign import privateKey_fromExtendedBytes :: ByteArray -> Nullable PrivateKey
foreign import privateKey_fromNormalBytes :: ByteArray -> Nullable PrivateKey
foreign import privateKey_sign :: PrivateKey -> ByteArray -> Ed25519Signature
foreign import privateKey_toHex :: PrivateKey -> String
foreign import privateKey_fromHex :: String -> Nullable PrivateKey

instance IsCsl PrivateKey where
  className _ = "PrivateKey"

-------------------------------------------------------------------------------------
-- Proposed protocol parameter updates

foreign import data ProposedProtocolParameterUpdates :: Type

foreign import proposedProtocolParameterUpdates_new :: Effect ProposedProtocolParameterUpdates

instance IsCsl ProposedProtocolParameterUpdates where
  className _ = "ProposedProtocolParameterUpdates"
instance IsBytes ProposedProtocolParameterUpdates
instance IsJson ProposedProtocolParameterUpdates
instance EncodeAeson ProposedProtocolParameterUpdates where encodeAeson = cslToAeson
instance DecodeAeson ProposedProtocolParameterUpdates where decodeAeson = cslFromAeson
instance Show ProposedProtocolParameterUpdates where show = showViaJson

instance IsMapContainer ProposedProtocolParameterUpdates GenesisHash ProtocolParamUpdate

-------------------------------------------------------------------------------------
-- Protocol param update

foreign import data ProtocolParamUpdate :: Type

foreign import protocolParamUpdate_setMinfeeA :: ProtocolParamUpdate -> BigNum -> Effect Unit
foreign import protocolParamUpdate_minfeeA :: ProtocolParamUpdate -> Nullable BigNum
foreign import protocolParamUpdate_setMinfeeB :: ProtocolParamUpdate -> BigNum -> Effect Unit
foreign import protocolParamUpdate_minfeeB :: ProtocolParamUpdate -> Nullable BigNum
foreign import protocolParamUpdate_setMaxBlockBodySize :: ProtocolParamUpdate -> Number -> Effect Unit
foreign import protocolParamUpdate_maxBlockBodySize :: ProtocolParamUpdate -> Nullable Number
foreign import protocolParamUpdate_setMaxTxSize :: ProtocolParamUpdate -> Number -> Effect Unit
foreign import protocolParamUpdate_maxTxSize :: ProtocolParamUpdate -> Nullable Number
foreign import protocolParamUpdate_setMaxBlockHeaderSize :: ProtocolParamUpdate -> Number -> Effect Unit
foreign import protocolParamUpdate_maxBlockHeaderSize :: ProtocolParamUpdate -> Nullable Number
foreign import protocolParamUpdate_setKeyDeposit :: ProtocolParamUpdate -> BigNum -> Effect Unit
foreign import protocolParamUpdate_keyDeposit :: ProtocolParamUpdate -> Nullable BigNum
foreign import protocolParamUpdate_setPoolDeposit :: ProtocolParamUpdate -> BigNum -> Effect Unit
foreign import protocolParamUpdate_poolDeposit :: ProtocolParamUpdate -> Nullable BigNum
foreign import protocolParamUpdate_setMaxEpoch :: ProtocolParamUpdate -> Number -> Effect Unit
foreign import protocolParamUpdate_maxEpoch :: ProtocolParamUpdate -> Nullable Number
foreign import protocolParamUpdate_setNOpt :: ProtocolParamUpdate -> Number -> Effect Unit
foreign import protocolParamUpdate_nOpt :: ProtocolParamUpdate -> Nullable Number
foreign import protocolParamUpdate_setPoolPledgeInfluence :: ProtocolParamUpdate -> UnitInterval -> Effect Unit
foreign import protocolParamUpdate_poolPledgeInfluence :: ProtocolParamUpdate -> Nullable UnitInterval
foreign import protocolParamUpdate_setExpansionRate :: ProtocolParamUpdate -> UnitInterval -> Effect Unit
foreign import protocolParamUpdate_expansionRate :: ProtocolParamUpdate -> Nullable UnitInterval
foreign import protocolParamUpdate_setTreasuryGrowthRate :: ProtocolParamUpdate -> UnitInterval -> Effect Unit
foreign import protocolParamUpdate_treasuryGrowthRate :: ProtocolParamUpdate -> Nullable UnitInterval
foreign import protocolParamUpdate_d :: ProtocolParamUpdate -> Nullable UnitInterval
foreign import protocolParamUpdate_extraEntropy :: ProtocolParamUpdate -> Nullable Nonce
foreign import protocolParamUpdate_setProtocolVersion :: ProtocolParamUpdate -> ProtocolVersion -> Effect Unit
foreign import protocolParamUpdate_protocolVersion :: ProtocolParamUpdate -> Nullable ProtocolVersion
foreign import protocolParamUpdate_setMinPoolCost :: ProtocolParamUpdate -> BigNum -> Effect Unit
foreign import protocolParamUpdate_minPoolCost :: ProtocolParamUpdate -> Nullable BigNum
foreign import protocolParamUpdate_setAdaPerUtxoByte :: ProtocolParamUpdate -> BigNum -> Effect Unit
foreign import protocolParamUpdate_adaPerUtxoByte :: ProtocolParamUpdate -> Nullable BigNum
foreign import protocolParamUpdate_setCostModels :: ProtocolParamUpdate -> Costmdls -> Effect Unit
foreign import protocolParamUpdate_costModels :: ProtocolParamUpdate -> Nullable Costmdls
foreign import protocolParamUpdate_setExecutionCosts :: ProtocolParamUpdate -> ExUnitPrices -> Effect Unit
foreign import protocolParamUpdate_executionCosts :: ProtocolParamUpdate -> Nullable ExUnitPrices
foreign import protocolParamUpdate_setMaxTxExUnits :: ProtocolParamUpdate -> ExUnits -> Effect Unit
foreign import protocolParamUpdate_maxTxExUnits :: ProtocolParamUpdate -> Nullable ExUnits
foreign import protocolParamUpdate_setMaxBlockExUnits :: ProtocolParamUpdate -> ExUnits -> Effect Unit
foreign import protocolParamUpdate_maxBlockExUnits :: ProtocolParamUpdate -> Nullable ExUnits
foreign import protocolParamUpdate_setMaxValueSize :: ProtocolParamUpdate -> Number -> Effect Unit
foreign import protocolParamUpdate_maxValueSize :: ProtocolParamUpdate -> Nullable Number
foreign import protocolParamUpdate_setCollateralPercentage :: ProtocolParamUpdate -> Number -> Effect Unit
foreign import protocolParamUpdate_collateralPercentage :: ProtocolParamUpdate -> Nullable Number
foreign import protocolParamUpdate_setMaxCollateralInputs :: ProtocolParamUpdate -> Number -> Effect Unit
foreign import protocolParamUpdate_maxCollateralInputs :: ProtocolParamUpdate -> Nullable Number
foreign import protocolParamUpdate_new :: ProtocolParamUpdate

instance IsCsl ProtocolParamUpdate where
  className _ = "ProtocolParamUpdate"
instance IsBytes ProtocolParamUpdate
instance IsJson ProtocolParamUpdate
instance EncodeAeson ProtocolParamUpdate where encodeAeson = cslToAeson
instance DecodeAeson ProtocolParamUpdate where decodeAeson = cslFromAeson
instance Show ProtocolParamUpdate where show = showViaJson

-------------------------------------------------------------------------------------
-- Protocol version

foreign import data ProtocolVersion :: Type

foreign import protocolVersion_major :: ProtocolVersion -> Number
foreign import protocolVersion_minor :: ProtocolVersion -> Number
foreign import protocolVersion_new :: Number -> Number -> ProtocolVersion

instance IsCsl ProtocolVersion where
  className _ = "ProtocolVersion"
instance IsBytes ProtocolVersion
instance IsJson ProtocolVersion
instance EncodeAeson ProtocolVersion where encodeAeson = cslToAeson
instance DecodeAeson ProtocolVersion where decodeAeson = cslFromAeson
instance Show ProtocolVersion where show = showViaJson

-------------------------------------------------------------------------------------
-- Public key

foreign import data PublicKey :: Type

foreign import publicKey_free :: PublicKey -> Nullable Unit
foreign import publicKey_fromBech32 :: String -> Nullable PublicKey
foreign import publicKey_toBech32 :: PublicKey -> String
foreign import publicKey_asBytes :: PublicKey -> ByteArray
foreign import publicKey_fromBytes :: ByteArray -> Nullable PublicKey
foreign import publicKey_verify :: PublicKey -> ByteArray -> Ed25519Signature -> Boolean
foreign import publicKey_hash :: PublicKey -> Ed25519KeyHash
foreign import publicKey_toHex :: PublicKey -> String
foreign import publicKey_fromHex :: String -> Nullable PublicKey

instance IsCsl PublicKey where
  className _ = "PublicKey"

-------------------------------------------------------------------------------------
-- Redeemer

foreign import data Redeemer :: Type

foreign import redeemer_tag :: Redeemer -> RedeemerTag
foreign import redeemer_index :: Redeemer -> BigNum
foreign import redeemer_data :: Redeemer -> PlutusData
foreign import redeemer_exUnits :: Redeemer -> ExUnits
foreign import redeemer_new :: RedeemerTag -> BigNum -> PlutusData -> ExUnits -> Redeemer

instance IsCsl Redeemer where
  className _ = "Redeemer"
instance IsBytes Redeemer
instance IsJson Redeemer
instance EncodeAeson Redeemer where encodeAeson = cslToAeson
instance DecodeAeson Redeemer where decodeAeson = cslFromAeson
instance Show Redeemer where show = showViaJson

-------------------------------------------------------------------------------------
-- Redeemer tag

foreign import data RedeemerTag :: Type

foreign import redeemerTag_newSpend :: RedeemerTag
foreign import redeemerTag_newMint :: RedeemerTag
foreign import redeemerTag_newCert :: RedeemerTag
foreign import redeemerTag_newReward :: RedeemerTag
foreign import redeemerTag_kind :: RedeemerTag -> Number

instance IsCsl RedeemerTag where
  className _ = "RedeemerTag"
instance IsBytes RedeemerTag
instance IsJson RedeemerTag
instance EncodeAeson RedeemerTag where encodeAeson = cslToAeson
instance DecodeAeson RedeemerTag where decodeAeson = cslFromAeson
instance Show RedeemerTag where show = showViaJson

-------------------------------------------------------------------------------------
-- Redeemers

foreign import data Redeemers :: Type

foreign import redeemers_new :: Effect Redeemers
foreign import redeemers_totalExUnits :: Redeemers -> ExUnits

instance IsCsl Redeemers where
  className _ = "Redeemers"
instance IsBytes Redeemers
instance IsJson Redeemers
instance EncodeAeson Redeemers where encodeAeson = cslToAeson
instance DecodeAeson Redeemers where decodeAeson = cslFromAeson
instance Show Redeemers where show = showViaJson

instance IsListContainer Redeemers Redeemer

-------------------------------------------------------------------------------------
-- Relay

foreign import data Relay :: Type

foreign import relay_newSingleHostAddr :: SingleHostAddr -> Relay
foreign import relay_newSingleHostName :: SingleHostName -> Relay
foreign import relay_newMultiHostName :: MultiHostName -> Relay
foreign import relay_kind :: Relay -> Number
foreign import relay_asSingleHostAddr :: Relay -> Nullable SingleHostAddr
foreign import relay_asSingleHostName :: Relay -> Nullable SingleHostName
foreign import relay_asMultiHostName :: Relay -> Nullable MultiHostName

instance IsCsl Relay where
  className _ = "Relay"
instance IsBytes Relay
instance IsJson Relay
instance EncodeAeson Relay where encodeAeson = cslToAeson
instance DecodeAeson Relay where decodeAeson = cslFromAeson
instance Show Relay where show = showViaJson

-------------------------------------------------------------------------------------
-- Relays

foreign import data Relays :: Type

foreign import relays_new :: Effect Relays

instance IsCsl Relays where
  className _ = "Relays"
instance IsBytes Relays
instance IsJson Relays
instance EncodeAeson Relays where encodeAeson = cslToAeson
instance DecodeAeson Relays where decodeAeson = cslFromAeson
instance Show Relays where show = showViaJson

instance IsListContainer Relays Relay

-------------------------------------------------------------------------------------
-- Reward address

foreign import data RewardAddress :: Type

foreign import rewardAddress_new :: Number -> StakeCredential -> RewardAddress
foreign import rewardAddress_paymentCred :: RewardAddress -> StakeCredential
foreign import rewardAddress_toAddress :: RewardAddress -> Address
foreign import rewardAddress_fromAddress :: Address -> Nullable RewardAddress

instance IsCsl RewardAddress where
  className _ = "RewardAddress"

-------------------------------------------------------------------------------------
-- Reward addresses

foreign import data RewardAddresses :: Type

foreign import rewardAddresses_new :: Effect RewardAddresses

instance IsCsl RewardAddresses where
  className _ = "RewardAddresses"
instance IsBytes RewardAddresses
instance IsJson RewardAddresses
instance EncodeAeson RewardAddresses where encodeAeson = cslToAeson
instance DecodeAeson RewardAddresses where decodeAeson = cslFromAeson
instance Show RewardAddresses where show = showViaJson

instance IsListContainer RewardAddresses RewardAddress

-------------------------------------------------------------------------------------
-- Script all

foreign import data ScriptAll :: Type

foreign import scriptAll_nativeScripts :: ScriptAll -> NativeScripts
foreign import scriptAll_new :: NativeScripts -> ScriptAll

instance IsCsl ScriptAll where
  className _ = "ScriptAll"
instance IsBytes ScriptAll
instance IsJson ScriptAll
instance EncodeAeson ScriptAll where encodeAeson = cslToAeson
instance DecodeAeson ScriptAll where decodeAeson = cslFromAeson
instance Show ScriptAll where show = showViaJson

-------------------------------------------------------------------------------------
-- Script any

foreign import data ScriptAny :: Type

foreign import scriptAny_nativeScripts :: ScriptAny -> NativeScripts
foreign import scriptAny_new :: NativeScripts -> ScriptAny

instance IsCsl ScriptAny where
  className _ = "ScriptAny"
instance IsBytes ScriptAny
instance IsJson ScriptAny
instance EncodeAeson ScriptAny where encodeAeson = cslToAeson
instance DecodeAeson ScriptAny where decodeAeson = cslFromAeson
instance Show ScriptAny where show = showViaJson

-------------------------------------------------------------------------------------
-- Script data hash

foreign import data ScriptDataHash :: Type

foreign import scriptDataHash_toBech32 :: ScriptDataHash -> String -> String
foreign import scriptDataHash_fromBech32 :: String -> Nullable ScriptDataHash

instance IsCsl ScriptDataHash where
  className _ = "ScriptDataHash"
instance IsBytes ScriptDataHash
instance EncodeAeson ScriptDataHash where encodeAeson = cslToAesonViaBytes
instance DecodeAeson ScriptDataHash where decodeAeson = cslFromAesonViaBytes
instance Show ScriptDataHash where show = showViaBytes

-------------------------------------------------------------------------------------
-- Script hash

foreign import data ScriptHash :: Type

foreign import scriptHash_toBech32 :: ScriptHash -> String -> String
foreign import scriptHash_fromBech32 :: String -> Nullable ScriptHash

instance IsCsl ScriptHash where
  className _ = "ScriptHash"
instance IsBytes ScriptHash
instance EncodeAeson ScriptHash where encodeAeson = cslToAesonViaBytes
instance DecodeAeson ScriptHash where decodeAeson = cslFromAesonViaBytes
instance Show ScriptHash where show = showViaBytes

-------------------------------------------------------------------------------------
-- Script hashes

foreign import data ScriptHashes :: Type

foreign import scriptHashes_new :: Effect ScriptHashes

instance IsCsl ScriptHashes where
  className _ = "ScriptHashes"
instance IsBytes ScriptHashes
instance IsJson ScriptHashes
instance EncodeAeson ScriptHashes where encodeAeson = cslToAeson
instance DecodeAeson ScriptHashes where decodeAeson = cslFromAeson
instance Show ScriptHashes where show = showViaJson

instance IsListContainer ScriptHashes ScriptHash

-------------------------------------------------------------------------------------
-- Script nOf k

foreign import data ScriptNOfK :: Type

foreign import scriptNOfK_n :: ScriptNOfK -> Number
foreign import scriptNOfK_nativeScripts :: ScriptNOfK -> NativeScripts
foreign import scriptNOfK_new :: Number -> NativeScripts -> ScriptNOfK

instance IsCsl ScriptNOfK where
  className _ = "ScriptNOfK"
instance IsBytes ScriptNOfK
instance IsJson ScriptNOfK
instance EncodeAeson ScriptNOfK where encodeAeson = cslToAeson
instance DecodeAeson ScriptNOfK where decodeAeson = cslFromAeson
instance Show ScriptNOfK where show = showViaJson

-------------------------------------------------------------------------------------
-- Script pubkey

foreign import data ScriptPubkey :: Type

foreign import scriptPubkey_addrKeyhash :: ScriptPubkey -> Ed25519KeyHash
foreign import scriptPubkey_new :: Ed25519KeyHash -> ScriptPubkey

instance IsCsl ScriptPubkey where
  className _ = "ScriptPubkey"
instance IsBytes ScriptPubkey
instance IsJson ScriptPubkey
instance EncodeAeson ScriptPubkey where encodeAeson = cslToAeson
instance DecodeAeson ScriptPubkey where decodeAeson = cslFromAeson
instance Show ScriptPubkey where show = showViaJson

-------------------------------------------------------------------------------------
-- Script ref

foreign import data ScriptRef :: Type

foreign import scriptRef_newNativeScript :: NativeScript -> ScriptRef
foreign import scriptRef_newPlutusScript :: PlutusScript -> ScriptRef
foreign import scriptRef_isNativeScript :: ScriptRef -> Boolean
foreign import scriptRef_isPlutusScript :: ScriptRef -> Boolean
foreign import scriptRef_nativeScript :: ScriptRef -> Nullable NativeScript
foreign import scriptRef_plutusScript :: ScriptRef -> Nullable PlutusScript

instance IsCsl ScriptRef where
  className _ = "ScriptRef"
instance IsBytes ScriptRef
instance IsJson ScriptRef
instance EncodeAeson ScriptRef where encodeAeson = cslToAeson
instance DecodeAeson ScriptRef where decodeAeson = cslFromAeson
instance Show ScriptRef where show = showViaJson

-------------------------------------------------------------------------------------
-- Single host addr

foreign import data SingleHostAddr :: Type

foreign import singleHostAddr_port :: SingleHostAddr -> Nullable Number
foreign import singleHostAddr_ipv4 :: SingleHostAddr -> Nullable Ipv4
foreign import singleHostAddr_ipv6 :: SingleHostAddr -> Nullable Ipv6
foreign import singleHostAddr_new :: Number -> Ipv4 -> Ipv6 -> SingleHostAddr

instance IsCsl SingleHostAddr where
  className _ = "SingleHostAddr"
instance IsBytes SingleHostAddr
instance IsJson SingleHostAddr
instance EncodeAeson SingleHostAddr where encodeAeson = cslToAeson
instance DecodeAeson SingleHostAddr where decodeAeson = cslFromAeson
instance Show SingleHostAddr where show = showViaJson

-------------------------------------------------------------------------------------
-- Single host name

foreign import data SingleHostName :: Type

foreign import singleHostName_port :: SingleHostName -> Nullable Number
foreign import singleHostName_dnsName :: SingleHostName -> DNSRecordAorAAAA
foreign import singleHostName_new :: Nullable Number -> DNSRecordAorAAAA -> SingleHostName

instance IsCsl SingleHostName where
  className _ = "SingleHostName"
instance IsBytes SingleHostName
instance IsJson SingleHostName
instance EncodeAeson SingleHostName where encodeAeson = cslToAeson
instance DecodeAeson SingleHostName where decodeAeson = cslFromAeson
instance Show SingleHostName where show = showViaJson

-------------------------------------------------------------------------------------
-- Stake credential

foreign import data StakeCredential :: Type

foreign import stakeCredential_fromKeyhash :: Ed25519KeyHash -> StakeCredential
foreign import stakeCredential_fromScripthash :: ScriptHash -> StakeCredential
foreign import stakeCredential_toKeyhash :: StakeCredential -> Nullable Ed25519KeyHash
foreign import stakeCredential_toScripthash :: StakeCredential -> Nullable ScriptHash
foreign import stakeCredential_kind :: StakeCredential -> Number

instance IsCsl StakeCredential where
  className _ = "StakeCredential"
instance IsBytes StakeCredential
instance IsJson StakeCredential
instance EncodeAeson StakeCredential where encodeAeson = cslToAeson
instance DecodeAeson StakeCredential where decodeAeson = cslFromAeson
instance Show StakeCredential where show = showViaJson

-------------------------------------------------------------------------------------
-- Stake credentials

foreign import data StakeCredentials :: Type

foreign import stakeCredentials_new :: Effect StakeCredentials

instance IsCsl StakeCredentials where
  className _ = "StakeCredentials"
instance IsBytes StakeCredentials
instance IsJson StakeCredentials
instance EncodeAeson StakeCredentials where encodeAeson = cslToAeson
instance DecodeAeson StakeCredentials where decodeAeson = cslFromAeson
instance Show StakeCredentials where show = showViaJson

instance IsListContainer StakeCredentials StakeCredential

-------------------------------------------------------------------------------------
-- Stake delegation

foreign import data StakeDelegation :: Type

foreign import stakeDelegation_stakeCredential :: StakeDelegation -> StakeCredential
foreign import stakeDelegation_poolKeyhash :: StakeDelegation -> Ed25519KeyHash
foreign import stakeDelegation_new :: StakeCredential -> Ed25519KeyHash -> StakeDelegation

instance IsCsl StakeDelegation where
  className _ = "StakeDelegation"
instance IsBytes StakeDelegation
instance IsJson StakeDelegation
instance EncodeAeson StakeDelegation where encodeAeson = cslToAeson
instance DecodeAeson StakeDelegation where decodeAeson = cslFromAeson
instance Show StakeDelegation where show = showViaJson

-------------------------------------------------------------------------------------
-- Stake deregistration

foreign import data StakeDeregistration :: Type

foreign import stakeDeregistration_stakeCredential :: StakeDeregistration -> StakeCredential
foreign import stakeDeregistration_new :: StakeCredential -> StakeDeregistration

instance IsCsl StakeDeregistration where
  className _ = "StakeDeregistration"
instance IsBytes StakeDeregistration
instance IsJson StakeDeregistration
instance EncodeAeson StakeDeregistration where encodeAeson = cslToAeson
instance DecodeAeson StakeDeregistration where decodeAeson = cslFromAeson
instance Show StakeDeregistration where show = showViaJson

-------------------------------------------------------------------------------------
-- Stake registration

foreign import data StakeRegistration :: Type

foreign import stakeRegistration_stakeCredential :: StakeRegistration -> StakeCredential
foreign import stakeRegistration_new :: StakeCredential -> StakeRegistration

instance IsCsl StakeRegistration where
  className _ = "StakeRegistration"
instance IsBytes StakeRegistration
instance IsJson StakeRegistration
instance EncodeAeson StakeRegistration where encodeAeson = cslToAeson
instance DecodeAeson StakeRegistration where decodeAeson = cslFromAeson
instance Show StakeRegistration where show = showViaJson

-------------------------------------------------------------------------------------
-- Timelock expiry

foreign import data TimelockExpiry :: Type

foreign import timelockExpiry_slot :: TimelockExpiry -> Number
foreign import timelockExpiry_slotBignum :: TimelockExpiry -> BigNum
foreign import timelockExpiry_new :: Number -> TimelockExpiry
foreign import timelockExpiry_newTimelockexpiry :: BigNum -> TimelockExpiry

instance IsCsl TimelockExpiry where
  className _ = "TimelockExpiry"
instance IsBytes TimelockExpiry
instance IsJson TimelockExpiry
instance EncodeAeson TimelockExpiry where encodeAeson = cslToAeson
instance DecodeAeson TimelockExpiry where decodeAeson = cslFromAeson
instance Show TimelockExpiry where show = showViaJson

-------------------------------------------------------------------------------------
-- Timelock start

foreign import data TimelockStart :: Type

foreign import timelockStart_slot :: TimelockStart -> Number
foreign import timelockStart_slotBignum :: TimelockStart -> BigNum
foreign import timelockStart_new :: Number -> TimelockStart
foreign import timelockStart_newTimelockstart :: BigNum -> TimelockStart

instance IsCsl TimelockStart where
  className _ = "TimelockStart"
instance IsBytes TimelockStart
instance IsJson TimelockStart
instance EncodeAeson TimelockStart where encodeAeson = cslToAeson
instance DecodeAeson TimelockStart where decodeAeson = cslFromAeson
instance Show TimelockStart where show = showViaJson

-------------------------------------------------------------------------------------
-- Transaction

foreign import data Transaction :: Type

foreign import transaction_body :: Transaction -> TransactionBody
foreign import transaction_witnessSet :: Transaction -> TransactionWitnessSet
foreign import transaction_isValid :: Transaction -> Boolean
foreign import transaction_auxiliaryData :: Transaction -> Nullable AuxiliaryData
foreign import transaction_setIsValid :: Transaction -> Boolean -> Effect Unit
foreign import transaction_new :: TransactionBody -> TransactionWitnessSet -> AuxiliaryData -> Transaction

instance IsCsl Transaction where
  className _ = "Transaction"
instance IsBytes Transaction
instance IsJson Transaction
instance EncodeAeson Transaction where encodeAeson = cslToAeson
instance DecodeAeson Transaction where decodeAeson = cslFromAeson
instance Show Transaction where show = showViaJson

-------------------------------------------------------------------------------------
-- Transaction batch

foreign import data TransactionBatch :: Type



instance IsCsl TransactionBatch where
  className _ = "TransactionBatch"

-------------------------------------------------------------------------------------
-- Transaction batch list

foreign import data TransactionBatchList :: Type



instance IsCsl TransactionBatchList where
  className _ = "TransactionBatchList"

-------------------------------------------------------------------------------------
-- Transaction body

foreign import data TransactionBody :: Type

foreign import transactionBody_inputs :: TransactionBody -> TransactionInputs
foreign import transactionBody_outputs :: TransactionBody -> TransactionOutputs
foreign import transactionBody_fee :: TransactionBody -> BigNum
foreign import transactionBody_ttl :: TransactionBody -> Nullable Number
foreign import transactionBody_ttlBignum :: TransactionBody -> Nullable BigNum
foreign import transactionBody_setTtl :: TransactionBody -> BigNum -> Effect Unit
foreign import transactionBody_removeTtl :: TransactionBody -> Nullable Unit
foreign import transactionBody_setCerts :: TransactionBody -> Certificates -> Effect Unit
foreign import transactionBody_certs :: TransactionBody -> Nullable Certificates
foreign import transactionBody_setWithdrawals :: TransactionBody -> Withdrawals -> Effect Unit
foreign import transactionBody_withdrawals :: TransactionBody -> Nullable Withdrawals
foreign import transactionBody_setUpdate :: TransactionBody -> Update -> Effect Unit
foreign import transactionBody_update :: TransactionBody -> Nullable Update
foreign import transactionBody_setAuxiliaryDataHash :: TransactionBody -> AuxiliaryDataHash -> Effect Unit
foreign import transactionBody_auxiliaryDataHash :: TransactionBody -> Nullable AuxiliaryDataHash
foreign import transactionBody_setValidityStartInterval :: TransactionBody -> Number -> Effect Unit
foreign import transactionBody_setValidityStartIntervalBignum :: TransactionBody -> BigNum -> Effect Unit
foreign import transactionBody_validityStartIntervalBignum :: TransactionBody -> Nullable BigNum
foreign import transactionBody_validityStartInterval :: TransactionBody -> Nullable Number
foreign import transactionBody_setMint :: TransactionBody -> Mint -> Effect Unit
foreign import transactionBody_mint :: TransactionBody -> Nullable Mint
foreign import transactionBody_multiassets :: TransactionBody -> Nullable Mint
foreign import transactionBody_setReferenceInputs :: TransactionBody -> TransactionInputs -> Effect Unit
foreign import transactionBody_referenceInputs :: TransactionBody -> Nullable TransactionInputs
foreign import transactionBody_setScriptDataHash :: TransactionBody -> ScriptDataHash -> Effect Unit
foreign import transactionBody_scriptDataHash :: TransactionBody -> Nullable ScriptDataHash
foreign import transactionBody_setCollateral :: TransactionBody -> TransactionInputs -> Effect Unit
foreign import transactionBody_collateral :: TransactionBody -> Nullable TransactionInputs
foreign import transactionBody_setRequiredSigners :: TransactionBody -> Ed25519KeyHashes -> Effect Unit
foreign import transactionBody_requiredSigners :: TransactionBody -> Nullable Ed25519KeyHashes
foreign import transactionBody_setNetworkId :: TransactionBody -> NetworkId -> Effect Unit
foreign import transactionBody_networkId :: TransactionBody -> Nullable NetworkId
foreign import transactionBody_setCollateralReturn :: TransactionBody -> TransactionOutput -> Effect Unit
foreign import transactionBody_collateralReturn :: TransactionBody -> Nullable TransactionOutput
foreign import transactionBody_setTotalCollateral :: TransactionBody -> BigNum -> Effect Unit
foreign import transactionBody_totalCollateral :: TransactionBody -> Nullable BigNum
foreign import transactionBody_new :: TransactionInputs -> TransactionOutputs -> BigNum -> Number -> TransactionBody
foreign import transactionBody_newTxBody :: TransactionInputs -> TransactionOutputs -> BigNum -> TransactionBody

instance IsCsl TransactionBody where
  className _ = "TransactionBody"
instance IsBytes TransactionBody
instance IsJson TransactionBody
instance EncodeAeson TransactionBody where encodeAeson = cslToAeson
instance DecodeAeson TransactionBody where decodeAeson = cslFromAeson
instance Show TransactionBody where show = showViaJson

-------------------------------------------------------------------------------------
-- Transaction hash

foreign import data TransactionHash :: Type

foreign import transactionHash_toBech32 :: TransactionHash -> String -> String
foreign import transactionHash_fromBech32 :: String -> Nullable TransactionHash

instance IsCsl TransactionHash where
  className _ = "TransactionHash"
instance IsBytes TransactionHash
instance EncodeAeson TransactionHash where encodeAeson = cslToAesonViaBytes
instance DecodeAeson TransactionHash where decodeAeson = cslFromAesonViaBytes
instance Show TransactionHash where show = showViaBytes

-------------------------------------------------------------------------------------
-- Transaction input

foreign import data TransactionInput :: Type

foreign import transactionInput_transactionId :: TransactionInput -> TransactionHash
foreign import transactionInput_index :: TransactionInput -> Number
foreign import transactionInput_new :: TransactionHash -> Number -> TransactionInput

instance IsCsl TransactionInput where
  className _ = "TransactionInput"
instance IsBytes TransactionInput
instance IsJson TransactionInput
instance EncodeAeson TransactionInput where encodeAeson = cslToAeson
instance DecodeAeson TransactionInput where decodeAeson = cslFromAeson
instance Show TransactionInput where show = showViaJson

-------------------------------------------------------------------------------------
-- Transaction inputs

foreign import data TransactionInputs :: Type

foreign import transactionInputs_new :: Effect TransactionInputs
foreign import transactionInputs_toOption :: TransactionInputs -> Nullable TransactionInputs

instance IsCsl TransactionInputs where
  className _ = "TransactionInputs"
instance IsBytes TransactionInputs
instance IsJson TransactionInputs
instance EncodeAeson TransactionInputs where encodeAeson = cslToAeson
instance DecodeAeson TransactionInputs where decodeAeson = cslFromAeson
instance Show TransactionInputs where show = showViaJson

instance IsListContainer TransactionInputs TransactionInput

-------------------------------------------------------------------------------------
-- Transaction metadatum

foreign import data TransactionMetadatum :: Type

foreign import transactionMetadatum_newMap :: MetadataMap -> TransactionMetadatum
foreign import transactionMetadatum_newList :: MetadataList -> TransactionMetadatum
foreign import transactionMetadatum_newInt :: Int -> TransactionMetadatum
foreign import transactionMetadatum_newBytes :: ByteArray -> TransactionMetadatum
foreign import transactionMetadatum_newText :: String -> TransactionMetadatum
foreign import transactionMetadatum_kind :: TransactionMetadatum -> Number
foreign import transactionMetadatum_asMap :: TransactionMetadatum -> Nullable MetadataMap
foreign import transactionMetadatum_asList :: TransactionMetadatum -> Nullable MetadataList
foreign import transactionMetadatum_asInt :: TransactionMetadatum -> Nullable Int
foreign import transactionMetadatum_asBytes :: TransactionMetadatum -> Nullable ByteArray
foreign import transactionMetadatum_asText :: TransactionMetadatum -> Nullable String

instance IsCsl TransactionMetadatum where
  className _ = "TransactionMetadatum"
instance IsBytes TransactionMetadatum
instance EncodeAeson TransactionMetadatum where encodeAeson = cslToAesonViaBytes
instance DecodeAeson TransactionMetadatum where decodeAeson = cslFromAesonViaBytes
instance Show TransactionMetadatum where show = showViaBytes

-------------------------------------------------------------------------------------
-- Transaction metadatum labels

foreign import data TransactionMetadatumLabels :: Type

foreign import transactionMetadatumLabels_new :: Effect TransactionMetadatumLabels

instance IsCsl TransactionMetadatumLabels where
  className _ = "TransactionMetadatumLabels"
instance IsBytes TransactionMetadatumLabels
instance EncodeAeson TransactionMetadatumLabels where encodeAeson = cslToAesonViaBytes
instance DecodeAeson TransactionMetadatumLabels where decodeAeson = cslFromAesonViaBytes
instance Show TransactionMetadatumLabels where show = showViaBytes

instance IsListContainer TransactionMetadatumLabels BigNum

-------------------------------------------------------------------------------------
-- Transaction output

foreign import data TransactionOutput :: Type

foreign import transactionOutput_address :: TransactionOutput -> Address
foreign import transactionOutput_amount :: TransactionOutput -> Value
foreign import transactionOutput_dataHash :: TransactionOutput -> Nullable DataHash
foreign import transactionOutput_plutusData :: TransactionOutput -> Nullable PlutusData
foreign import transactionOutput_scriptRef :: TransactionOutput -> Nullable ScriptRef
foreign import transactionOutput_setScriptRef :: TransactionOutput -> ScriptRef -> Effect Unit
foreign import transactionOutput_setPlutusData :: TransactionOutput -> PlutusData -> Effect Unit
foreign import transactionOutput_setDataHash :: TransactionOutput -> DataHash -> Effect Unit
foreign import transactionOutput_hasPlutusData :: TransactionOutput -> Boolean
foreign import transactionOutput_hasDataHash :: TransactionOutput -> Boolean
foreign import transactionOutput_hasScriptRef :: TransactionOutput -> Boolean
foreign import transactionOutput_new :: Address -> Value -> TransactionOutput
foreign import transactionOutput_serializationFormat :: TransactionOutput -> Nullable Number

instance IsCsl TransactionOutput where
  className _ = "TransactionOutput"
instance IsBytes TransactionOutput
instance IsJson TransactionOutput
instance EncodeAeson TransactionOutput where encodeAeson = cslToAeson
instance DecodeAeson TransactionOutput where decodeAeson = cslFromAeson
instance Show TransactionOutput where show = showViaJson

-------------------------------------------------------------------------------------
-- Transaction outputs

foreign import data TransactionOutputs :: Type

foreign import transactionOutputs_new :: Effect TransactionOutputs

instance IsCsl TransactionOutputs where
  className _ = "TransactionOutputs"
instance IsBytes TransactionOutputs
instance IsJson TransactionOutputs
instance EncodeAeson TransactionOutputs where encodeAeson = cslToAeson
instance DecodeAeson TransactionOutputs where decodeAeson = cslFromAeson
instance Show TransactionOutputs where show = showViaJson

instance IsListContainer TransactionOutputs TransactionOutput

-------------------------------------------------------------------------------------
-- Transaction unspent output

foreign import data TransactionUnspentOutput :: Type

foreign import transactionUnspentOutput_new :: TransactionInput -> TransactionOutput -> TransactionUnspentOutput
foreign import transactionUnspentOutput_input :: TransactionUnspentOutput -> TransactionInput
foreign import transactionUnspentOutput_output :: TransactionUnspentOutput -> TransactionOutput

instance IsCsl TransactionUnspentOutput where
  className _ = "TransactionUnspentOutput"
instance IsBytes TransactionUnspentOutput
instance IsJson TransactionUnspentOutput
instance EncodeAeson TransactionUnspentOutput where encodeAeson = cslToAeson
instance DecodeAeson TransactionUnspentOutput where decodeAeson = cslFromAeson
instance Show TransactionUnspentOutput where show = showViaJson

-------------------------------------------------------------------------------------
-- Transaction unspent outputs

foreign import data TransactionUnspentOutputs :: Type

foreign import transactionUnspentOutputs_new :: Effect TransactionUnspentOutputs

instance IsCsl TransactionUnspentOutputs where
  className _ = "TransactionUnspentOutputs"
instance IsJson TransactionUnspentOutputs
instance EncodeAeson TransactionUnspentOutputs where encodeAeson = cslToAeson
instance DecodeAeson TransactionUnspentOutputs where decodeAeson = cslFromAeson
instance Show TransactionUnspentOutputs where show = showViaJson

instance IsListContainer TransactionUnspentOutputs TransactionUnspentOutput

-------------------------------------------------------------------------------------
-- Transaction witness set

foreign import data TransactionWitnessSet :: Type

foreign import transactionWitnessSet_setVkeys :: TransactionWitnessSet -> Vkeywitnesses -> Effect Unit
foreign import transactionWitnessSet_vkeys :: TransactionWitnessSet -> Effect ((Nullable Vkeywitnesses))
foreign import transactionWitnessSet_setNativeScripts :: TransactionWitnessSet -> NativeScripts -> Effect Unit
foreign import transactionWitnessSet_nativeScripts :: TransactionWitnessSet -> Effect ((Nullable NativeScripts))
foreign import transactionWitnessSet_setBootstraps :: TransactionWitnessSet -> BootstrapWitnesses -> Effect Unit
foreign import transactionWitnessSet_bootstraps :: TransactionWitnessSet -> Effect ((Nullable BootstrapWitnesses))
foreign import transactionWitnessSet_setPlutusScripts :: TransactionWitnessSet -> PlutusScripts -> Effect Unit
foreign import transactionWitnessSet_plutusScripts :: TransactionWitnessSet -> Effect ((Nullable PlutusScripts))
foreign import transactionWitnessSet_setPlutusData :: TransactionWitnessSet -> PlutusList -> Effect Unit
foreign import transactionWitnessSet_plutusData :: TransactionWitnessSet -> Effect ((Nullable PlutusList))
foreign import transactionWitnessSet_setRedeemers :: TransactionWitnessSet -> Redeemers -> Effect Unit
foreign import transactionWitnessSet_redeemers :: TransactionWitnessSet -> Effect ((Nullable Redeemers))
foreign import transactionWitnessSet_new :: Effect TransactionWitnessSet

instance IsCsl TransactionWitnessSet where
  className _ = "TransactionWitnessSet"
instance IsBytes TransactionWitnessSet
instance IsJson TransactionWitnessSet
instance EncodeAeson TransactionWitnessSet where encodeAeson = cslToAeson
instance DecodeAeson TransactionWitnessSet where decodeAeson = cslFromAeson
instance Show TransactionWitnessSet where show = showViaJson

-------------------------------------------------------------------------------------
-- URL

foreign import data URL :: Type

foreign import url_new :: String -> URL
foreign import url_url :: URL -> String

instance IsCsl URL where
  className _ = "URL"
instance IsBytes URL
instance IsJson URL
instance EncodeAeson URL where encodeAeson = cslToAeson
instance DecodeAeson URL where decodeAeson = cslFromAeson
instance Show URL where show = showViaJson

-------------------------------------------------------------------------------------
-- Unit interval

foreign import data UnitInterval :: Type

foreign import unitInterval_numerator :: UnitInterval -> BigNum
foreign import unitInterval_denominator :: UnitInterval -> BigNum
foreign import unitInterval_new :: BigNum -> BigNum -> UnitInterval

instance IsCsl UnitInterval where
  className _ = "UnitInterval"
instance IsBytes UnitInterval
instance IsJson UnitInterval
instance EncodeAeson UnitInterval where encodeAeson = cslToAeson
instance DecodeAeson UnitInterval where decodeAeson = cslFromAeson
instance Show UnitInterval where show = showViaJson

-------------------------------------------------------------------------------------
-- Update

foreign import data Update :: Type

foreign import update_proposedProtocolParameterUpdates :: Update -> ProposedProtocolParameterUpdates
foreign import update_epoch :: Update -> Number
foreign import update_new :: ProposedProtocolParameterUpdates -> Number -> Update

instance IsCsl Update where
  className _ = "Update"
instance IsBytes Update
instance IsJson Update
instance EncodeAeson Update where encodeAeson = cslToAeson
instance DecodeAeson Update where decodeAeson = cslFromAeson
instance Show Update where show = showViaJson

-------------------------------------------------------------------------------------
-- VRFCert

foreign import data VRFCert :: Type

foreign import vrfCert_output :: VRFCert -> ByteArray
foreign import vrfCert_proof :: VRFCert -> ByteArray
foreign import vrfCert_new :: ByteArray -> ByteArray -> VRFCert

instance IsCsl VRFCert where
  className _ = "VRFCert"
instance IsBytes VRFCert
instance IsJson VRFCert
instance EncodeAeson VRFCert where encodeAeson = cslToAeson
instance DecodeAeson VRFCert where decodeAeson = cslFromAeson
instance Show VRFCert where show = showViaJson

-------------------------------------------------------------------------------------
-- VRFKey hash

foreign import data VRFKeyHash :: Type

foreign import vrfKeyHash_toBech32 :: VRFKeyHash -> String -> String
foreign import vrfKeyHash_fromBech32 :: String -> Nullable VRFKeyHash

instance IsCsl VRFKeyHash where
  className _ = "VRFKeyHash"
instance IsBytes VRFKeyHash
instance EncodeAeson VRFKeyHash where encodeAeson = cslToAesonViaBytes
instance DecodeAeson VRFKeyHash where decodeAeson = cslFromAesonViaBytes
instance Show VRFKeyHash where show = showViaBytes

-------------------------------------------------------------------------------------
-- VRFVKey

foreign import data VRFVKey :: Type

foreign import vrfvKey_toBech32 :: VRFVKey -> String -> String
foreign import vrfvKey_fromBech32 :: String -> Nullable VRFVKey

instance IsCsl VRFVKey where
  className _ = "VRFVKey"
instance IsBytes VRFVKey
instance EncodeAeson VRFVKey where encodeAeson = cslToAesonViaBytes
instance DecodeAeson VRFVKey where decodeAeson = cslFromAesonViaBytes
instance Show VRFVKey where show = showViaBytes

-------------------------------------------------------------------------------------
-- Value

foreign import data Value :: Type

foreign import value_new :: BigNum -> Value
foreign import value_newFromAssets :: MultiAsset -> Value
foreign import value_newWithAssets :: BigNum -> MultiAsset -> Value
foreign import value_zero :: Value
foreign import value_isZero :: Value -> Boolean
foreign import value_coin :: Value -> BigNum
foreign import value_setCoin :: Value -> BigNum -> Effect Unit
foreign import value_multiasset :: Value -> Nullable MultiAsset
foreign import value_setMultiasset :: Value -> MultiAsset -> Effect Unit
foreign import value_checkedAdd :: Value -> Value -> Nullable Value
foreign import value_checkedSub :: Value -> Value -> Nullable Value
foreign import value_clampedSub :: Value -> Value -> Value
foreign import value_compare :: Value -> Value -> Nullable Number

instance IsCsl Value where
  className _ = "Value"
instance IsBytes Value
instance IsJson Value
instance EncodeAeson Value where encodeAeson = cslToAeson
instance DecodeAeson Value where decodeAeson = cslFromAeson
instance Show Value where show = showViaJson

-------------------------------------------------------------------------------------
-- Vkey

foreign import data Vkey :: Type

foreign import vkey_new :: PublicKey -> Vkey
foreign import vkey_publicKey :: Vkey -> PublicKey

instance IsCsl Vkey where
  className _ = "Vkey"
instance IsBytes Vkey
instance IsJson Vkey
instance EncodeAeson Vkey where encodeAeson = cslToAeson
instance DecodeAeson Vkey where decodeAeson = cslFromAeson
instance Show Vkey where show = showViaJson

-------------------------------------------------------------------------------------
-- Vkeys

foreign import data Vkeys :: Type

foreign import vkeys_new :: Effect Vkeys

instance IsCsl Vkeys where
  className _ = "Vkeys"

instance IsListContainer Vkeys Vkey

-------------------------------------------------------------------------------------
-- Vkeywitness

foreign import data Vkeywitness :: Type

foreign import vkeywitness_new :: Vkey -> Ed25519Signature -> Vkeywitness
foreign import vkeywitness_vkey :: Vkeywitness -> Vkey
foreign import vkeywitness_signature :: Vkeywitness -> Ed25519Signature

instance IsCsl Vkeywitness where
  className _ = "Vkeywitness"
instance IsBytes Vkeywitness
instance IsJson Vkeywitness
instance EncodeAeson Vkeywitness where encodeAeson = cslToAeson
instance DecodeAeson Vkeywitness where decodeAeson = cslFromAeson
instance Show Vkeywitness where show = showViaJson

-------------------------------------------------------------------------------------
-- Vkeywitnesses

foreign import data Vkeywitnesses :: Type

foreign import vkeywitnesses_new :: Effect Vkeywitnesses

instance IsCsl Vkeywitnesses where
  className _ = "Vkeywitnesses"
instance IsBytes Vkeywitnesses
instance IsJson Vkeywitnesses
instance EncodeAeson Vkeywitnesses where encodeAeson = cslToAeson
instance DecodeAeson Vkeywitnesses where decodeAeson = cslFromAeson
instance Show Vkeywitnesses where show = showViaJson

instance IsListContainer Vkeywitnesses Vkeywitness

-------------------------------------------------------------------------------------
-- Withdrawals

foreign import data Withdrawals :: Type

foreign import withdrawals_new :: Effect Withdrawals

instance IsCsl Withdrawals where
  className _ = "Withdrawals"
instance IsBytes Withdrawals
instance IsJson Withdrawals
instance EncodeAeson Withdrawals where encodeAeson = cslToAeson
instance DecodeAeson Withdrawals where decodeAeson = cslFromAeson
instance Show Withdrawals where show = showViaJson

instance IsMapContainer Withdrawals RewardAddress BigNum

