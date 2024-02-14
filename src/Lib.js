"use strict";

import * as CSL from "@mlabs-haskell/cardano-serialization-lib-gc";

// Pass in a function and its list of arguments, that is expected to fail on evaluation, wraps in Either
function errorableToPurs(f, ...vars) {
    try {
        return f(...vars) || null;
    }
    catch (err) {
        return null;
    }
 }

// Address
export const address_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const address_fromBech32 = bech_str => errorableToPurs(CSL.Address.from_bech32, bech_str);
export const address_networkId = self => self.network_id.bind(self)();

// AssetName
export const assetName_new = name => CSL.AssetName.new(name);
export const assetName_name = self => self.name.bind(self)();

// AssetNames
export const assetNames_new = () => CSL.AssetNames.new();

// Assets
export const assets_new = () => CSL.Assets.new();

// AuxiliaryData
export const auxiliaryData_new = () => CSL.AuxiliaryData.new();
export const auxiliaryData_metadata = self => self.metadata.bind(self)();
export const auxiliaryData_setMetadata = self => metadata => () => self.set_metadata.bind(self)(metadata);
export const auxiliaryData_nativeScripts = self => self.native_scripts.bind(self)();
export const auxiliaryData_setNativeScripts = self => native_scripts => () => self.set_native_scripts.bind(self)(native_scripts);
export const auxiliaryData_plutusScripts = self => self.plutus_scripts.bind(self)();
export const auxiliaryData_setPlutusScripts = self => plutus_scripts => () => self.set_plutus_scripts.bind(self)(plutus_scripts);
export const auxiliaryData_preferAlonzoFormat = self => self.prefer_alonzo_format.bind(self)();
export const auxiliaryData_setPreferAlonzoFormat = self => prefer => () => self.set_prefer_alonzo_format.bind(self)(prefer);

// AuxiliaryDataHash
export const auxiliaryDataHash_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const auxiliaryDataHash_fromBech32 = bech_str => errorableToPurs(CSL.AuxiliaryDataHash.from_bech32, bech_str);

// BaseAddress
export const baseAddress_new = network => payment => stake => CSL.BaseAddress.new(network, payment, stake);
export const baseAddress_paymentCred = self => self.payment_cred.bind(self)();
export const baseAddress_stakeCred = self => self.stake_cred.bind(self)();
export const baseAddress_toAddress = self => self.to_address.bind(self)();
export const baseAddress_fromAddress = addr => CSL.BaseAddress.from_address(addr);

// BigInt
export const bigInt_isZero = self => self.is_zero.bind(self)();
export const bigInt_asU64 = self => self.as_u64.bind(self)();
export const bigInt_asInt = self => self.as_int.bind(self)();
export const bigInt_fromStr = text => errorableToPurs(CSL.BigInt.from_str, text);
export const bigInt_toStr = self => self.to_str.bind(self)();
export const bigInt_mul = self => other => self.mul.bind(self)(other);
export const bigInt_one = CSL.BigInt.one();
export const bigInt_increment = self => self.increment.bind(self)();
export const bigInt_divCeil = self => other => self.div_ceil.bind(self)(other);

// BigNum
export const bigNum_fromStr = string => errorableToPurs(CSL.BigNum.from_str, string);
export const bigNum_toStr = self => self.to_str.bind(self)();
export const bigNum_zero = CSL.BigNum.zero();
export const bigNum_one = CSL.BigNum.one();
export const bigNum_isZero = self => self.is_zero.bind(self)();
export const bigNum_divFloor = self => other => self.div_floor.bind(self)(other);
export const bigNum_checkedMul = self => other => errorableToPurs(self.checked_mul.bind(self), other);
export const bigNum_checkedAdd = self => other => errorableToPurs(self.checked_add.bind(self), other);
export const bigNum_checkedSub = self => other => errorableToPurs(self.checked_sub.bind(self), other);
export const bigNum_clampedSub = self => other => self.clamped_sub.bind(self)(other);
export const bigNum_compare = self => rhs_value => self.compare.bind(self)(rhs_value);
export const bigNum_lessThan = self => rhs_value => self.less_than.bind(self)(rhs_value);
export const bigNum_maxValue = CSL.BigNum.max_value();
export const bigNum_max = a => b => CSL.BigNum.max(a, b);

// Bip32PrivateKey
export const bip32PrivateKey_derive = self => index => self.derive.bind(self)(index);
export const bip32PrivateKey_from128Xprv = bytes => CSL.Bip32PrivateKey.from_128_xprv(bytes);
export const bip32PrivateKey_to128Xprv = self => self.to_128_xprv.bind(self)();
export const bip32PrivateKey_generateEd25519Bip32 = CSL.Bip32PrivateKey.generate_ed25519_bip32();
export const bip32PrivateKey_toRawKey = self => self.to_raw_key.bind(self)();
export const bip32PrivateKey_toPublic = self => self.to_public.bind(self)();
export const bip32PrivateKey_asBytes = self => self.as_bytes.bind(self)();
export const bip32PrivateKey_fromBech32 = bech32_str => errorableToPurs(CSL.Bip32PrivateKey.from_bech32, bech32_str);
export const bip32PrivateKey_toBech32 = self => self.to_bech32.bind(self)();
export const bip32PrivateKey_fromBip39Entropy = entropy => password => CSL.Bip32PrivateKey.from_bip39_entropy(entropy, password);
export const bip32PrivateKey_chaincode = self => self.chaincode.bind(self)();

// Bip32PublicKey
export const bip32PublicKey_derive = self => index => self.derive.bind(self)(index);
export const bip32PublicKey_toRawKey = self => self.to_raw_key.bind(self)();
export const bip32PublicKey_asBytes = self => self.as_bytes.bind(self)();
export const bip32PublicKey_fromBech32 = bech32_str => errorableToPurs(CSL.Bip32PublicKey.from_bech32, bech32_str);
export const bip32PublicKey_toBech32 = self => self.to_bech32.bind(self)();
export const bip32PublicKey_chaincode = self => self.chaincode.bind(self)();

// BlockHash
export const blockHash_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const blockHash_fromBech32 = bech_str => errorableToPurs(CSL.BlockHash.from_bech32, bech_str);

// BootstrapWitness
export const bootstrapWitness_vkey = self => self.vkey.bind(self)();
export const bootstrapWitness_signature = self => self.signature.bind(self)();
export const bootstrapWitness_chainCode = self => self.chain_code.bind(self)();
export const bootstrapWitness_attributes = self => self.attributes.bind(self)();
export const bootstrapWitness_new = vkey => signature => chain_code => attributes => CSL.BootstrapWitness.new(vkey, signature, chain_code, attributes);

// BootstrapWitnesses
export const bootstrapWitnesses_new = () => CSL.BootstrapWitnesses.new();

// ByronAddress
export const byronAddress_toBase58 = self => self.to_base58.bind(self)();
export const byronAddress_byronProtocolMagic = self => self.byron_protocol_magic.bind(self)();
export const byronAddress_attributes = self => self.attributes.bind(self)();
export const byronAddress_networkId = self => self.network_id.bind(self)();
export const byronAddress_fromBase58 = s => errorableToPurs(CSL.ByronAddress.from_base58, s);
export const byronAddress_icarusFromKey = key => protocol_magic => CSL.ByronAddress.icarus_from_key(key, protocol_magic);
export const byronAddress_isValid = s => CSL.ByronAddress.is_valid(s);
export const byronAddress_toAddress = self => self.to_address.bind(self)();
export const byronAddress_fromAddress = addr => CSL.ByronAddress.from_address(addr);

// Certificate
export const certificate_newStakeRegistration = stake_registration => CSL.Certificate.new_stake_registration(stake_registration);
export const certificate_newStakeDeregistration = stake_deregistration => CSL.Certificate.new_stake_deregistration(stake_deregistration);
export const certificate_newStakeDelegation = stake_delegation => CSL.Certificate.new_stake_delegation(stake_delegation);
export const certificate_newPoolRegistration = pool_registration => CSL.Certificate.new_pool_registration(pool_registration);
export const certificate_newPoolRetirement = pool_retirement => CSL.Certificate.new_pool_retirement(pool_retirement);
export const certificate_newGenesisKeyDelegation = genesis_key_delegation => CSL.Certificate.new_genesis_key_delegation(genesis_key_delegation);
export const certificate_newMoveInstantaneousRewardsCert = move_instantaneous_rewards_cert => CSL.Certificate.new_move_instantaneous_rewards_cert(move_instantaneous_rewards_cert);
export const certificate_kind = self => self.kind.bind(self)();
export const certificate_asStakeRegistration = self => self.as_stake_registration.bind(self)();
export const certificate_asStakeDeregistration = self => self.as_stake_deregistration.bind(self)();
export const certificate_asStakeDelegation = self => self.as_stake_delegation.bind(self)();
export const certificate_asPoolRegistration = self => self.as_pool_registration.bind(self)();
export const certificate_asPoolRetirement = self => self.as_pool_retirement.bind(self)();
export const certificate_asGenesisKeyDelegation = self => self.as_genesis_key_delegation.bind(self)();
export const certificate_asMoveInstantaneousRewardsCert = self => self.as_move_instantaneous_rewards_cert.bind(self)();

// Certificates
export const certificates_new = () => CSL.Certificates.new();

// ConstrPlutusData
export const constrPlutusData_alternative = self => self.alternative.bind(self)();
export const constrPlutusData_data = self => self.data.bind(self)();
export const constrPlutusData_new = alternative => data => CSL.ConstrPlutusData.new(alternative, data);

// CostModel
export const costModel_free = self => errorableToPurs(self.free.bind(self), );
export const costModel_toBytes = self => self.to_bytes.bind(self)();
export const costModel_fromBytes = bytes => errorableToPurs(CSL.CostModel.from_bytes, bytes);
export const costModel_toHex = self => self.to_hex.bind(self)();
export const costModel_fromHex = hex_str => errorableToPurs(CSL.CostModel.from_hex, hex_str);
export const costModel_toJson = self => self.to_json.bind(self)();
export const costModel_fromJson = json => errorableToPurs(CSL.CostModel.from_json, json);
export const costModel_new = () => CSL.CostModel.new();
export const costModel_set = self => operation => cost => () => self.set.bind(self)(operation, cost);
export const costModel_get = self => operation => () => self.get.bind(self)(operation);
export const costModel_len = self => () => self.len.bind(self)();

// Costmdls
export const costmdls_new = () => CSL.Costmdls.new();
export const costmdls_retainLanguageVersions = self => languages => self.retain_language_versions.bind(self)(languages);

// DNSRecordAorAAAA
export const dnsRecordAorAAAA_new = dns_name => CSL.DNSRecordAorAAAA.new(dns_name);
export const dnsRecordAorAAAA_record = self => self.record.bind(self)();

// DNSRecordSRV
export const dnsRecordSRV_new = dns_name => CSL.DNSRecordSRV.new(dns_name);
export const dnsRecordSRV_record = self => self.record.bind(self)();

// DataCost
export const dataCost_newCoinsPerWord = coins_per_word => CSL.DataCost.new_coins_per_word(coins_per_word);
export const dataCost_newCoinsPerByte = coins_per_byte => CSL.DataCost.new_coins_per_byte(coins_per_byte);
export const dataCost_coinsPerByte = self => self.coins_per_byte.bind(self)();

// DataHash
export const dataHash_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const dataHash_fromBech32 = bech_str => errorableToPurs(CSL.DataHash.from_bech32, bech_str);

// DatumSource
export const datumSource_new = datum => CSL.DatumSource.new(datum);
export const datumSource_newRefInput = input => CSL.DatumSource.new_ref_input(input);

// Ed25519KeyHash
export const ed25519KeyHash_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const ed25519KeyHash_fromBech32 = bech_str => errorableToPurs(CSL.Ed25519KeyHash.from_bech32, bech_str);

// Ed25519KeyHashes
export const ed25519KeyHashes_new = CSL.Ed25519KeyHashes.new();
export const ed25519KeyHashes_toOption = self => self.to_option.bind(self)();

// Ed25519Signature
export const ed25519Signature_toBech32 = self => self.to_bech32.bind(self)();
export const ed25519Signature_fromBech32 = bech32_str => errorableToPurs(CSL.Ed25519Signature.from_bech32, bech32_str);

// EnterpriseAddress
export const enterpriseAddress_new = network => payment => CSL.EnterpriseAddress.new(network, payment);
export const enterpriseAddress_paymentCred = self => self.payment_cred.bind(self)();
export const enterpriseAddress_toAddress = self => self.to_address.bind(self)();
export const enterpriseAddress_fromAddress = addr => CSL.EnterpriseAddress.from_address(addr);

// ExUnitPrices
export const exUnitPrices_memPrice = self => self.mem_price.bind(self)();
export const exUnitPrices_stepPrice = self => self.step_price.bind(self)();
export const exUnitPrices_new = mem_price => step_price => CSL.ExUnitPrices.new(mem_price, step_price);

// ExUnits
export const exUnits_mem = self => self.mem.bind(self)();
export const exUnits_steps = self => self.steps.bind(self)();
export const exUnits_new = mem => steps => CSL.ExUnits.new(mem, steps);

// GeneralTransactionMetadata
export const generalTransactionMetadata_new = () => CSL.GeneralTransactionMetadata.new();

// GenesisDelegateHash
export const genesisDelegateHash_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const genesisDelegateHash_fromBech32 = bech_str => errorableToPurs(CSL.GenesisDelegateHash.from_bech32, bech_str);

// GenesisHash
export const genesisHash_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const genesisHash_fromBech32 = bech_str => errorableToPurs(CSL.GenesisHash.from_bech32, bech_str);

// GenesisHashes
export const genesisHashes_new = () => CSL.GenesisHashes.new();

// GenesisKeyDelegation
export const genesisKeyDelegation_genesishash = self => self.genesishash.bind(self)();
export const genesisKeyDelegation_genesisDelegateHash = self => self.genesis_delegate_hash.bind(self)();
export const genesisKeyDelegation_vrfKeyhash = self => self.vrf_keyhash.bind(self)();
export const genesisKeyDelegation_new = genesishash => genesis_delegate_hash => vrf_keyhash => CSL.GenesisKeyDelegation.new(genesishash, genesis_delegate_hash, vrf_keyhash);

// InputWithScriptWitness
export const inputWithScriptWitness_newWithNativeScriptWitness = input => witness => CSL.InputWithScriptWitness.new_with_native_script_witness(input, witness);
export const inputWithScriptWitness_newWithPlutusWitness = input => witness => CSL.InputWithScriptWitness.new_with_plutus_witness(input, witness);
export const inputWithScriptWitness_input = self => self.input.bind(self)();

// InputsWithScriptWitness
export const inputsWithScriptWitness_new = CSL.InputsWithScriptWitness.new();

// Int
export const int_new = x => CSL.Int.new(x);
export const int_newNegative = x => CSL.Int.new_negative(x);
export const int_newI32 = x => CSL.Int.new_i32(x);
export const int_isPositive = self => self.is_positive.bind(self)();
export const int_asPositive = self => self.as_positive.bind(self)();
export const int_asNegative = self => self.as_negative.bind(self)();
export const int_asI32 = self => self.as_i32.bind(self)();
export const int_asI32OrNothing = self => self.as_i32_or_nothing.bind(self)();
export const int_asI32OrFail = self => self.as_i32_or_fail.bind(self)();
export const int_toStr = self => self.to_str.bind(self)();
export const int_fromStr = string => errorableToPurs(CSL.Int.from_str, string);

// Ipv4
export const ipv4_new = data => CSL.Ipv4.new(data);
export const ipv4_ip = self => self.ip.bind(self)();

// Ipv6
export const ipv6_new = data => CSL.Ipv6.new(data);
export const ipv6_ip = self => self.ip.bind(self)();

// KESSignature

// KESVKey
export const kesvKey_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const kesvKey_fromBech32 = bech_str => errorableToPurs(CSL.KESVKey.from_bech32, bech_str);

// Language
export const language_newPlutusV1 = CSL.Language.new_plutus_v1();
export const language_newPlutusV2 = CSL.Language.new_plutus_v2();
export const language_kind = self => self.kind.bind(self)();

// Languages
export const languages_new = () => CSL.Languages.new();
export const languages_list = CSL.Languages.list();

// LegacyDaedalusPrivateKey
export const legacyDaedalusPrivateKey_asBytes = self => self.as_bytes.bind(self)();
export const legacyDaedalusPrivateKey_chaincode = self => self.chaincode.bind(self)();

// LinearFee
export const linearFee_constant = self => self.constant.bind(self)();
export const linearFee_coefficient = self => self.coefficient.bind(self)();
export const linearFee_new = coefficient => constant => CSL.LinearFee.new(coefficient, constant);

// MIRToStakeCredentials
export const mirToStakeCredentials_new = () => CSL.MIRToStakeCredentials.new();

// MetadataList
export const metadataList_new = () => CSL.MetadataList.new();

// MetadataMap
export const metadataMap_new = () => CSL.MetadataMap.new();
export const metadataMap_insertStr = self => key => value => () => self.insert_str.bind(self)(key, value);
export const metadataMap_insertI32 = self => key => value => () => self.insert_i32.bind(self)(key, value);
export const metadataMap_getStr = self => key => () => self.get_str.bind(self)(key);
export const metadataMap_getI32 = self => key => () => self.get_i32.bind(self)(key);
export const metadataMap_has = self => key => () => self.has.bind(self)(key);

// Mint
export const mint_new = () => CSL.Mint.new();
export const mint_newFromEntry = key => value => () => CSL.Mint.new_from_entry(key, value);
export const mint_getAll = self => key => self.get_all.bind(self)(key);
export const mint_asPositiveMultiasset = self => () => self.as_positive_multiasset.bind(self)();
export const mint_asNegativeMultiasset = self => () => self.as_negative_multiasset.bind(self)();

// MintAssets
export const mintAssets_new = () => CSL.MintAssets.new();
export const mintAssets_newFromEntry = key => value => CSL.MintAssets.new_from_entry(key, value);

// MintWitness
export const mintWitness_newNativeScript = native_script => CSL.MintWitness.new_native_script(native_script);
export const mintWitness_newPlutusScript = plutus_script => redeemer => CSL.MintWitness.new_plutus_script(plutus_script, redeemer);

// MintsAssets

// MoveInstantaneousReward
export const moveInstantaneousReward_newToOtherPot = pot => amount => CSL.MoveInstantaneousReward.new_to_other_pot(pot, amount);
export const moveInstantaneousReward_newToStakeCreds = pot => amounts => CSL.MoveInstantaneousReward.new_to_stake_creds(pot, amounts);
export const moveInstantaneousReward_pot = self => self.pot.bind(self)();
export const moveInstantaneousReward_kind = self => self.kind.bind(self)();
export const moveInstantaneousReward_asToOtherPot = self => self.as_to_other_pot.bind(self)();
export const moveInstantaneousReward_asToStakeCreds = self => self.as_to_stake_creds.bind(self)();

// MoveInstantaneousRewardsCert
export const moveInstantaneousRewardsCert_moveInstantaneousReward = self => self.move_instantaneous_reward.bind(self)();
export const moveInstantaneousRewardsCert_new = move_instantaneous_reward => CSL.MoveInstantaneousRewardsCert.new(move_instantaneous_reward);

// MultiAsset
export const multiAsset_new = () => CSL.MultiAsset.new();
export const multiAsset_setAsset = self => policy_id => asset_name => value => () => self.set_asset.bind(self)(policy_id, asset_name, value);
export const multiAsset_getAsset = self => policy_id => asset_name => () => self.get_asset.bind(self)(policy_id, asset_name);
export const multiAsset_sub = self => rhs_ma => () => self.sub.bind(self)(rhs_ma);

// MultiHostName
export const multiHostName_dnsName = self => self.dns_name.bind(self)();
export const multiHostName_new = dns_name => CSL.MultiHostName.new(dns_name);

// NativeScript
export const nativeScript_hash = self => self.hash.bind(self)();
export const nativeScript_newScriptPubkey = script_pubkey => CSL.NativeScript.new_script_pubkey(script_pubkey);
export const nativeScript_newScriptAll = script_all => CSL.NativeScript.new_script_all(script_all);
export const nativeScript_newScriptAny = script_any => CSL.NativeScript.new_script_any(script_any);
export const nativeScript_newScriptNOfK = script_n_of_k => CSL.NativeScript.new_script_n_of_k(script_n_of_k);
export const nativeScript_newTimelockStart = timelock_start => CSL.NativeScript.new_timelock_start(timelock_start);
export const nativeScript_newTimelockExpiry = timelock_expiry => CSL.NativeScript.new_timelock_expiry(timelock_expiry);
export const nativeScript_kind = self => self.kind.bind(self)();
export const nativeScript_asScriptPubkey = self => self.as_script_pubkey.bind(self)();
export const nativeScript_asScriptAll = self => self.as_script_all.bind(self)();
export const nativeScript_asScriptAny = self => self.as_script_any.bind(self)();
export const nativeScript_asScriptNOfK = self => self.as_script_n_of_k.bind(self)();
export const nativeScript_asTimelockStart = self => self.as_timelock_start.bind(self)();
export const nativeScript_asTimelockExpiry = self => self.as_timelock_expiry.bind(self)();
export const nativeScript_getRequiredSigners = self => self.get_required_signers.bind(self)();

// NativeScripts
export const nativeScripts_new = () => CSL.NativeScripts.new();

// NetworkId
export const networkId_testnet = CSL.NetworkId.testnet();
export const networkId_mainnet = CSL.NetworkId.mainnet();
export const networkId_kind = self => self.kind.bind(self)();

// NetworkInfo
export const networkInfo_new = network_id => protocol_magic => CSL.NetworkInfo.new(network_id, protocol_magic);
export const networkInfo_networkId = self => self.network_id.bind(self)();
export const networkInfo_protocolMagic = self => self.protocol_magic.bind(self)();
export const networkInfo_testnetPreview = CSL.NetworkInfo.testnet_preview();
export const networkInfo_testnetPreprod = CSL.NetworkInfo.testnet_preprod();
export const networkInfo_testnet = CSL.NetworkInfo.testnet();
export const networkInfo_mainnet = CSL.NetworkInfo.mainnet();

// Nonce
export const nonce_newIdentity = CSL.Nonce.new_identity();
export const nonce_newFromHash = hash => CSL.Nonce.new_from_hash(hash);
export const nonce_getHash = self => self.get_hash.bind(self)();

// OperationalCert
export const operationalCert_hotVkey = self => self.hot_vkey.bind(self)();
export const operationalCert_sequenceNumber = self => self.sequence_number.bind(self)();
export const operationalCert_kesPeriod = self => self.kes_period.bind(self)();
export const operationalCert_sigma = self => self.sigma.bind(self)();
export const operationalCert_new = hot_vkey => sequence_number => kes_period => sigma => CSL.OperationalCert.new(hot_vkey, sequence_number, kes_period, sigma);

// OutputDatum
export const outputDatum_newDataHash = data_hash => CSL.OutputDatum.new_data_hash(data_hash);
export const outputDatum_newData = data => CSL.OutputDatum.new_data(data);
export const outputDatum_dataHash = self => self.data_hash.bind(self)();
export const outputDatum_data = self => self.data.bind(self)();

// PlutusData
export const plutusData_newConstrPlutusData = constr_plutus_data => CSL.PlutusData.new_constr_plutus_data(constr_plutus_data);
export const plutusData_newEmptyConstrPlutusData = alternative => CSL.PlutusData.new_empty_constr_plutus_data(alternative);
export const plutusData_newSingleValueConstrPlutusData = alternative => plutus_data => CSL.PlutusData.new_single_value_constr_plutus_data(alternative, plutus_data);
export const plutusData_newMap = map => CSL.PlutusData.new_map(map);
export const plutusData_newList = list => CSL.PlutusData.new_list(list);
export const plutusData_newInteger = integer => CSL.PlutusData.new_integer(integer);
export const plutusData_newBytes = bytes => CSL.PlutusData.new_bytes(bytes);
export const plutusData_kind = self => self.kind.bind(self)();
export const plutusData_asConstrPlutusData = self => self.as_constr_plutus_data.bind(self)();
export const plutusData_asMap = self => self.as_map.bind(self)();
export const plutusData_asList = self => self.as_list.bind(self)();
export const plutusData_asInteger = self => self.as_integer.bind(self)();
export const plutusData_asBytes = self => self.as_bytes.bind(self)();
export const plutusData_fromAddress = address => CSL.PlutusData.from_address(address);

// PlutusList
export const plutusList_new = () => CSL.PlutusList.new();

// PlutusMap
export const plutusMap_new = () => CSL.PlutusMap.new();

// PlutusScript
export const plutusScript_new = bytes => CSL.PlutusScript.new(bytes);
export const plutusScript_newV2 = bytes => CSL.PlutusScript.new_v2(bytes);
export const plutusScript_newWithVersion = bytes => language => CSL.PlutusScript.new_with_version(bytes, language);
export const plutusScript_bytes = self => self.bytes.bind(self)();
export const plutusScript_fromBytesV2 = bytes => CSL.PlutusScript.from_bytes_v2(bytes);
export const plutusScript_fromBytesWithVersion = bytes => language => CSL.PlutusScript.from_bytes_with_version(bytes, language);
export const plutusScript_fromHexWithVersion = hex_str => language => CSL.PlutusScript.from_hex_with_version(hex_str, language);
export const plutusScript_hash = self => self.hash.bind(self)();
export const plutusScript_languageVersion = self => self.language_version.bind(self)();

// PlutusScriptSource
export const plutusScriptSource_new = script => CSL.PlutusScriptSource.new(script);
export const plutusScriptSource_newRefInput = script_hash => input => CSL.PlutusScriptSource.new_ref_input(script_hash, input);
export const plutusScriptSource_newRefInputWithLangVer = script_hash => input => lang_ver => CSL.PlutusScriptSource.new_ref_input_with_lang_ver(script_hash, input, lang_ver);

// PlutusScripts
export const plutusScripts_new = () => CSL.PlutusScripts.new();

// PlutusWitness
export const plutusWitness_new = script => datum => redeemer => CSL.PlutusWitness.new(script, datum, redeemer);
export const plutusWitness_newWithRef = script => datum => redeemer => CSL.PlutusWitness.new_with_ref(script, datum, redeemer);
export const plutusWitness_newWithoutDatum = script => redeemer => CSL.PlutusWitness.new_without_datum(script, redeemer);
export const plutusWitness_newWithRefWithoutDatum = script => redeemer => CSL.PlutusWitness.new_with_ref_without_datum(script, redeemer);
export const plutusWitness_script = self => self.script.bind(self)();
export const plutusWitness_datum = self => self.datum.bind(self)();
export const plutusWitness_redeemer = self => self.redeemer.bind(self)();

// PlutusWitnesses
export const plutusWitnesses_new = () => CSL.PlutusWitnesses.new();

// Pointer
export const pointer_new = slot => tx_index => cert_index => CSL.Pointer.new(slot, tx_index, cert_index);
export const pointer_newPointer = slot => tx_index => cert_index => CSL.Pointer.new_pointer(slot, tx_index, cert_index);
export const pointer_slot = self => self.slot.bind(self)();
export const pointer_txIndex = self => self.tx_index.bind(self)();
export const pointer_certIndex = self => self.cert_index.bind(self)();
export const pointer_slotBignum = self => self.slot_bignum.bind(self)();
export const pointer_txIndexBignum = self => self.tx_index_bignum.bind(self)();
export const pointer_certIndexBignum = self => self.cert_index_bignum.bind(self)();

// PointerAddress
export const pointerAddress_new = network => payment => stake => CSL.PointerAddress.new(network, payment, stake);
export const pointerAddress_paymentCred = self => self.payment_cred.bind(self)();
export const pointerAddress_stakePointer = self => self.stake_pointer.bind(self)();
export const pointerAddress_toAddress = self => self.to_address.bind(self)();
export const pointerAddress_fromAddress = addr => CSL.PointerAddress.from_address(addr);

// PoolMetadata
export const poolMetadata_url = self => self.url.bind(self)();
export const poolMetadata_poolMetadataHash = self => self.pool_metadata_hash.bind(self)();
export const poolMetadata_new = url => pool_metadata_hash => CSL.PoolMetadata.new(url, pool_metadata_hash);

// PoolMetadataHash
export const poolMetadataHash_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const poolMetadataHash_fromBech32 = bech_str => errorableToPurs(CSL.PoolMetadataHash.from_bech32, bech_str);

// PoolParams
export const poolParams_operator = self => self.operator.bind(self)();
export const poolParams_vrfKeyhash = self => self.vrf_keyhash.bind(self)();
export const poolParams_pledge = self => self.pledge.bind(self)();
export const poolParams_cost = self => self.cost.bind(self)();
export const poolParams_margin = self => self.margin.bind(self)();
export const poolParams_rewardAccount = self => self.reward_account.bind(self)();
export const poolParams_poolOwners = self => self.pool_owners.bind(self)();
export const poolParams_relays = self => self.relays.bind(self)();
export const poolParams_poolMetadata = self => self.pool_metadata.bind(self)();
export const poolParams_new = operator => vrf_keyhash => pledge => cost => margin => reward_account => pool_owners => relays => pool_metadata => CSL.PoolParams.new(operator, vrf_keyhash, pledge, cost, margin, reward_account, pool_owners, relays, pool_metadata);

// PoolRegistration
export const poolRegistration_poolParams = self => self.pool_params.bind(self)();
export const poolRegistration_new = pool_params => CSL.PoolRegistration.new(pool_params);

// PoolRetirement
export const poolRetirement_poolKeyhash = self => self.pool_keyhash.bind(self)();
export const poolRetirement_epoch = self => self.epoch.bind(self)();
export const poolRetirement_new = pool_keyhash => epoch => CSL.PoolRetirement.new(pool_keyhash, epoch);

// PrivateKey
export const privateKey_free = self => errorableToPurs(self.free.bind(self), );
export const privateKey_toPublic = self => self.to_public.bind(self)();
export const privateKey_generateEd25519 = CSL.PrivateKey.generate_ed25519();
export const privateKey_generateEd25519extended = CSL.PrivateKey.generate_ed25519extended();
export const privateKey_fromBech32 = bech32_str => errorableToPurs(CSL.PrivateKey.from_bech32, bech32_str);
export const privateKey_toBech32 = self => self.to_bech32.bind(self)();
export const privateKey_asBytes = self => self.as_bytes.bind(self)();
export const privateKey_fromExtendedBytes = bytes => errorableToPurs(CSL.PrivateKey.from_extended_bytes, bytes);
export const privateKey_fromNormalBytes = bytes => errorableToPurs(CSL.PrivateKey.from_normal_bytes, bytes);
export const privateKey_sign = self => message => self.sign.bind(self)(message);
export const privateKey_toHex = self => self.to_hex.bind(self)();
export const privateKey_fromHex = hex_str => errorableToPurs(CSL.PrivateKey.from_hex, hex_str);

// ProposedProtocolParameterUpdates
export const proposedProtocolParameterUpdates_new = () => CSL.ProposedProtocolParameterUpdates.new();

// ProtocolParamUpdate
export const protocolParamUpdate_setMinfeeA = self => minfee_a => () => self.set_minfee_a.bind(self)(minfee_a);
export const protocolParamUpdate_minfeeA = self => self.minfee_a.bind(self)();
export const protocolParamUpdate_setMinfeeB = self => minfee_b => () => self.set_minfee_b.bind(self)(minfee_b);
export const protocolParamUpdate_minfeeB = self => self.minfee_b.bind(self)();
export const protocolParamUpdate_setMaxBlockBodySize = self => max_block_body_size => () => self.set_max_block_body_size.bind(self)(max_block_body_size);
export const protocolParamUpdate_maxBlockBodySize = self => self.max_block_body_size.bind(self)();
export const protocolParamUpdate_setMaxTxSize = self => max_tx_size => () => self.set_max_tx_size.bind(self)(max_tx_size);
export const protocolParamUpdate_maxTxSize = self => self.max_tx_size.bind(self)();
export const protocolParamUpdate_setMaxBlockHeaderSize = self => max_block_header_size => () => self.set_max_block_header_size.bind(self)(max_block_header_size);
export const protocolParamUpdate_maxBlockHeaderSize = self => self.max_block_header_size.bind(self)();
export const protocolParamUpdate_setKeyDeposit = self => key_deposit => () => self.set_key_deposit.bind(self)(key_deposit);
export const protocolParamUpdate_keyDeposit = self => self.key_deposit.bind(self)();
export const protocolParamUpdate_setPoolDeposit = self => pool_deposit => () => self.set_pool_deposit.bind(self)(pool_deposit);
export const protocolParamUpdate_poolDeposit = self => self.pool_deposit.bind(self)();
export const protocolParamUpdate_setMaxEpoch = self => max_epoch => () => self.set_max_epoch.bind(self)(max_epoch);
export const protocolParamUpdate_maxEpoch = self => self.max_epoch.bind(self)();
export const protocolParamUpdate_setNOpt = self => n_opt => () => self.set_n_opt.bind(self)(n_opt);
export const protocolParamUpdate_nOpt = self => self.n_opt.bind(self)();
export const protocolParamUpdate_setPoolPledgeInfluence = self => pool_pledge_influence => () => self.set_pool_pledge_influence.bind(self)(pool_pledge_influence);
export const protocolParamUpdate_poolPledgeInfluence = self => self.pool_pledge_influence.bind(self)();
export const protocolParamUpdate_setExpansionRate = self => expansion_rate => () => self.set_expansion_rate.bind(self)(expansion_rate);
export const protocolParamUpdate_expansionRate = self => self.expansion_rate.bind(self)();
export const protocolParamUpdate_setTreasuryGrowthRate = self => treasury_growth_rate => () => self.set_treasury_growth_rate.bind(self)(treasury_growth_rate);
export const protocolParamUpdate_treasuryGrowthRate = self => self.treasury_growth_rate.bind(self)();
export const protocolParamUpdate_d = self => self.d.bind(self)();
export const protocolParamUpdate_extraEntropy = self => self.extra_entropy.bind(self)();
export const protocolParamUpdate_setProtocolVersion = self => protocol_version => () => self.set_protocol_version.bind(self)(protocol_version);
export const protocolParamUpdate_protocolVersion = self => self.protocol_version.bind(self)();
export const protocolParamUpdate_setMinPoolCost = self => min_pool_cost => () => self.set_min_pool_cost.bind(self)(min_pool_cost);
export const protocolParamUpdate_minPoolCost = self => self.min_pool_cost.bind(self)();
export const protocolParamUpdate_setAdaPerUtxoByte = self => ada_per_utxo_byte => () => self.set_ada_per_utxo_byte.bind(self)(ada_per_utxo_byte);
export const protocolParamUpdate_adaPerUtxoByte = self => self.ada_per_utxo_byte.bind(self)();
export const protocolParamUpdate_setCostModels = self => cost_models => () => self.set_cost_models.bind(self)(cost_models);
export const protocolParamUpdate_costModels = self => self.cost_models.bind(self)();
export const protocolParamUpdate_setExecutionCosts = self => execution_costs => () => self.set_execution_costs.bind(self)(execution_costs);
export const protocolParamUpdate_executionCosts = self => self.execution_costs.bind(self)();
export const protocolParamUpdate_setMaxTxExUnits = self => max_tx_ex_units => () => self.set_max_tx_ex_units.bind(self)(max_tx_ex_units);
export const protocolParamUpdate_maxTxExUnits = self => self.max_tx_ex_units.bind(self)();
export const protocolParamUpdate_setMaxBlockExUnits = self => max_block_ex_units => () => self.set_max_block_ex_units.bind(self)(max_block_ex_units);
export const protocolParamUpdate_maxBlockExUnits = self => self.max_block_ex_units.bind(self)();
export const protocolParamUpdate_setMaxValueSize = self => max_value_size => () => self.set_max_value_size.bind(self)(max_value_size);
export const protocolParamUpdate_maxValueSize = self => self.max_value_size.bind(self)();
export const protocolParamUpdate_setCollateralPercentage = self => collateral_percentage => () => self.set_collateral_percentage.bind(self)(collateral_percentage);
export const protocolParamUpdate_collateralPercentage = self => self.collateral_percentage.bind(self)();
export const protocolParamUpdate_setMaxCollateralInputs = self => max_collateral_inputs => () => self.set_max_collateral_inputs.bind(self)(max_collateral_inputs);
export const protocolParamUpdate_maxCollateralInputs = self => self.max_collateral_inputs.bind(self)();
export const protocolParamUpdate_new = CSL.ProtocolParamUpdate.new();

// ProtocolVersion
export const protocolVersion_major = self => self.major.bind(self)();
export const protocolVersion_minor = self => self.minor.bind(self)();
export const protocolVersion_new = major => minor => CSL.ProtocolVersion.new(major, minor);

// PublicKey
export const publicKey_free = self => errorableToPurs(self.free.bind(self), );
export const publicKey_fromBech32 = bech32_str => errorableToPurs(CSL.PublicKey.from_bech32, bech32_str);
export const publicKey_toBech32 = self => self.to_bech32.bind(self)();
export const publicKey_asBytes = self => self.as_bytes.bind(self)();
export const publicKey_fromBytes = bytes => errorableToPurs(CSL.PublicKey.from_bytes, bytes);
export const publicKey_verify = self => data => signature => self.verify.bind(self)(data, signature);
export const publicKey_hash = self => self.hash.bind(self)();
export const publicKey_toHex = self => self.to_hex.bind(self)();
export const publicKey_fromHex = hex_str => errorableToPurs(CSL.PublicKey.from_hex, hex_str);

// Redeemer
export const redeemer_tag = self => self.tag.bind(self)();
export const redeemer_index = self => self.index.bind(self)();
export const redeemer_data = self => self.data.bind(self)();
export const redeemer_exUnits = self => self.ex_units.bind(self)();
export const redeemer_new = tag => index => data => ex_units => CSL.Redeemer.new(tag, index, data, ex_units);

// RedeemerTag
export const redeemerTag_newSpend = CSL.RedeemerTag.new_spend();
export const redeemerTag_newMint = CSL.RedeemerTag.new_mint();
export const redeemerTag_newCert = CSL.RedeemerTag.new_cert();
export const redeemerTag_newReward = CSL.RedeemerTag.new_reward();
export const redeemerTag_kind = self => self.kind.bind(self)();

// Redeemers
export const redeemers_new = () => CSL.Redeemers.new();
export const redeemers_totalExUnits = self => self.total_ex_units.bind(self)();

// Relay
export const relay_newSingleHostAddr = single_host_addr => CSL.Relay.new_single_host_addr(single_host_addr);
export const relay_newSingleHostName = single_host_name => CSL.Relay.new_single_host_name(single_host_name);
export const relay_newMultiHostName = multi_host_name => CSL.Relay.new_multi_host_name(multi_host_name);
export const relay_kind = self => self.kind.bind(self)();
export const relay_asSingleHostAddr = self => self.as_single_host_addr.bind(self)();
export const relay_asSingleHostName = self => self.as_single_host_name.bind(self)();
export const relay_asMultiHostName = self => self.as_multi_host_name.bind(self)();

// Relays
export const relays_new = () => CSL.Relays.new();

// RewardAddress
export const rewardAddress_new = network => payment => CSL.RewardAddress.new(network, payment);
export const rewardAddress_paymentCred = self => self.payment_cred.bind(self)();
export const rewardAddress_toAddress = self => self.to_address.bind(self)();
export const rewardAddress_fromAddress = addr => CSL.RewardAddress.from_address(addr);

// RewardAddresses
export const rewardAddresses_new = () => CSL.RewardAddresses.new();

// ScriptAll
export const scriptAll_nativeScripts = self => self.native_scripts.bind(self)();
export const scriptAll_new = native_scripts => CSL.ScriptAll.new(native_scripts);

// ScriptAny
export const scriptAny_nativeScripts = self => self.native_scripts.bind(self)();
export const scriptAny_new = native_scripts => CSL.ScriptAny.new(native_scripts);

// ScriptDataHash
export const scriptDataHash_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const scriptDataHash_fromBech32 = bech_str => errorableToPurs(CSL.ScriptDataHash.from_bech32, bech_str);

// ScriptHash
export const scriptHash_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const scriptHash_fromBech32 = bech_str => errorableToPurs(CSL.ScriptHash.from_bech32, bech_str);

// ScriptHashes
export const scriptHashes_new = () => CSL.ScriptHashes.new();

// ScriptNOfK
export const scriptNOfK_n = self => self.n.bind(self)();
export const scriptNOfK_nativeScripts = self => self.native_scripts.bind(self)();
export const scriptNOfK_new = n => native_scripts => CSL.ScriptNOfK.new(n, native_scripts);

// ScriptPubkey
export const scriptPubkey_addrKeyhash = self => self.addr_keyhash.bind(self)();
export const scriptPubkey_new = addr_keyhash => CSL.ScriptPubkey.new(addr_keyhash);

// ScriptRef
export const scriptRef_newNativeScript = native_script => CSL.ScriptRef.new_native_script(native_script);
export const scriptRef_newPlutusScript = plutus_script => CSL.ScriptRef.new_plutus_script(plutus_script);
export const scriptRef_isNativeScript = self => self.is_native_script.bind(self)();
export const scriptRef_isPlutusScript = self => self.is_plutus_script.bind(self)();
export const scriptRef_nativeScript = self => self.native_script.bind(self)();
export const scriptRef_plutusScript = self => self.plutus_script.bind(self)();

// SingleHostAddr
export const singleHostAddr_port = self => self.port.bind(self)();
export const singleHostAddr_ipv4 = self => self.ipv4.bind(self)();
export const singleHostAddr_ipv6 = self => self.ipv6.bind(self)();
export const singleHostAddr_new = port => ipv4 => ipv6 => CSL.SingleHostAddr.new(port, ipv4, ipv6);

// SingleHostName
export const singleHostName_port = self => self.port.bind(self)();
export const singleHostName_dnsName = self => self.dns_name.bind(self)();
export const singleHostName_new = port => dns_name => CSL.SingleHostName.new(port, dns_name);

// StakeCredential
export const stakeCredential_fromKeyhash = hash => CSL.StakeCredential.from_keyhash(hash);
export const stakeCredential_fromScripthash = hash => CSL.StakeCredential.from_scripthash(hash);
export const stakeCredential_toKeyhash = self => self.to_keyhash.bind(self)();
export const stakeCredential_toScripthash = self => self.to_scripthash.bind(self)();
export const stakeCredential_kind = self => self.kind.bind(self)();

// StakeCredentials
export const stakeCredentials_new = () => CSL.StakeCredentials.new();

// StakeDelegation
export const stakeDelegation_stakeCredential = self => self.stake_credential.bind(self)();
export const stakeDelegation_poolKeyhash = self => self.pool_keyhash.bind(self)();
export const stakeDelegation_new = stake_credential => pool_keyhash => CSL.StakeDelegation.new(stake_credential, pool_keyhash);

// StakeDeregistration
export const stakeDeregistration_stakeCredential = self => self.stake_credential.bind(self)();
export const stakeDeregistration_new = stake_credential => CSL.StakeDeregistration.new(stake_credential);

// StakeRegistration
export const stakeRegistration_stakeCredential = self => self.stake_credential.bind(self)();
export const stakeRegistration_new = stake_credential => CSL.StakeRegistration.new(stake_credential);

// TimelockExpiry
export const timelockExpiry_slot = self => self.slot.bind(self)();
export const timelockExpiry_slotBignum = self => self.slot_bignum.bind(self)();
export const timelockExpiry_new = slot => CSL.TimelockExpiry.new(slot);
export const timelockExpiry_newTimelockexpiry = slot => CSL.TimelockExpiry.new_timelockexpiry(slot);

// TimelockStart
export const timelockStart_slot = self => self.slot.bind(self)();
export const timelockStart_slotBignum = self => self.slot_bignum.bind(self)();
export const timelockStart_new = slot => CSL.TimelockStart.new(slot);
export const timelockStart_newTimelockstart = slot => CSL.TimelockStart.new_timelockstart(slot);

// Transaction
export const transaction_body = self => self.body.bind(self)();
export const transaction_witnessSet = self => self.witness_set.bind(self)();
export const transaction_isValid = self => self.is_valid.bind(self)();
export const transaction_auxiliaryData = self => self.auxiliary_data.bind(self)();
export const transaction_setIsValid = self => valid => () => self.set_is_valid.bind(self)(valid);
export const transaction_new = body => witness_set => auxiliary_data => CSL.Transaction.new(body, witness_set, auxiliary_data);

// TransactionBatch

// TransactionBatchList

// TransactionBody
export const transactionBody_inputs = self => self.inputs.bind(self)();
export const transactionBody_outputs = self => self.outputs.bind(self)();
export const transactionBody_fee = self => self.fee.bind(self)();
export const transactionBody_ttl = self => self.ttl.bind(self)();
export const transactionBody_ttlBignum = self => self.ttl_bignum.bind(self)();
export const transactionBody_setTtl = self => ttl => () => self.set_ttl.bind(self)(ttl);
export const transactionBody_removeTtl = self => errorableToPurs(self.remove_ttl.bind(self), );
export const transactionBody_setCerts = self => certs => () => self.set_certs.bind(self)(certs);
export const transactionBody_certs = self => self.certs.bind(self)();
export const transactionBody_setWithdrawals = self => withdrawals => () => self.set_withdrawals.bind(self)(withdrawals);
export const transactionBody_withdrawals = self => self.withdrawals.bind(self)();
export const transactionBody_setUpdate = self => update => () => self.set_update.bind(self)(update);
export const transactionBody_update = self => self.update.bind(self)();
export const transactionBody_setAuxiliaryDataHash = self => auxiliary_data_hash => () => self.set_auxiliary_data_hash.bind(self)(auxiliary_data_hash);
export const transactionBody_auxiliaryDataHash = self => self.auxiliary_data_hash.bind(self)();
export const transactionBody_setValidityStartInterval = self => validity_start_interval => () => self.set_validity_start_interval.bind(self)(validity_start_interval);
export const transactionBody_setValidityStartIntervalBignum = self => validity_start_interval => () => self.set_validity_start_interval_bignum.bind(self)(validity_start_interval);
export const transactionBody_validityStartIntervalBignum = self => self.validity_start_interval_bignum.bind(self)();
export const transactionBody_validityStartInterval = self => self.validity_start_interval.bind(self)();
export const transactionBody_setMint = self => mint => () => self.set_mint.bind(self)(mint);
export const transactionBody_mint = self => self.mint.bind(self)();
export const transactionBody_multiassets = self => self.multiassets.bind(self)();
export const transactionBody_setReferenceInputs = self => reference_inputs => () => self.set_reference_inputs.bind(self)(reference_inputs);
export const transactionBody_referenceInputs = self => self.reference_inputs.bind(self)();
export const transactionBody_setScriptDataHash = self => script_data_hash => () => self.set_script_data_hash.bind(self)(script_data_hash);
export const transactionBody_scriptDataHash = self => self.script_data_hash.bind(self)();
export const transactionBody_setCollateral = self => collateral => () => self.set_collateral.bind(self)(collateral);
export const transactionBody_collateral = self => self.collateral.bind(self)();
export const transactionBody_setRequiredSigners = self => required_signers => () => self.set_required_signers.bind(self)(required_signers);
export const transactionBody_requiredSigners = self => self.required_signers.bind(self)();
export const transactionBody_setNetworkId = self => network_id => () => self.set_network_id.bind(self)(network_id);
export const transactionBody_networkId = self => self.network_id.bind(self)();
export const transactionBody_setCollateralReturn = self => collateral_return => () => self.set_collateral_return.bind(self)(collateral_return);
export const transactionBody_collateralReturn = self => self.collateral_return.bind(self)();
export const transactionBody_setTotalCollateral = self => total_collateral => () => self.set_total_collateral.bind(self)(total_collateral);
export const transactionBody_totalCollateral = self => self.total_collateral.bind(self)();
export const transactionBody_new = inputs => outputs => fee => ttl => CSL.TransactionBody.new(inputs, outputs, fee, ttl);
export const transactionBody_newTxBody = inputs => outputs => fee => CSL.TransactionBody.new_tx_body(inputs, outputs, fee);

// TransactionHash
export const transactionHash_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const transactionHash_fromBech32 = bech_str => errorableToPurs(CSL.TransactionHash.from_bech32, bech_str);

// TransactionInput
export const transactionInput_transactionId = self => self.transaction_id.bind(self)();
export const transactionInput_index = self => self.index.bind(self)();
export const transactionInput_new = transaction_id => index => CSL.TransactionInput.new(transaction_id, index);

// TransactionInputs
export const transactionInputs_new = () => CSL.TransactionInputs.new();
export const transactionInputs_toOption = self => self.to_option.bind(self)();

// TransactionMetadatum
export const transactionMetadatum_newMap = map => CSL.TransactionMetadatum.new_map(map);
export const transactionMetadatum_newList = list => CSL.TransactionMetadatum.new_list(list);
export const transactionMetadatum_newInt = int => CSL.TransactionMetadatum.new_int(int);
export const transactionMetadatum_newBytes = bytes => CSL.TransactionMetadatum.new_bytes(bytes);
export const transactionMetadatum_newText = text => CSL.TransactionMetadatum.new_text(text);
export const transactionMetadatum_kind = self => self.kind.bind(self)();
export const transactionMetadatum_asMap = self => errorableToPurs(self.as_map.bind(self), );
export const transactionMetadatum_asList = self => errorableToPurs(self.as_list.bind(self), );
export const transactionMetadatum_asInt = self => errorableToPurs(self.as_int.bind(self), );
export const transactionMetadatum_asBytes = self => errorableToPurs(self.as_bytes.bind(self), );
export const transactionMetadatum_asText = self => errorableToPurs(self.as_text.bind(self), );

// TransactionMetadatumLabels
export const transactionMetadatumLabels_new = () => CSL.TransactionMetadatumLabels.new();

// TransactionOutput
export const transactionOutput_address = self => self.address.bind(self)();
export const transactionOutput_amount = self => self.amount.bind(self)();
export const transactionOutput_dataHash = self => self.data_hash.bind(self)();
export const transactionOutput_plutusData = self => self.plutus_data.bind(self)();
export const transactionOutput_scriptRef = self => self.script_ref.bind(self)();
export const transactionOutput_setScriptRef = self => script_ref => () => self.set_script_ref.bind(self)(script_ref);
export const transactionOutput_setPlutusData = self => data => () => self.set_plutus_data.bind(self)(data);
export const transactionOutput_setDataHash = self => data_hash => () => self.set_data_hash.bind(self)(data_hash);
export const transactionOutput_hasPlutusData = self => self.has_plutus_data.bind(self)();
export const transactionOutput_hasDataHash = self => self.has_data_hash.bind(self)();
export const transactionOutput_hasScriptRef = self => self.has_script_ref.bind(self)();
export const transactionOutput_new = address => amount => CSL.TransactionOutput.new(address, amount);
export const transactionOutput_serializationFormat = self => self.serialization_format.bind(self)();

// TransactionOutputs
export const transactionOutputs_new = () => CSL.TransactionOutputs.new();

// TransactionUnspentOutput
export const transactionUnspentOutput_new = input => output => CSL.TransactionUnspentOutput.new(input, output);
export const transactionUnspentOutput_input = self => self.input.bind(self)();
export const transactionUnspentOutput_output = self => self.output.bind(self)();

// TransactionUnspentOutputs
export const transactionUnspentOutputs_new = () => CSL.TransactionUnspentOutputs.new();

// TransactionWitnessSet
export const transactionWitnessSet_setVkeys = self => vkeys => () => self.set_vkeys.bind(self)(vkeys);
export const transactionWitnessSet_vkeys = self => () => self.vkeys.bind(self)();
export const transactionWitnessSet_setNativeScripts = self => native_scripts => () => self.set_native_scripts.bind(self)(native_scripts);
export const transactionWitnessSet_nativeScripts = self => () => self.native_scripts.bind(self)();
export const transactionWitnessSet_setBootstraps = self => bootstraps => () => self.set_bootstraps.bind(self)(bootstraps);
export const transactionWitnessSet_bootstraps = self => () => self.bootstraps.bind(self)();
export const transactionWitnessSet_setPlutusScripts = self => plutus_scripts => () => self.set_plutus_scripts.bind(self)(plutus_scripts);
export const transactionWitnessSet_plutusScripts = self => () => self.plutus_scripts.bind(self)();
export const transactionWitnessSet_setPlutusData = self => plutus_data => () => self.set_plutus_data.bind(self)(plutus_data);
export const transactionWitnessSet_plutusData = self => () => self.plutus_data.bind(self)();
export const transactionWitnessSet_setRedeemers = self => redeemers => () => self.set_redeemers.bind(self)(redeemers);
export const transactionWitnessSet_redeemers = self => () => self.redeemers.bind(self)();
export const transactionWitnessSet_new = () => CSL.TransactionWitnessSet.new();

// URL
export const url_new = url => CSL.URL.new(url);
export const url_url = self => self.url.bind(self)();

// UnitInterval
export const unitInterval_numerator = self => self.numerator.bind(self)();
export const unitInterval_denominator = self => self.denominator.bind(self)();
export const unitInterval_new = numerator => denominator => CSL.UnitInterval.new(numerator, denominator);

// Update
export const update_proposedProtocolParameterUpdates = self => self.proposed_protocol_parameter_updates.bind(self)();
export const update_epoch = self => self.epoch.bind(self)();
export const update_new = proposed_protocol_parameter_updates => epoch => CSL.Update.new(proposed_protocol_parameter_updates, epoch);

// VRFCert
export const vrfCert_output = self => self.output.bind(self)();
export const vrfCert_proof = self => self.proof.bind(self)();
export const vrfCert_new = output => proof => CSL.VRFCert.new(output, proof);

// VRFKeyHash
export const vrfKeyHash_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const vrfKeyHash_fromBech32 = bech_str => errorableToPurs(CSL.VRFKeyHash.from_bech32, bech_str);

// VRFVKey
export const vrfvKey_toBech32 = self => prefix => self.to_bech32.bind(self)(prefix);
export const vrfvKey_fromBech32 = bech_str => errorableToPurs(CSL.VRFVKey.from_bech32, bech_str);

// Value
export const value_new = coin => CSL.Value.new(coin);
export const value_newFromAssets = multiasset => CSL.Value.new_from_assets(multiasset);
export const value_newWithAssets = coin => multiasset => CSL.Value.new_with_assets(coin, multiasset);
export const value_zero = CSL.Value.zero();
export const value_isZero = self => self.is_zero.bind(self)();
export const value_coin = self => self.coin.bind(self)();
export const value_setCoin = self => coin => () => self.set_coin.bind(self)(coin);
export const value_multiasset = self => self.multiasset.bind(self)();
export const value_setMultiasset = self => multiasset => () => self.set_multiasset.bind(self)(multiasset);
export const value_checkedAdd = self => rhs => errorableToPurs(self.checked_add.bind(self), rhs);
export const value_checkedSub = self => rhs_value => errorableToPurs(self.checked_sub.bind(self), rhs_value);
export const value_clampedSub = self => rhs_value => self.clamped_sub.bind(self)(rhs_value);
export const value_compare = self => rhs_value => self.compare.bind(self)(rhs_value);

// Vkey
export const vkey_new = pk => CSL.Vkey.new(pk);
export const vkey_publicKey = self => self.public_key.bind(self)();

// Vkeys
export const vkeys_new = () => CSL.Vkeys.new();

// Vkeywitness
export const vkeywitness_new = vkey => signature => CSL.Vkeywitness.new(vkey, signature);
export const vkeywitness_vkey = self => self.vkey.bind(self)();
export const vkeywitness_signature = self => self.signature.bind(self)();

// Vkeywitnesses
export const vkeywitnesses_new = () => CSL.Vkeywitnesses.new();

// Withdrawals
export const withdrawals_new = () => CSL.Withdrawals.new();


export const hashTransaction = tx_body => CSL.hash_transaction(tx_body);
export const hashPlutusData = plutus_data => CSL.hash_plutus_data(plutus_data);
export const minAdaForOutput = output => data_cost => CSL.min_ada_for_output(output, data_cost);

