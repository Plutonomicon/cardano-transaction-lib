import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";

const setter = prop => obj => value => () => obj["set_" + prop](value);

export function hashTransaction(body) {
  return () => lib.hash_transaction(body);
}

export function newValue(coin) {
  return () => lib.Value.new(coin);
}

export function newValueFromAssets(multiasset) {
  return () => lib.Value.new_from_assets(multiasset);
}

export const valueSetCoin = setter("coin");

export function newTransactionInput(transaction_id) {
  return index => () => lib.TransactionInput.new(transaction_id, index);
}

export function newTransactionInputs() {
  return lib.TransactionInputs.new();
}

export function addTransactionInput(inputs) {
  return input => () => inputs.add(input);
}

export function newTransactionOutput(address) {
  return amount => () => lib.TransactionOutput.new(address, amount);
}

export function newTransactionOutputs() {
  return lib.TransactionOutputs.new();
}

export function addTransactionOutput(outputs) {
  return output => () => outputs.add(output);
}

export function newTransactionBody(inputs) {
  return outputs => fee => () =>
    lib.TransactionBody.new_tx_body(inputs, outputs, fee);
}

export function setTxIsValid(tx) {
  return isValid => () => tx.set_is_valid(isValid);
}

export function newTransaction(body) {
  return witness_set => auxiliary_data => () =>
    lib.Transaction.new(body, witness_set, auxiliary_data);
}

export function newTransaction_(body) {
  return witness_set => () => lib.Transaction.new(body, witness_set);
}

export function newTransactionUnspentOutput(input) {
  return output => () => lib.TransactionUnspentOutput.new(input, output);
}

export function newMultiAsset() {
  return lib.MultiAsset.new();
}

export function insertMultiAsset(multiasset) {
  return key => value => () => multiasset.insert(key, value);
}

export function newAssets() {
  return lib.Assets.new();
}

export function insertAssets(assets) {
  return key => value => () => assets.insert(key, value);
}

export function newAssetName(name) {
  return () => lib.AssetName.new(name);
}

export const transactionOutputSetDataHash = setter("data_hash");
export const transactionOutputSetPlutusData = setter("plutus_data");
export const transactionOutputSetScriptRef = setter("script_ref");

export function scriptRefNewNativeScript(nativeScript) {
  return lib.ScriptRef.new_native_script(nativeScript);
}

export function scriptRefNewPlutusScript(plutusScript) {
  return lib.ScriptRef.new_plutus_script(plutusScript);
}

export function newVkeywitnesses() {
  return lib.Vkeywitnesses.new();
}

export function makeVkeywitness(hash) {
  return key => () => lib.make_vkey_witness(hash, key);
}

export function newVkeywitness(vkey) {
  return signature => () => lib.Vkeywitness.new(vkey, signature);
}

export function addVkeywitness(witnesses) {
  return witness => () => witnesses.add(witness);
}

export function newVkeyFromPublicKey(public_key) {
  return () => lib.Vkey.new(public_key);
}

export function publicKeyHash(pk) {
  return pk.hash();
}

export const transactionWitnessSetSetVkeys = setter("vkeys");

export function newCostmdls() {
  return lib.Costmdls.new();
}

export function defaultCostmdls() {
  return lib.TxBuilderConstants.plutus_vasil_cost_models();
}

export function costmdlsSetCostModel(cms) {
  return lang => cm => () => cms.insert(lang, cm);
}

export function newCostModel() {
  return lib.CostModel.new();
}

export function costModelSetCost(cm) {
  return op => cost => () => cm.set(op, cost);
}

export function newPlutusV1() {
  return lib.Language.new_plutus_v1();
}

export function newPlutusV2() {
  return lib.Language.new_plutus_v2();
}

export function _hashScriptData(rs) {
  return cms => ds => () => {
    const list = lib.PlutusList.new();
    ds.forEach(d => list.add(d));
    return lib.hash_script_data(rs, cms, list);
  };
}

export function _hashScriptDataNoDatums(rs) {
  return cms => () => lib.hash_script_data(rs, cms);
}

export function newRedeemers() {
  return lib.Redeemers.new();
}

export function addRedeemer(rs) {
  return r => () => rs.add(r);
}

export function setTxBodyReferenceInputs(txBody) {
  return referenceInputs => () => txBody.set_reference_inputs(referenceInputs);
}

export const setTxBodyScriptDataHash = setter("script_data_hash");
export const setTxBodyMint = setter("mint");

export function newMint() {
  return lib.Mint.new();
}

export function _bigIntToInt(maybe) {
  return bigInt => {
    try {
      const str = bigInt.to_str();
      if (str[0] == "-") {
        return maybe.just(
          lib.Int.new_negative(lib.BigNum.from_str(str.slice(1)))
        );
      } else {
        return maybe.just(lib.Int.new(lib.BigNum.from_str(str)));
      }
    } catch (_) {
      return maybe.nothing;
    }
  };
}

export const newMintAssets = lib.MintAssets.new;

export function insertMintAssets(mint) {
  return scriptHash => mintAssets => () => mint.insert(scriptHash, mintAssets);
}

export function insertMintAsset(mintAssets) {
  return assetName => int => () => mintAssets.insert(assetName, int);
}

export function networkIdTestnet() {
  return lib.NetworkId.testnet();
}

export function networkIdMainnet() {
  return lib.NetworkId.mainnet();
}

export function setTxBodyCollateralReturn(txBody) {
  return collateralReturn => () =>
    txBody.set_collateral_return(collateralReturn);
}

export function setTxBodyTotalCollateral(txBody) {
  return totalCollateral => () => txBody.set_total_collateral(totalCollateral);
}

export const setTxBodyTtl = setter("ttl");
export const setTxBodyCerts = setter("certs");

export function newCertificates() {
  return lib.Certificates.new();
}

export function newStakeRegistrationCertificate(stakeCredential) {
  return () =>
    lib.Certificate.new_stake_registration(
      lib.StakeRegistration.new(stakeCredential)
    );
}

export function newStakeDeregistrationCertificate(stakeCredential) {
  return () =>
    lib.Certificate.new_stake_deregistration(
      lib.StakeDeregistration.new(stakeCredential)
    );
}

export function newStakeDelegationCertificate(stakeCredential) {
  return ed25519KeyHash => () =>
    lib.Certificate.new_stake_delegation(
      lib.StakeDelegation.new(stakeCredential, ed25519KeyHash)
    );
}

export function newPoolRegistrationCertificate(operator) {
  return vrfKeyhash =>
    pledge =>
    cost =>
    margin =>
    reward_account =>
    poolOwners =>
    relays =>
    poolMetadata =>
    () =>
      lib.Certificate.new_pool_registration(
        lib.PoolRegistration.new(
          lib.PoolParams.new(
            operator,
            vrfKeyhash,
            pledge,
            cost,
            margin,
            reward_account,
            poolOwners,
            relays,
            poolMetadata
          )
        )
      );
}

export function newUnitInterval(numerator) {
  return denominator => () => lib.UnitInterval.new(numerator, denominator);
}

export function newPoolRetirementCertificate(poolKeyHash) {
  return epoch => () =>
    lib.Certificate.new_pool_retirement(
      lib.PoolRetirement.new(poolKeyHash, epoch)
    );
}

export function newGenesisKeyDelegationCertificate(genesisHash) {
  return genesisDelegateHash => vrfKeyhash => () =>
    lib.Certificate.new_genesis_key_delegation(
      lib.GenesisKeyDelegation.new(genesisHash, genesisDelegateHash, vrfKeyhash)
    );
}

export function addCert(certificates) {
  return certificate => () => certificates.add(certificate);
}

export const setTxBodyCollateral = setter("collateral");
export const setTxBodyNetworkId = setter("network_id");

export function transactionBodySetRequiredSigners(containerHelper) {
  return body => keyHashes => () =>
    body.set_required_signers(
      containerHelper.pack(lib.Ed25519KeyHashes, keyHashes)
    );
}

export const transactionBodySetValidityStartInterval = setter(
  "validity_start_interval_bignum"
);

export function transactionBodySetAuxiliaryDataHash(txBody) {
  return hash => () => txBody.set_auxiliary_data_hash(hash);
}

export function convertPoolOwners(containerHelper) {
  return keyHashes => () =>
    containerHelper.pack(lib.Ed25519KeyHashes, keyHashes);
}

export function packRelays(containerHelper) {
  return relays => containerHelper.pack(lib.Relays, relays);
}

export function newIpv4(data) {
  return () => lib.Ipv4.new(data);
}

export function newIpv6(data) {
  return () => lib.Ipv6.new(data);
}

export function newSingleHostAddr(port) {
  return ipv4 => ipv6 => () =>
    lib.Relay.new_single_host_addr(lib.SingleHostAddr.new(port, ipv4, ipv6));
}

export function newSingleHostName(port) {
  return dnsName => () =>
    lib.Relay.new_single_host_name(
      lib.SingleHostName.new(port, lib.DNSRecordAorAAAA.new(dnsName))
    );
}

export function newMultiHostName(dnsName) {
  return () =>
    lib.Relay.new_multi_host_name(
      lib.MultiHostName.new(lib.DNSRecordSRV.new(dnsName))
    );
}

export function newPoolMetadata(url) {
  return hash => () => lib.PoolMetadata.new(lib.URL.new(url), hash);
}

export function newMoveInstantaneousRewardToOtherPot(pot) {
  return amount => () =>
    lib.MoveInstantaneousReward.new_to_other_pot(pot, amount);
}

export function newMoveInstantaneousRewardToStakeCreds(pot) {
  return amounts => () =>
    lib.MoveInstantaneousReward.new_to_stake_creds(pot, amounts);
}

export function newMIRToStakeCredentials(containerHelper) {
  return entries => () =>
    containerHelper.packMap(lib.MIRToStakeCredentials, entries);
}

export function newMoveInstantaneousRewardsCertificate(mir) {
  return () =>
    lib.Certificate.new_move_instantaneous_rewards_cert(
      lib.MoveInstantaneousRewardsCert.new(mir)
    );
}

export function newWithdrawals(containerHelper) {
  return entries => () => containerHelper.packMap(lib.Withdrawals, entries);
}

export const setTxBodyWithdrawals = setter("withdrawals");
export const setTxBodyUpdate = setter("update");

export function newUpdate(ppUpdates) {
  return epoch => () => lib.Update.new(ppUpdates, epoch);
}

export const ppuSetMinfeeA = setter("minfee_a");
export const ppuSetMinfeeB = setter("minfee_b");
export const ppuSetMaxBlockBodySize = setter("max_block_body_size");
export const ppuSetMaxTxSize = setter("max_tx_size");
export const ppuSetMaxBlockHeaderSize = setter("max_block_header_size");
export const ppuSetKeyDeposit = setter("key_deposit");
export const ppuSetPoolDeposit = setter("pool_deposit");
export const ppuSetMaxEpoch = setter("max_epoch");
export const ppuSetNOpt = setter("n_opt");
export const ppuSetPoolPledgeInfluence = setter("pool_pledge_influence");
export const ppuSetExpansionRate = setter("expansion_rate");
export const ppuSetTreasuryGrowthRate = setter("treasury_growth_rate");

export function newProtocolVersion(major) {
  return minor => () => lib.ProtocolVersion.new(major, minor);
}

export function ppuSetProtocolVersion(ppu) {
  return version => () => ppu.set_protocol_version(version);
}

export const ppuSetMinPoolCost = setter("min_pool_cost");
export const ppuSetAdaPerUtxoByte = setter("ada_per_utxo_byte");
export const ppuSetCostModels = setter("cost_models");

export function newExUnitPrices(mem_price) {
  return step_price => () => lib.ExUnitPrices.new(mem_price, step_price);
}

export const ppuSetExecutionCosts = setter("execution_costs");
export const ppuSetMaxTxExUnits = setter("max_tx_ex_units");
export const ppuSetMaxBlockExUnits = setter("max_block_ex_units");
export const ppuSetMaxValueSize = setter("max_value_size");
export const ppuSetCollateralPercentage = setter("collateral_percentage");
export const ppuSetMaxCollateralInputs = setter("max_collateral_inputs");

export function newProtocolParamUpdate() {
  return lib.ProtocolParamUpdate.new();
}

export function newProposedProtocolParameterUpdates(containerHelper) {
  return kvs => () =>
    containerHelper.packMap(lib.ProposedProtocolParameterUpdates, kvs);
}
