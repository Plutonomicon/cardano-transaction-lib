/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

const setter = prop => obj => value => () => obj["set_" + prop](value);

exports.hashTransaction = body => () => lib.hash_transaction(body);

exports.newBigNum = maybe => string => {
  try {
    return maybe.just(lib.BigNum.from_str(string));
  } catch (_) {
    return maybe.nothing;
  }
};

exports.newValue = coin => () => lib.Value.new(coin);

exports.newValueFromAssets = multiasset => () =>
  lib.Value.new_from_assets(multiasset);

exports.valueSetCoin = setter("coin");

exports.newTransactionInput = transaction_id => index => () =>
  lib.TransactionInput.new(transaction_id, index);

exports.newTransactionInputs = () => lib.TransactionInputs.new();

exports.addTransactionInput = inputs => input => () => inputs.add(input);

exports.newTransactionOutput = address => amount => () =>
  lib.TransactionOutput.new(address, amount);

exports.newTransactionOutputs = () => lib.TransactionOutputs.new();

exports.addTransactionOutput = outputs => output => () => outputs.add(output);

exports.newTransactionBody = inputs => outputs => fee => () =>
  lib.TransactionBody.new_tx_body(inputs, outputs, fee);

exports.setTxIsValid = tx => isValid => () => tx.set_is_valid(isValid);

exports.newTransaction = body => witness_set => auxiliary_data => () =>
  lib.Transaction.new(body, witness_set, auxiliary_data);

exports.newTransaction_ = body => witness_set => () =>
  lib.Transaction.new(body, witness_set);

exports.newTransactionUnspentOutputFromBytes = bytes => () =>
  lib.TransactionUnspentOutput.from_bytes(bytes);

exports.newTransactionUnspentOutput = input => output => () =>
  lib.TransactionUnspentOutput.new(input, output);

exports.newTransactionWitnessSetFromBytes = bytes => () =>
  lib.TransactionWitnessSet.from_bytes(bytes);

exports.newMultiAsset = () => lib.MultiAsset.new();

exports.insertMultiAsset = multiasset => key => value => () =>
  multiasset.insert(key, value);

exports.newAssets = () => lib.Assets.new();

exports.insertAssets = assets => key => value => () =>
  assets.insert(key, value);

exports.newAssetName = name => () => lib.AssetName.new(name);

exports.transactionOutputSetDataHash = setter("data_hash");

exports.transactionOutputSetPlutusData = setter("plutus_data");

exports.transactionOutputSetScriptRef = setter("script_ref");

exports.scriptRefNewNativeScript = nativeScript =>
  lib.ScriptRef.new_native_script(nativeScript);

exports.scriptRefNewPlutusScript = plutusScript =>
  lib.ScriptRef.new_plutus_script(plutusScript);

exports.newVkeywitnesses = () => lib.Vkeywitnesses.new();

exports.makeVkeywitness = hash => key => () => lib.make_vkey_witness(hash, key);

exports.newVkeywitness = vkey => signature => () =>
  lib.Vkeywitness.new(vkey, signature);

exports.addVkeywitness = witnesses => witness => () => witnesses.add(witness);

exports.newVkeyFromPublicKey = public_key => () => lib.Vkey.new(public_key);

exports._publicKeyFromBech32 = maybe => bech32 => {
  try {
    return maybe.just(lib.PublicKey.from_bech32(bech32));
  } catch (_) {
    return maybe.nothing;
  }
};

exports.publicKeyFromPrivateKey = private_key => () => {
  return private_key.to_public();
};

exports._privateKeyFromBytes = maybe => bytes => {
  try {
    return maybe.just(lib.PrivateKey.from_normal_bytes(bytes));
  } catch (_) {
    return maybe.nothing;
  }
};

exports._bytesFromPrivateKey = maybe => key => {
  try {
    return maybe.just(key.as_bytes());
  } catch (err) {
    return maybe.nothing;
  }
};

exports.publicKeyHash = pk => pk.hash();

exports.newEd25519Signature = bech32 => () =>
  lib.Ed25519Signature.from_bech32(bech32);

exports.transactionWitnessSetSetVkeys = setter("vkeys");

exports.toBytes = sth => sth.to_bytes();

exports.newCostmdls = () => lib.Costmdls.new();

exports.defaultCostmdls = () =>
  lib.TxBuilderConstants.plutus_vasil_cost_models();

exports.costmdlsSetCostModel = cms => lang => cm => () => cms.insert(lang, cm);

exports.newCostModel = () => lib.CostModel.new();

exports.costModelSetCost = cm => op => cost => () => cm.set(op, cost);

exports.newPlutusV1 = () => lib.Language.new_plutus_v1();

exports.newPlutusV2 = () => lib.Language.new_plutus_v2();

exports._hashScriptData = rs => cms => ds => () => {
  const list = lib.PlutusList.new();
  ds.forEach(d => list.add(d));
  return lib.hash_script_data(rs, cms, list);
};

exports._hashScriptDataNoDatums = rs => cms => () =>
  lib.hash_script_data(rs, cms);

exports.newRedeemers = () => lib.Redeemers.new();

exports.addRedeemer = rs => r => () => rs.add(r);

exports.setTxBodyReferenceInputs = txBody => referenceInputs => () =>
  txBody.set_reference_inputs(referenceInputs);

exports.newScriptDataHashFromBytes = bytes => () =>
  lib.ScriptDataHash.from_bytes(bytes);

exports.setTxBodyScriptDataHash = setter("script_data_hash");

exports.setTxBodyMint = setter("mint");

exports.newMint = () => lib.Mint.new();

exports._bigIntToInt = maybe => bigInt => {
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

exports.newMintAssets = lib.MintAssets.new;

exports.insertMintAssets = mint => scriptHash => mintAssets => () =>
  mint.insert(scriptHash, mintAssets);

exports.insertMintAsset = mintAssets => assetName => int => () =>
  mintAssets.insert(assetName, int);

exports.networkIdTestnet = () => lib.NetworkId.testnet();

exports.networkIdMainnet = () => lib.NetworkId.mainnet();

exports.setTxBodyCollateralReturn = txBody => collateralReturn => () =>
  txBody.set_collateral_return(collateralReturn);

exports.setTxBodyTotalCollateral = txBody => totalCollateral => () =>
  txBody.set_total_collateral(totalCollateral);

exports.setTxBodyTtl = setter("ttl");

exports.setTxBodyCerts = setter("certs");

exports.newCertificates = () => lib.Certificates.new();

exports.newStakeRegistrationCertificate = stakeCredential => () =>
  lib.Certificate.new_stake_registration(
    lib.StakeRegistration.new(stakeCredential)
  );

exports.newStakeDeregistrationCertificate = stakeCredential => () =>
  lib.Certificate.new_stake_deregistration(
    lib.StakeDeregistration.new(stakeCredential)
  );

exports.newStakeDelegationCertificate =
  stakeCredential => ed25519KeyHash => () =>
    lib.Certificate.new_stake_delegation(
      lib.StakeDelegation.new(stakeCredential, ed25519KeyHash)
    );

exports.newPoolRegistrationCertificate =
  operator =>
  vrfKeyhash =>
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

exports.newUnitInterval = numerator => denominator => () =>
  lib.UnitInterval.new(numerator, denominator);

exports.newPoolRetirementCertificate = poolKeyHash => epoch => () =>
  lib.Certificate.new_pool_retirement(
    lib.PoolRetirement.new(poolKeyHash, epoch)
  );

exports.newGenesisKeyDelegationCertificate =
  genesisHash => genesisDelegateHash => vrfKeyhash => () =>
    lib.Certificate.new_genesis_key_delegation(
      lib.GenesisKeyDelegation.new(genesisHash, genesisDelegateHash, vrfKeyhash)
    );

exports.addCert = certificates => certificate => () =>
  certificates.add(certificate);

exports.setTxBodyCollateral = setter("collateral");

exports.setTxBodyNetworkId = setter("network_id");

exports.transactionBodySetRequiredSigners =
  containerHelper => body => keyHashes => () =>
    body.set_required_signers(
      containerHelper.pack(lib.Ed25519KeyHashes, keyHashes)
    );

exports.transactionBodySetValidityStartInterval = setter(
  "validity_start_interval_bignum"
);

exports.transactionBodySetAuxiliaryDataHash = txBody => hashBytes => () =>
  txBody.set_auxiliary_data_hash(lib.AuxiliaryDataHash.from_bytes(hashBytes));

exports.convertPoolOwners = containerHelper => keyHashes => () =>
  containerHelper.pack(lib.Ed25519KeyHashes, keyHashes);

exports.packRelays = containerHelper => relays =>
  containerHelper.pack(lib.Relays, relays);

exports.newIpv4 = data => () => lib.Ipv4.new(data);

exports.newIpv6 = data => () => lib.Ipv6.new(data);

exports.newSingleHostAddr = port => ipv4 => ipv6 => () =>
  lib.Relay.new_single_host_addr(lib.SingleHostAddr.new(port, ipv4, ipv6));

exports.newSingleHostName = port => dnsName => () =>
  lib.Relay.new_single_host_name(
    lib.SingleHostName.new(port, lib.DNSRecordAorAAAA.new(dnsName))
  );

exports.newMultiHostName = dnsName => () =>
  lib.Relay.new_multi_host_name(
    lib.MultiHostName.new(lib.DNSRecordSRV.new(dnsName))
  );

exports.newPoolMetadata = url => hash => () =>
  lib.PoolMetadata.new(lib.URL.new(url), lib.PoolMetadataHash.from_bytes(hash));

exports.newGenesisHash = bytes => () => lib.GenesisHash.from_bytes(bytes);

exports.newGenesisDelegateHash = bytes => () =>
  lib.GenesisDelegateHash.from_bytes(bytes);

exports.newMoveInstantaneousRewardToOtherPot = pot => amount => () =>
  lib.MoveInstantaneousReward.new_to_other_pot(pot, amount);

exports.newMoveInstantaneousRewardToStakeCreds = pot => amounts => () =>
  lib.MoveInstantaneousReward.new_to_stake_creds(pot, amounts);

exports.newMIRToStakeCredentials = containerHelper => entries => () =>
  containerHelper.packMap(lib.MIRToStakeCredentials, entries);

exports.newMoveInstantaneousRewardsCertificate = mir => () =>
  lib.Certificate.new_move_instantaneous_rewards_cert(
    lib.MoveInstantaneousRewardsCert.new(mir)
  );

exports.newWithdrawals = containerHelper => entries => () =>
  containerHelper.packMap(lib.Withdrawals, entries);

exports.setTxBodyWithdrawals = setter("withdrawals");

exports.setTxBodyUpdate = setter("update");

exports.newUpdate = ppUpdates => epoch => () =>
  lib.Update.new(ppUpdates, epoch);

exports.ppuSetMinfeeA = setter("minfee_a");

exports.ppuSetMinfeeB = setter("minfee_b");

exports.ppuSetMaxBlockBodySize = setter("max_block_body_size");

exports.ppuSetMaxTxSize = setter("max_tx_size");

exports.ppuSetMaxBlockHeaderSize = setter("max_block_header_size");

exports.ppuSetKeyDeposit = setter("key_deposit");

exports.ppuSetPoolDeposit = setter("pool_deposit");

exports.ppuSetMaxEpoch = setter("max_epoch");

exports.ppuSetNOpt = setter("n_opt");

exports.ppuSetPoolPledgeInfluence = setter("pool_pledge_influence");

exports.ppuSetExpansionRate = setter("expansion_rate");

exports.ppuSetTreasuryGrowthRate = setter("treasury_growth_rate");

exports.newProtocolVersion = major => minor => () =>
  lib.ProtocolVersion.new(major, minor);

exports.ppuSetProtocolVersion = ppu => version => () =>
  ppu.set_protocol_version(version);

exports.ppuSetMinPoolCost = setter("min_pool_cost");

exports.ppuSetAdaPerUtxoByte = setter("ada_per_utxo_byte");

exports.ppuSetCostModels = setter("cost_models");

exports.newExUnitPrices = mem_price => step_price => () =>
  lib.ExUnitPrices.new(mem_price, step_price);

exports.ppuSetExecutionCosts = setter("execution_costs");

exports.ppuSetMaxTxExUnits = setter("max_tx_ex_units");

exports.ppuSetMaxBlockExUnits = setter("max_block_ex_units");

exports.ppuSetMaxValueSize = setter("max_value_size");

exports.newProtocolParamUpdate = () => lib.ProtocolParamUpdate.new();

exports.newProposedProtocolParameterUpdates = containerHelper => kvs => () =>
  containerHelper.packMap(lib.ProposedProtocolParameterUpdates, kvs);
