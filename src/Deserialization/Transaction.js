/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

const call = property => object => object[property]();
const callMaybe = property => maybe => object => {
  const res = object[property]();
  return res != null ? maybe.just(res) : maybe.nothing;
};

exports._txIsValid = tx => tx.is_valid();
exports._txWitnessSet = tx => tx.witness_set();
exports._txBody = tx => tx.body();
exports._txAuxiliaryData = maybe => tx => {
  const ad = tx.auxiliary_data();
  return ad == null ? maybe.nothing : maybe.just(ad);
};

const maybeGetter_ = fmap => propstr => maybe => obj => {
  if (typeof propstr != "string") {
    const s = "maybeGetter_ propstr must be a string, got " + propstr;
    throw s;
  }
  const res = obj[propstr]();
  return res == null ? maybe.nothing : maybe.just(fmap(res));
};
const maybeGetter = maybeGetter_(a => a);
const maybeGetterMulti = propstr => containerHelper =>
  maybeGetter_(o => containerHelper.unpack(o))(propstr);

exports._txAuxiliaryData = maybeGetter("auxiliary_data");
exports._adGeneralMetadata = maybeGetter("metadata");
exports._adNativeScripts = maybeGetter("native_scripts");
exports._adPlutusScripts = maybeGetter("plutus_scripts");

// inputs(): TransactionInputs;
exports._txBodyInputs = containerhelper => body =>
  containerhelper.unpack(body.inputs());
// outputs(): TransactionOutputs;
exports._txBodyOutputs = containerhelper => body =>
  containerhelper.unpack(body.outputs());
// fee(): BigNum;
exports._txBodyFee = body => body.fee();
// ttl(): number | void;
exports._txBodyTtl = maybeGetter("ttl_bignum");
// certs(): Certificates | void;
exports._txBodyCerts = maybeGetterMulti("certs");
// withdrawals(): Withdrawals | void;
exports._txBodyWithdrawals = maybeGetter("withdrawals");
// update(): Update | void;
exports._txBodyUpdate = maybeGetter("update");
// auxiliary_data_hash(): AuxiliaryDataHash | void;
exports._txBodyAuxiliaryDataHash = maybeGetter("auxiliary_data_hash");
// validity_start_interval(): number | void;
exports._txBodyValidityStartInterval = maybeGetter(
  "validity_start_interval_bignum"
);
// multiassets(): Mint | void;
exports._txBodyMultiAssets = maybeGetter("multiassets");
exports._txBodyReferenceInputs = maybe => containerhelper => body =>
  body.reference_inputs()
    ? maybe.just(containerhelper.unpack(body.reference_inputs()))
    : maybe.nothing;
// script_data_hash(): ScriptDataHash | void;
exports._txBodyScriptDataHash = maybeGetter("script_data_hash");
// collateral(): Array TransactionInput | void;
exports._txBodyCollateral = maybeGetterMulti("collateral");
// required_signers(): Ed25519KeyHashes | void;
exports._txBodyRequiredSigners = maybeGetterMulti("required_signers");
// network_id(): number | void;
exports._txBodyNetworkId = testnet => mainnet =>
  maybeGetter_(o => {
    switch (o.kind()) {
      case lib.NetworkIdKind.Testnet:
        return testnet;
      case lib.NetworkIdKind.Mainnet:
        return mainnet;
      default:
        throw "Unknown NetworkIdKind: " + o.kind();
    }
  })("network_id");

// collateral_return(): TransactionOutput | void;
exports._txBodyCollateralReturn = maybeGetter("collateral_return");

// total_collateral(): BigNum | void
exports._txBodyTotalCollateral = maybeGetter("total_collateral");

// foreign import _unpackWithdrawals :: ContainerHelper -> CSL.Withdrawals -> Array(Tuple CSL.RewardAddress CSL.BigNum)
exports._unpackWithdrawals = containerhelper =>
  containerhelper.unpackKeyIndexed;

exports._unpackUpdate = containerhelper => update => {
  const pppus = containerhelper.unpackKeyIndexed(
    update.proposed_protocol_parameter_updates()
  );
  return { epoch: update.epoch(), paramUpdates: pppus };
};

exports._unpackMint = containerhelper => containerhelper.unpackKeyIndexed;

exports._unpackMintAssets = containerhelper => containerhelper.unpackKeyIndexed;

exports._convertCert = certConvHelper => cert => {
  switch (cert.kind()) {
    case lib.CertificateKind.StakeRegistration:
      return certConvHelper.stakeRegistration(
        cert.as_stake_registration().stake_credential()
      );
    case lib.CertificateKind.StakeDeregistration:
      return certConvHelper.stakeDeregistration(
        cert.as_stake_deregistration().stake_credential()
      );
    case lib.CertificateKind.StakeDelegation:
      return certConvHelper.stakeDelegation(
        cert.as_stake_delegation().stake_credential()
      )(cert.as_stake_delegation().pool_keyhash());
    case lib.CertificateKind.PoolRegistration:
      return certConvHelper.poolRegistration(
        cert.as_pool_registration().pool_params()
      );
    case lib.CertificateKind.PoolRetirement:
      return certConvHelper.poolRetirement(
        cert.as_pool_retirement().pool_keyhash()
      )(cert.as_pool_retirement().epoch());
    case lib.CertificateKind.GenesisKeyDelegation:
      return certConvHelper.genesisKeyDelegation(
        cert.as_genesis_key_delegation().genesishash()
      )(cert.as_genesis_key_delegation().genesis_delegate_hash())(
        cert.as_genesis_key_delegation().vrf_keyhash()
      );
    case lib.CertificateKind.MoveInstantaneousRewardsCert:
      const mirCert = cert.as_move_instantaneous_rewards_cert();
      const mir = mirCert.move_instantaneous_reward();
      switch (mir.kind()) {
        case lib.MIRKind.ToOtherPot:
          return certConvHelper.moveInstantaneousRewardsToOtherPotCert(
            mir.pot()
          )(mir.as_to_other_pot());
        case lib.MIRKind.ToStakeCredentials:
          return certConvHelper.moveInstantaneousRewardsToStakeCreds(mir.pot())(
            mir.as_to_stake_creds()
          );
        default:
          throw (
            "MoveInstantaneousReward convertion failed for kind" + mir.kind()
          );
      }
    default:
      throw ("Cert conversion failed for kind: ", cert.kind());
  }
};

exports._unpackProtocolParamUpdate = maybe => ppu => {
  const optional = x => (x == null ? maybe.nothing : maybe.just(x));

  return {
    minfeeA: optional(ppu.minfee_a()),
    minfeeB: optional(ppu.minfee_b()),
    maxBlockBodySize: optional(ppu.max_block_body_size()),
    maxTxSize: optional(ppu.max_tx_size()),
    maxBlockHeaderSize: optional(ppu.max_block_header_size()),
    keyDeposit: optional(ppu.key_deposit()),
    poolDeposit: optional(ppu.pool_deposit()),
    maxEpoch: optional(ppu.max_epoch()),
    nOpt: optional(ppu.n_opt()),
    poolPledgeInfluence: optional(ppu.pool_pledge_influence()),
    expansionRate: optional(ppu.expansion_rate()),
    treasuryGrowthRate: optional(ppu.treasury_growth_rate()),
    protocolVersion: optional(ppu.protocol_version()),
    minPoolCost: optional(ppu.min_pool_cost()),
    adaPerUtxoByte: optional(ppu.ada_per_utxo_byte()),
    costModels: optional(ppu.cost_models()),
    executionCosts: optional(ppu.execution_costs()),
    maxTxExUnits: optional(ppu.max_tx_ex_units()),
    maxBlockExUnits: optional(ppu.max_block_ex_units()),
    maxValueSize: optional(ppu.max_value_size()),
  };
};

exports._unpackCostModels = containerhelper => containerhelper.unpackKeyIndexed;

exports._unpackCostModel = cm => {
  const res = [];
  for (let op = 0; op < cm.len(); op++) {
    res.push(cm.get(op).to_str());
  }
  return res;
};

exports._convertNonce = nonceCtors => cslNonce => {
  const hashBytes = cslNonce.get_hash();
  return hashBytes == null
    ? nonceCtors.identityNonce
    : nonceCtors.hashNonce(hashBytes);
};

exports._unpackMetadatums = containerHelper => containerHelper.unpackKeyIndexed;

exports._unpackMetadataMap = containerHelper =>
  containerHelper.unpackKeyIndexed;

exports._unpackMetadataList = containerHelper => containerHelper.unpack;

exports._convertMetadatum = metadataCtors => cslMetadatum => {
  // map
  let r = null;
  try {
    r = cslMetadatum.as_map();
  } catch (_) {
    r = null;
  }
  if (r) return metadataCtors.from_map(r);
  // list
  try {
    r = cslMetadatum.as_list();
  } catch (_) {
    r = null;
  }
  if (r) return metadataCtors.from_list(r);

  // int
  try {
    r = cslMetadatum.as_int();
  } catch (_) {
    r = null;
  }
  if (r) return metadataCtors.from_int(r);

  // bytes
  try {
    r = cslMetadatum.as_bytes();
  } catch (_) {
    r = null;
  }
  if (r) return metadataCtors.from_bytes(r);

  // text
  try {
    r = cslMetadatum.as_text();
  } catch (_) {
    r = null;
  }
  if (r) return metadataCtors.from_text(r);

  return metadataCtors.error("Could not convert to known types.");
};

exports._unpackExUnits = exunits => {
  return {
    mem: exunits.mem(),
    steps: exunits.steps(),
  };
};

exports._unpackUnitInterval = ui => {
  return {
    numerator: ui.numerator(),
    denominator: ui.denominator(),
  };
};

exports._unpackProtocolVersion = cslPV => ({
  major: cslPV.major(),
  minor: cslPV.minor(),
});

exports._unpackExUnitsPrices = cslEup => {
  return {
    memPrice: cslEup.mem_price(),
    stepPrice: cslEup.step_price(),
  };
};

exports.poolParamsOperator = call("operator");
exports.poolParamsVrfKeyhash = call("vrf_keyhash");
exports.poolParamsPledge = call("pledge");
exports.poolParamsCost = call("cost");
exports.poolParamsMargin = call("margin");
exports.poolParamsRewardAccount = call("reward_account");
exports.poolParamsPoolOwners = containerHelper => poolParams =>
  containerHelper.unpack(poolParams.pool_owners());
exports.poolParamsRelays = containerHelper => poolParams =>
  containerHelper.unpack(poolParams.relays());
exports.poolParamsPoolMetadata = callMaybe("pool_metadata");

exports.convertRelay_ = helper => relay => {
  let res = relay.as_single_host_addr();
  if (res) {
    return helper.asSingleHostAddr(res);
  }

  res = relay.as_single_host_name();
  if (res) {
    return helper.asSingleHostName(res);
  }

  res = relay.as_multi_host_name();
  if (res) {
    return helper.asMultiHostName(res);
  }

  throw "convertRelay_: impossible happened: invalid Relay";
};

exports.convertIpv6_ = ipv6 => ipv6.ip();

exports.convertIpv4_ = ipv6 => ipv6.ip();

exports.convertSingleHostAddr_ = maybe => cont => singleHostAddr => {
  const port = singleHostAddr.port();
  const ipv4 = singleHostAddr.ipv4();
  const ipv6 = singleHostAddr.ipv6();

  return cont(port ? maybe.just(port) : maybe.nothing)(
    ipv4 ? maybe.just(ipv4) : maybe.nothing
  )(ipv6 ? maybe.just(ipv6) : maybe.nothing);
};

exports.convertSingleHostName_ = maybe => cont => singleHostName => {
  const port = singleHostName.port();
  return cont(port ? maybe.just(port) : maybe.nothing)(
    singleHostName.dns_name().record()
  );
};

exports.convertMultiHostName_ = multiHostName =>
  multiHostName.dns_name().record();

exports.unpackMIRToStakeCredentials_ =
  containerHelper => mirToStakeCredentials =>
    containerHelper.unpackKeyIndexed(mirToStakeCredentials);

exports.convertPoolMetadata_ = cont => poolMetadata =>
  cont(poolMetadata.url().url())(poolMetadata.pool_metadata_hash().to_bytes());
