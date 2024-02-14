import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";

const call = property => object => object[property]();
const callMaybe = property => maybe => object => {
  const res = object[property]();
  return res != null ? maybe.just(res) : maybe.nothing;
};

export function _txIsValid(tx) {
  return tx.is_valid();
}

export function _txWitnessSet(tx) {
  return tx.witness_set();
}

export function _txBody(tx) {
  return tx.body();
}

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

export const _txAuxiliaryData = maybeGetter("auxiliary_data");
export const _adGeneralMetadata = maybeGetter("metadata");
export const _adNativeScripts = maybeGetter("native_scripts");
export const _adPlutusScripts = maybeGetter("plutus_scripts");

// inputs(): TransactionInputs;
export function _txBodyInputs(containerhelper) {
  return body => containerhelper.unpack(body.inputs());
}

// outputs(): TransactionOutputs;
export function _txBodyOutputs(containerhelper) {
  return body => containerhelper.unpack(body.outputs());
}

// fee(): BigNum;
export function _txBodyFee(body) {
  return body.fee();
}

// ttl(): number | void;
export const _txBodyTtl = maybeGetter("ttl_bignum");

// certs(): Certificates | void;
export const _txBodyCerts = maybeGetterMulti("certs");

// withdrawals(): Withdrawals | void;
export const _txBodyWithdrawals = maybeGetter("withdrawals");

// update(): Update | void;
export const _txBodyUpdate = maybeGetter("update");

// auxiliary_data_hash(): AuxiliaryDataHash | void;
export const _txBodyAuxiliaryDataHash = maybeGetter("auxiliary_data_hash");

// validity_start_interval(): number | void;
export const _txBodyValidityStartInterval = maybeGetter(
  "validity_start_interval_bignum"
);

// multiassets(): Mint | void;
export const _txBodyMultiAssets = maybeGetter("multiassets");

export function _txBodyReferenceInputs(maybe) {
  return containerhelper => body =>
    body.reference_inputs()
      ? maybe.just(containerhelper.unpack(body.reference_inputs()))
      : maybe.nothing;
}

// script_data_hash(): ScriptDataHash | void;
export const _txBodyScriptDataHash = maybeGetter("script_data_hash");

// collateral(): Array TransactionInput | void;
export const _txBodyCollateral = maybeGetterMulti("collateral");

// required_signers(): Ed25519KeyHashes | void;
export const _txBodyRequiredSigners = maybeGetterMulti("required_signers");

// network_id(): number | void;
export function _txBodyNetworkId(testnet) {
  return mainnet =>
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
}

// collateral_return(): TransactionOutput | void;
export const _txBodyCollateralReturn = maybeGetter("collateral_return");

// total_collateral(): BigNum | void
export const _txBodyTotalCollateral = maybeGetter("total_collateral");

// foreign import _unpackWithdrawals :: ContainerHelper -> CSL.Withdrawals -> Array(Tuple CSL.RewardAddress CSL.BigNum)
export function _unpackWithdrawals(containerhelper) {
  return containerhelper.unpackKeyIndexed;
}

export function _unpackUpdate(containerhelper) {
  return update => {
    const pppus = containerhelper.unpackKeyIndexed(
      update.proposed_protocol_parameter_updates()
    );
    return { epoch: update.epoch(), paramUpdates: pppus };
  };
}

export function _unpackMint(containerhelper) {
  return containerhelper.unpackKeyIndexed;
}

export function _unpackMintAssets(containerhelper) {
  return containerhelper.unpackKeyIndexed;
}

export function _convertCert(certConvHelper) {
  return cert => {
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
            return certConvHelper.moveInstantaneousRewardsToStakeCreds(
              mir.pot()
            )(mir.as_to_stake_creds());
          default:
            throw (
              "MoveInstantaneousReward convertion failed for kind" + mir.kind()
            );
        }
      default:
        throw ("Cert conversion failed for kind: ", cert.kind());
    }
  };
}

export function _unpackProtocolParamUpdate(maybe) {
  return ppu => {
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
      collateralPercentage: optional(ppu.collateral_percentage()),
      maxCollateralInputs: optional(ppu.max_collateral_inputs())
    };
  };
}

export function _unpackCostModels(containerhelper) {
  return containerhelper.unpackKeyIndexed;
}

export function _unpackCostModel(cm) {
  const res = [];
  for (let op = 0; op < cm.len(); op++) {
    res.push(cm.get(op).to_str());
  }
  return res;
}

export function _convertNonce(nonceCtors) {
  return cslNonce => {
    const hashBytes = cslNonce.get_hash();
    return hashBytes == null
      ? nonceCtors.identityNonce
      : nonceCtors.hashNonce(hashBytes);
  };
}

export function _unpackMetadatums(containerHelper) {
  return containerHelper.unpackKeyIndexed;
}

export function _unpackMetadataMap(containerHelper) {
  return containerHelper.unpackKeyIndexed;
}

export function _unpackMetadataList(containerHelper) {
  return containerHelper.unpack;
}

export function _convertMetadatum(metadataCtors) {
  return cslMetadatum => {
    switch (cslMetadatum.kind()) {
      case lib.TransactionMetadatumKind.MetadataMap:
        return metadataCtors.from_map(cslMetadatum.as_map());
      case lib.TransactionMetadatumKind.MetadataList:
        return metadataCtors.from_list(cslMetadatum.as_list());
      case lib.TransactionMetadatumKind.Int:
        return metadataCtors.from_int(cslMetadatum.as_int());
      case lib.TransactionMetadatumKind.Bytes:
        return metadataCtors.from_bytes(cslMetadatum.as_bytes());
      case lib.TransactionMetadatumKind.Text:
        return metadataCtors.from_text(cslMetadatum.as_text());
      default:
        throw "Could not convert to known types.";
    }
  };
}

export function _unpackExUnits(exunits) {
  return {
    mem: exunits.mem(),
    steps: exunits.steps()
  };
}

export function _unpackUnitInterval(ui) {
  return {
    numerator: ui.numerator(),
    denominator: ui.denominator()
  };
}

export function _unpackProtocolVersion(cslPV) {
  return {
    major: cslPV.major(),
    minor: cslPV.minor()
  };
}

export function _unpackExUnitsPrices(cslEup) {
  return {
    memPrice: cslEup.mem_price(),
    stepPrice: cslEup.step_price()
  };
}

export const poolParamsOperator = call("operator");
export const poolParamsVrfKeyhash = call("vrf_keyhash");
export const poolParamsPledge = call("pledge");
export const poolParamsCost = call("cost");
export const poolParamsMargin = call("margin");
export const poolParamsRewardAccount = call("reward_account");

export function poolParamsPoolOwners(containerHelper) {
  return poolParams => containerHelper.unpack(poolParams.pool_owners());
}

export function poolParamsRelays(containerHelper) {
  return poolParams => containerHelper.unpack(poolParams.relays());
}

export const poolParamsPoolMetadata = callMaybe("pool_metadata");

export function convertRelay_(helper) {
  return relay => {
    switch (relay.kind()) {
      case lib.RelayKind.SingleHostAddr:
        return helper.asSingleHostAddr(relay.as_single_host_addr());
      case lib.RelayKind.SingleHostName:
        return helper.asSingleHostName(relay.as_single_host_name());
      case lib.RelayKind.MultiHostName:
        return helper.asMultiHostName(relay.as_multi_host_name());
      default:
        throw "convertRelay_: impossible happened: invalid Relay";
    }
  };
}

export function convertIpv6_(ipv6) {
  return ipv6.ip();
}

export function convertIpv4_(ipv6) {
  return ipv6.ip();
}

export function convertSingleHostAddr_(maybe) {
  return cont => singleHostAddr => {
    const port = singleHostAddr.port();
    const ipv4 = singleHostAddr.ipv4();
    const ipv6 = singleHostAddr.ipv6();

    return cont(port ? maybe.just(port) : maybe.nothing)(
      ipv4 ? maybe.just(ipv4) : maybe.nothing
    )(ipv6 ? maybe.just(ipv6) : maybe.nothing);
  };
}

export function convertSingleHostName_(maybe) {
  return cont => singleHostName => {
    const port = singleHostName.port();
    return cont(port ? maybe.just(port) : maybe.nothing)(
      singleHostName.dns_name().record()
    );
  };
}

export function convertMultiHostName_(multiHostName) {
  return multiHostName.dns_name().record();
}

export function unpackMIRToStakeCredentials_(containerHelper) {
  return mirToStakeCredentials =>
    containerHelper.unpackKeyIndexed(mirToStakeCredentials);
}

export function convertPoolMetadata_(cont) {
  return poolMetadata =>
    cont(poolMetadata.url().url())(poolMetadata.pool_metadata_hash());
}
