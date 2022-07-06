/* global require exports BROWSER_RUNTIME */

var lib;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    lib = require('@emurgo/cardano-serialization-lib-browser');
} else {
    lib = require('@emurgo/cardano-serialization-lib-nodejs');
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
    ad = tx.auxiliary_data();
    return ad == null ? maybe.nothing : maybe.just(ad);
};


const maybeGetter_ = fmap => propstr => maybe => obj => {
    if (typeof(propstr) != 'string'){
        const s = "maybeGetter_ propstr must be a string, got " + propstr;
        throw s;
    }
    const res = obj[propstr]();
    return res == null ? maybe.nothing : maybe.just(fmap(res));
};
const maybeGetter = maybeGetter_ (a => a);
const maybeGetterMulti = propstr => containerHelper => maybeGetter_(o => containerHelper.unpack(o))(propstr);

exports._txAuxiliaryData = maybeGetter("auxiliary_data");
exports._adGeneralMetadata = maybeGetter("metadata");
exports._adNativeScripts = maybeGetter("native_scripts");
exports._adPlutusScripts = maybeGetter("plutus_scripts");

// inputs(): TransactionInputs;
exports._txBodyInputs = containerhelper => body => containerhelper.unpack(body.inputs());
// outputs(): TransactionOutputs;
exports._txBodyOutputs = containerhelper => body => containerhelper.unpack(body.outputs());
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
exports._txBodyValidityStartInterval = maybeGetter("validity_start_interval_bignum");
// multiassets(): Mint | void;
exports._txBodyMultiAssets = maybeGetter("multiassets");
exports._txBodyReferenceInputs = maybe => containerhelper => body =>
    body.reference_inputs() ?
    maybe.just(containerhelper.unpack(body.reference_inputs())) :
    maybe.nothing;
// script_data_hash(): ScriptDataHash | void;
exports._txBodyScriptDataHash = maybeGetter("script_data_hash");
// collateral(): Array TransactionInput | void;
exports._txBodyCollateral = maybeGetterMulti("collateral");
// required_signers(): Ed25519KeyHashes | void;
exports._txBodyRequiredSigners = maybeGetterMulti("required_signers");
// network_id(): number | void;
exports._txBodyNetworkId = testnet => mainnet => maybeGetter_(
    o => {
        switch (o.kind()) {
        case lib.NetworkIdKind.Testnet:
            return testnet;
        case lib.NetworkIdKind.Mainnet:
            return mainnet;
        default:
            throw ("Unknown NetworkIdKind: " + o.kind());
        }
    }
)("network_id");

// collateral_return(): TransactionOutput | void;
exports._txBodyCollateralReturn = maybeGetter("collateral_return");

// total_collateral(): BigNum | void
exports._txBodyTotalCollateral = maybeGetter("total_collateral");

// foreign import _unpackWithdrawals :: ContainerHelper -> CSL.Withdrawals -> Array(Tuple CSL.RewardAddress CSL.BigNum)
exports._unpackWithdrawals = containerhelper => containerhelper.unpackKeyIndexed;

// foreign import _unpackUpdate :: (forall a b.a -> b -> Tuple a b) -> CSL.Update -> { epoch:: Int, paramUpdates:: Array(Tuple GenesisHash CSL.ProtocolParamUpdate) }
exports._unpackUpdate = containerhelper => update => {
    const pppus = containerhelper.unpackKeyIndexed(update.proposed_protocol_parameter_updates());
    return { epoch: update.epoch(), paramUpdates: pppus};
};

// foreign import _unpackMint :: ContainerHelper -> CSL.Mint -> Array(Tuple ScriptHash MintAssets  )
exports._unpackMint = containerhelper => containerhelper.unpackKeyIndexed;

// foreign import _unpackMintAssets :: ContainerHelper -> CSL.MintAssets -> Array (Tuple CSL.AssetName Int)
exports._unpackMintAssets = containerhelper => containerhelper.unpackKeyIndexed;

// type CertConvHelper (r :: Row Type) =
//   { stakeDeregistration :: Csl.StakeCredential -> Err r T.Certificate
//   , stakeRegistration :: Csl.StakeCredential -> Err r T.Certificate
//   , stakeDelegation ::
//       Csl.StakeCredential -> Ed25519KeyHash -> Err r T.Certificate
//   , poolRegistration :: Csl.PoolParams -> Err r T.Certificate
//   , poolRetirement :: Ed25519KeyHash -> Int -> Err r T.Certificate
//   , genesisKeyDelegation ::
//       Csl.GenesisHash
//       -> Csl.GenesisDelegateHash
//       -> Csl.VRFKeyHash
//       -> Err r T.Certificate
//   , moveInstantaneousRewardsToOtherPotCert ::
//       Number -> Csl.BigNum -> Err r T.Certificate
//   , moveInstantaneousRewardsToStakeCreds ::
//       Number -> Csl.MIRToStakeCredentials -> Err r T.Certificate
//   }
// foreign import _convertCert :: forall r.CertConvHelper r -> CSL.Certificate -> Err r Certificate
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
        )(
            cert.as_stake_delegation().pool_keyhash()
        );
    case lib.CertificateKind.PoolRegistration:
        return certConvHelper.poolRegistration(
            cert.as_pool_registration().pool_params()
        );
    case lib.CertificateKind.PoolRetirement:
        return certConvHelper.poolRetirement(
            cert.as_pool_retirement().pool_keyhash()
        )(
            cert.as_pool_retirement().epoch()
        );
    case lib.CertificateKind.GenesisKeyDelegation:
        return certConvHelper.genesisKeyDelegation(
            cert.as_genesis_key_delegation().genesishash()
        )(
            cert.as_genesis_key_delegation().genesis_delegate_hash()
        )(
            cert.as_genesis_key_delegation().vrf_keyhash()
        );
    case lib.CertificateKind.MoveInstantaneousRewardsCert:
        const mirCert = cert.as_move_instantaneous_rewards_cert();
        const mir = mirCert.move_instantaneous_reward();
        switch (mir.kind()) {
        case lib.MIRKind.ToOtherPot:
            return certConvHelper.moveInstantaneousRewardsToOtherPotCert(
                mir.pot()
            )(
                mir.as_to_other_pot()
            );
        case lib.MIRKind.ToStakeCredentials:
            return certConvHelper.moveInstantaneousRewardsToStakeCreds(
                mir.pot()
            )(
                mir.as_to_stake_creds()
            );
        default:
            throw ("MoveInstantaneousReward convertion failed for kind" + mir.kind());
        };
    default:
        throw ("Cert conversion failed for kind: ", cert.kind());
    };
};


// foreign import _unpackProtocolParamUpdate
//   :: MaybeFfiHelper
//   -> Csl.ProtocolParamUpdate
//   -> { minfeeA :: Maybe Csl.BigNum
//      , minfeeB :: Maybe Csl.BigNum
//      , maxBlockBodySize :: Maybe Number
//      , maxTxSize :: Maybe Number
//      , maxBlockHeaderSize :: Maybe Number
//      , keyDeposit :: Maybe Csl.BigNum
//      , poolDeposit :: Maybe Csl.BigNum
//      , maxEpoch :: Maybe Number
//      , nOpt :: Maybe Number
//      , poolPledgeInfluence :: Maybe Csl.UnitInterval
//      , expansionRate ::
//          Maybe Csl.UnitInterval
//      , treasuryGrowthRate ::
//          Maybe Csl.UnitInterval
//      , d :: Maybe Csl.UnitInterval
//      , extraEntropy :: Maybe Csl.Nonce
//      , protocolVersion :: Maybe Csl.ProtocolVersions
//      , minPoolCost :: Maybe Csl.BigNum
//      , adaPerUtxoByte :: Maybe Csl.BigNum
//      , costModels :: Maybe Csl.Costmdls
//      , executionCosts :: Maybe Csl.ExUnitPrices
//      , maxTxExUnits :: Maybe Csl.ExUnits
//      , maxBlockExUnits :: Maybe Csl.ExUnits
//      , maxValueSize :: Maybe Number
//      }
exports._unpackProtocolParamUpdate = maybe => ppu => {
    const optional = x => (x == null) ? maybe.nothing : maybe.just(x);

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
        maxBlockExUnits:  optional(ppu.max_block_ex_units()),
        maxValueSize: optional(ppu.max_value_size())
    };
};

// foreign import _unpackCostModels :: ContainerHelper -> CSL.Costmdls -> Array(Tuple CSL.Language CSL.CostModel)
exports._unpackCostModels = containerhelper => containerhelper.unpackKeyIndexed;

// foreign import unpackCostModel :: CSL.CostModel -> Array String
exports._unpackCostModel = cm => {
    // XXX should OP_COUNT be used instead?
    var err = false;
    const res = [];
    try {
      for(var op = 0;; op++) {
        res.push(cm.get(op).to_str());
      }
    } catch (_) { }
    return res;
};

// foreign import _convertLanguage
//   :: forall r.ErrorFfiHelper r -> { plutusV1:: Language } -> CSL.Language -> E r Language
exports._convertLanguage = errorHelper => langCtors => cslLang => {
    try{
        if(cslLang.kind()==lib.LanguageKind.PlutusV1){
            return errorHelper.valid(langCtors.plutusV1);
        }
        else{
            return errorHelper.error("_convertLanguage: Unsupported language kind: " + cslLang.kind());
        }
    }
    catch(e){
        return errorHelper.error("_convertLanguage raised: " + e);
    }
};

// foreign import _convertNonce :: forall r. { identityNonce:: Nonce, hashNonce :: UInt8Array -> Nonce } -> CSL.Nonce -> Nonce
exports._convertNonce = nonceCtors => cslNonce => {
    const hashBytes = cslNonce.get_hash();
    return hashBytes == null ? nonceCtors.identityNonce : nonceCtors.hashNonce(hashBytes);
};

// foreign import _unpackMetadatums
//   :: ContainerHelper
//     -> CSL.GeneralTransactionMetadata
//     -> Array(Tuple CSL.BigNum CSL.TransactionMetadatum)
exports._unpackMetadatums = containerHelper => containerHelper.unpackKeyIndexed;

// foreign import _unpackMetadataMap :: ContainerHelper -> CSL.MetadataMap -> Array(Tuple CSL.TransactionMetadatum CSL.TransactionMetadatum)
exports._unpackMetadataMap = containerHelper => containerHelper.unpackKeyIndexed;

// foreign import _unpackMetadataList
//   :: ContainerHelper -> CSL.MetadataList -> Array CSL.TransactionMetadatum
exports._unpackMetadataList = containerHelper => containerHelper.unpack;

exports._convertMetadatum = metadataCtors => cslMetadatum => {
    // map
    var r = null;
    try{
        r = cslMetadatum.as_map();
    }
    catch(_){
        r = null;
    }
    if (r) return metadataCtors.from_map(r);
    // list
    try {
        r = cslMetadatum.as_list();
    }
    catch (_) {
        r = null;
    }
    if (r) return metadataCtors.from_list(r);

    // int
    try {
        r = cslMetadatum.as_int();
    }
    catch (_) {
        r = null;
    }
    if (r) return metadataCtors.from_int(r);

    // bytes
    try {
        r = cslMetadatum.as_bytes();
    }
    catch (_) {
        r = null;
    }
    if (r) return metadataCtors.from_bytes(r);

    // text
    try {
        r = cslMetadatum.as_text();
    }
    catch (_) {
        r = null;
    }
    if (r) return metadataCtors.from_text(r);

    return metadataCtors.error("Could not convert to known types.");
};

// foreign import _unpackExUnits
// :: Csl.ExUnits -> {mem :: Csl.BigNum, steps :: Csl.BigNum}
exports._unpackExUnits = exunits => {
    return {
      mem: exunits.mem(),
      steps: exunits.steps()
    };
};


// foreign import _unpackUnitInterval ::
// Csl.UnitInterval -> { numerator :: Csl.BigNum, denominator :: Csl.BigNum}
exports._unpackUnitInterval = ui => {
    return {
        numerator: ui.numerator(),
        denominator: ui.denominator()
    };
};

// foreign import _unpackProtocolVersions
// :: ContainerHelper -> Csl.ProtocolVersions -> Array { major :: Number, minor :: Number }
exports._unpackProtocolVersions = containerhelper => cslPV => {
    const pvs = containerhelper.unpack(cslPV);
    const res = [];
    for(var i=0; i<pvs.length; i++){
        res.push({major: pvs[i].major(), minor: pvs[i].minor()});
    }
    return res;
};


// foreign import _unpackExUnitsPrices
// :: Csl.ExUnitPrices -> {memPrice :: Csl.UnitInterval, stepPrice :: Csl.UnitInterval}
exports._unpackExUnitsPrices = cslEup => {
    return {
        memPrice: cslEup.mem_price(),
        stepPrice: cslEup.step_price(),
    };
};

exports.poolParamsOperator = call('operator');
exports.poolParamsVrfKeyhash = call('vrf_keyhash');
exports.poolParamsPledge = call('pledge');
exports.poolParamsCost = call('cost');
exports.poolParamsMargin = call('margin');
exports.poolParamsRewardAccount = call('reward_account');
exports.poolParamsPoolOwners = containerHelper => poolParams =>
    containerHelper.unpack(poolParams.pool_owners());
exports.poolParamsRelays = containerHelper => poolParams =>
    containerHelper.unpack(poolParams.relays());
exports.poolParamsPoolMetadata = callMaybe('pool_metadata');

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

    return cont(
        port ? maybe.just(port) : maybe.nothing
    )(
        ipv4 ? maybe.just(ipv4) : maybe.nothing
    )(
        ipv6 ? maybe.just(ipv6) : maybe.nothing
    );
};

exports.convertSingleHostName_ = maybe => cont => singleHostName => {
    const port = singleHostName.port();
    return cont(
        port ? maybe.just(port) : maybe.nothing
    )(
        singleHostName.dns_name().record()
    );
};

exports.convertMultiHostName_ = multiHostName =>
    multiHostName.dns_name().record();

exports.unpackMIRToStakeCredentials_ = containerHelper => mirToStakeCredentials =>
    containerHelper.unpackKeyIndexed(mirToStakeCredentials);

exports.convertPoolMetadata_ = cont => poolMetadata =>
    cont(poolMetadata.url().url())(poolMetadata.pool_metadata_hash().to_bytes());
