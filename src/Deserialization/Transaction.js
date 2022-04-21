
exports._txIsValid = tx => tx.is_valid();
exports._txWitnessSet = tx => tx.witness_set();
exports._txBody = tx => tx.body();
exports._txAuxiliaryData = maybe => tx => {
    ad = tx.auxiliary_data();
    return ad == null ? maybe.nothing : maybe.just(ad);
};


const maybeGetter_ = fmap => propstr => maybe => obj => {
    const res = obj[propstr]();
    return res == null ? maybe.nothing : maybe.just(fmap(res));
};
const maybeGetter = maybeGetter_ (a => a);
const maybeGetterMulti = containerHelper => maybeGetter_(o => containerHelper.unpack(o));

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
exports._txBodyTtl = maybeGetter("ttl");
// certs(): Certificates | void;
exports._txBodyCerts = maybeGetter("certs");
// withdrawals(): Withdrawals | void;
exports._txBodyWithdrawals = maybeGetter("withdrawals");
// update(): Update | void;
exports._txBodyUpdate = maybeGetter("update");
// auxiliary_data_hash(): AuxiliaryDataHash | void;
exports._txBodyAuxiliaryDataHash = maybeGetter("auxiliary_data_hash");
// validity_start_interval(): number | void;
exports._txBodyValidityStartInterval = maybeGetter("validity_start_interval");
// mint(): Mint | void;
exports._txBodyMint = maybeGetter("mint");
// multiassets(): Mint | void;
exports._txBodyMultiAssets = maybeGetter("multiassets");
// script_data_hash(): ScriptDataHash | void;
exports._txBodyScriptDataHash = maybeGetter("script_data_hash");
// collateral(): Array TransactionInput | void;
exports._txBodyCollateral = maybeGetterMulti("collateral");
// required_signers(): Ed25519KeyHashes | void;
exports._txBodyRequiredSigners = maybeGetterMulti("required_signers");
// network_id(): number | void;
exports._txBodyNetworkId = maybeGetter_(o => o.kind())("network_id");



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


// type CertConvHelper r =
// {
//     stakeDeregistration:: StakeCredential -> Err r Certificate
//         , stakeRegistration :: StakeCredential -> Err r Certificate
//             , stakeDelegation ::
//     StakeCredential -> Ed25519KeyHash -> Err r Certificate
//         , notImplementedError :: String -> Err r Certificate
// }
// foreign import _convertCert :: forall r.CertConvHelper r -> CSL.Certificate -> Err r Certificate
exports._convertCert = certConvHelper => cert => {
    // StakeRegistration,
    var r = cert.as_stake_registration();
    if (r) return certConvHelper.stakeRegistration(r.stake_credential());
    //     StakeDeregistration,
    r = cert.as_stake_deregistration();
    if (r) return certConvHelper.stakeDeregistration(r.stake_credential());
    //     StakeDeregistration,
    r = cert.as_stake_delegation();
    if (r) return certConvHelper.stakeDelegation(r.stake_credential())(r.pool_keyhash());
    certConvHelper.notImplementedError("Cert conversion not implemented for kind: ", cert.kind());
};


// foreign import _unpackProtocolParamUpdate :: CSL.ProtocolParamUpdate -> 
//   { minfeeA :: Maybe CSL.BigNum
//   , minfeeB :: Maybe CSL.BigNum
//   , maxBlockBodySize :: Maybe Number
//   , maxTxSize :: Maybe Number
//   , maxBlockHeaderSize :: Maybe Number
//   , keyDeposit :: Maybe CSL.BigNum
//   , poolDeposit :: Maybe CSL.BigNum
//   , maxEpoch :: Maybe Number
//   , nOpt :: Maybe Number
//   , poolPledgeInfluence :: Maybe { numerator :: CSL.BigNum, denominator :: CSL.BigNum }
//   , expansionRate :: Maybe   { numerator :: CSL.BigNum, denominator :: CSL.BigNum}
//   , treasuryGrowthRate :: Maybe { numerator :: CSL.BigNum, denominator :: CSL.BigNum }
//   , d :: Maybe   { numerator :: CSL.BigNum, denominator :: CSL.BigNum  }
//   , extraEntropy :: Maybe CSL.Nonce
//   , protocolVersion :: Maybe (Array {major :: Number, minor :: Number})
//   , minPoolCost :: Maybe CSL.BigNum
//   , adaPerUtxoByte :: Maybe CSL.BigNum
//   , costModels :: Maybe CSL.Costmdls
//   , executionCosts :: Maybe { memPrice :: { numerator :: CSL.BigNum, denominator :: CSL.BigNum }, stepPrice :: { numerator :: CSL.BigNum, denominator :: CSL.BigNum }}
//   , maxTxExUnits :: Maybe { mem :: CSL.BigNum, steps :: CSL.BigNum }
//   , maxBlockExUnits :: Maybe { mem :: CSL.BigNum, steps :: CSL.BigNum }
//   , maxValueSize :: Maybe Number
//   , collateralPercentage :: Maybe Number
//   , maxCollateralInputs :: Maybe Number
//   }
exports._unpackProtocolParamUpdate = maybe => protocolParamUpdate => {throw "not implemented";};

// foreign import _unpackCostModels :: ContainerHelper -> CSL.Costmdls -> Array(Tuple CSL.Language CSL.CostModel)
exports._unpackCostModels = containerhelper => containerhelper.unpackKeyIndexed;

// foreign import unpackCostModel :: CSL.CostModel -> Array String
exports._unpackCostModel = cm => {
    // XXX should OP_COUNT be used instead?
    var op = 0;
    var err = false;
    const res = [];
    while(!err){
        try{
            res.push(cm.get(op).to_str());
        }
        catch(_){
            err=true;
        }
    }
    return res;
};

// foreign import _convertLanguage
//   :: forall r.ErrorFfiHelper r -> { plutusV1:: Language } -> CSL.Language -> E r Language
exports._convertLanguage = errorHelper => langCtors => cslLang => {
    try{
        if(cslLang.kind()==0){
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
