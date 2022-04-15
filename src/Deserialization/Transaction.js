
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

// foreign import _convertCert :: forall r.CertConvHelper r -> CSL.Certificate -> Err r Certificate
exports._convertCert = certConvHelper => cert => certConvHelper.notImplementedError ("Cert conversion not implemented.")
