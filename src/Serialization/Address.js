const CardanoWasm = require("@emurgo/cardano-serialization-lib-nodejs");


exports.stakeCredFromKeyHash = validEd25519KeyHash => {
    return CardanoWasm.StakeCredential.from_keyhash(validEd25519KeyHash);
};

exports.stakeCredFromScriptHash = validScriptHash => {
    return CardanoWasm.StakeCredential.from_keyhash(validScriptHash);
};

// it is safe to use only with checked arguments
exports.newBaseAddressCsl = checkedArgs => {
    return CardanoWasm.BaseAddress.new(
        checkedArgs.networkStakeCred,
        checkedArgs.paymentStakeCred,
        checkedArgs.delegation);
};

// it is safe to use only with valid CardanoWasm.BaseAddress instance
exports.addressBytesImpl = baseAddr => {
    return baseAddr.to_bytes();
};

exports.addressFromBytesImpl = maybe => bytes => {
    const ret = null;
    try{
        const addr = CardanoWasm.Address.from_bytes(bytes);
        ret = CardanoWasm.BaseAddress.from_address(addr);
    }
    catch(e){
        console.log(e);
    }
    if (ret==null){
        return maybe.nothing;
    }
    return maybe.just(ret);
};

exports.addressFromBech32Impl = maybe => str => {
    const ret = null;
    try {
        const addr = CardanoWasm.Address.from_bech32(str);
        ret = CardanoWasm.BaseAddress.from_address(addr);
    }
    catch (e) {
        console.log(e);
    }
    if (ret == null) {
        return maybe.nothing;
    }
    return maybe.just(ret);
};

exports.headerCheck = maybe => checks => bytes => baseAddr => {
    // shelley payment addresses:
    // bit 7: 0
    // bit 6: base/other
    // bit 5: pointer/enterprise [for base: stake cred is keyhash/scripthash]
    // bit 4: payment cred is keyhash/scripthash
    // bits 3-0: network id
    //
    // reward addresses:
    // bits 7-5: 111
    // bit 4: credential is keyhash/scripthash
    // bits 3-0: network id
    //
    // byron addresses:
    // bits 7-4: 1000
    const paymcheck = maybe.from(null, checks.payment);
    if (paymcheck != null && !checkBitSet(bytes, 4, paymcheck)){
        return maybe.nothing;
    }
    const delegcheck = maybe.from(null, checks.delegation);
    if (delegcheck != null && !checkBitSet(bytes, 3, delegcheck)) {
        return maybe.nothing;
    }
    return maybe.just(baseAddr);
};
