const CardanoWasm = require("@emurgo/cardano-serialization-lib-nodejs");


exports.stakeCredFromKeyHash_ = validEd25519KeyHash => {
    return CardanoWasm.StakeCredential.from_keyhash(validEd25519KeyHash);
};

exports.stakeCredFromScriptHash_ = validScriptHash => {
    return CardanoWasm.StakeCredential.from_keyhash(validScriptHash);
};

// it is safe to use only with valid arguments
exports.newBaseAddressCsl_ = checkedArgs => {
    return CardanoWasm.BaseAddress.new(
        checkedArgs.network,
        checkedArgs.paymentStakeCred,
        checkedArgs.delegationStakeCred);
};

// it is safe to use only with valid arguments
exports.newRewardAddressCsl_ = checkedArgs => {
    return CardanoWasm.RewardAddress.new(
        checkedArgs.network,
        checkedArgs.paymentStakeCred);
};

exports.baseAddressFromBytesImpl_ = maybe => bytes => {
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

exports.rewardAddressFromBytesImpl_ = maybe => bytes => {
    const ret = null;
    try {
        const addr = CardanoWasm.Address.from_bytes(bytes);
        ret = CardanoWasm.RewardAddress.from_address(addr);
    }
    catch (e) {
        console.log(e);
    }
    if (ret == null) {
        return maybe.nothing;
    }
    return maybe.just(ret);
};

exports.baseAddressFromBech32Impl_ = maybe => str => {
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

exports.rewardAddressFromBech32Impl_ = maybe => str => {
    const ret = null;
    try {
        const addr = CardanoWasm.Address.from_bech32(str);
        ret = CardanoWasm.RewardAddress.from_address(addr);
    }
    catch (e) {
        console.log(e);
    }
    if (ret == null) {
        return maybe.nothing;
    }
    return maybe.just(ret);
};

exports.headerCheckBaseAddr_ = maybe => checks => bytes => baseAddr => {
    // NOTE from CIP-19 and CSL codebase:
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

exports.headerCheckRewardAddr_ = maybe => checks => bytes => baseAddr => {
    // NOTE from CIP-19 and CSL codebase:
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
    if (paymcheck != null && !checkBitSet(bytes, 4, paymcheck)) {
        return maybe.nothing;
    }
    return maybe.just(baseAddr);
};


exports.toAddressCslUnsafe_ = addresType => {
    return addresType.to_address();
};

exports.addressBytesImpl_ = baseAddr => {
    return baseAddr.to_bytes();
};

exports.addressBech32Impl_ = baseAddr => {
    return baseAddr.to_bech32();
};
