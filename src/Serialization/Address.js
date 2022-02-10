var CardanoWasm;
if (typeof BROWSER_RUNTIME != 'undefined' && BROWSER_RUNTIME) {
    CardanoWasm = require('@ngua/cardano-serialization-lib-browser');
} else {
    CardanoWasm = require('@ngua/cardano-serialization-lib-nodejs');
}


exports._stakeCredFromKeyHash = validEd25519KeyHash => {
    return CardanoWasm.StakeCredential.from_keyhash(validEd25519KeyHash);
};

exports._stakeCredFromScriptHash = validScriptHash => {
    return CardanoWasm.StakeCredential.from_scripthash(validScriptHash);
};

// it is safe to use only with valid arguments
exports._newBaseAddressCsl = checkedArgs => {
    return CardanoWasm.BaseAddress.new(
        checkedArgs.network,
        checkedArgs.paymentStakeCred,
        checkedArgs.delegationStakeCred);
};

// it is safe to use only with valid arguments
exports._newRewardAddressCsl = checkedArgs => {
    return CardanoWasm.RewardAddress.new(
        checkedArgs.network,
        checkedArgs.paymentStakeCred);
};

exports._baseAddressFromBytesImpl = maybe => bytes => {
    var ret = null;
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

exports._rewardAddressFromBytesImpl = maybe => bytes => {
    var ret = null;
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

exports._baseAddressFromBech32Impl = maybe => str => {
    var ret = null;
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
    console.log(ret);
    return maybe.just(ret);
};

exports._rewardAddressFromBech32Impl = maybe => str => {
    var ret = null;
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


exports._headerCheckBaseAddr = maybe => checks => intToNetTag => baseAddr => {
    if (!baseAddr) return maybe.nothing;

    const pm = checks.payment == "from_keyhash"
        ? baseAddr.payment_cred().to_keyhash()
        : baseAddr.payment_cred().to_scripthash();
    if (!pm) return maybe.nothing;

    const deleg = checks.delegation == "from_keyhash"
          ? baseAddr.stake_cred().to_keyhash()
          : baseAddr.stake_cred().to_scripthash();
    if (!deleg) return maybe.nothing;

    const net = maybe.from(null)(intToNetTag(baseAddr.to_address().network_id()));
    if (!net) return maybe.nothing;

    return maybe.just({
        network: net,
        payment: pm,
        delegation: deleg
    });

};

exports._headerCheckRewardAddr = maybe => checks => intToNetTag => rewAddr => {
    if (!rewAddr) return maybe.nothing;
    const pm = checks.payment == "from_keyhash"
          ? rewAddr.payment_cred().to_keyhash()
          : rewAddr.payment_cred().to_scripthash();

    if (!pm) return maybe.nothing;

    const net = maybe.from(null)(intToNetTag(rewAddr.to_address().network_id()));
    if (!net) return maybe.nothing;

    return maybe.just({
        network: net,
        payment: pm
    });
};


exports._toAddressCslUnsafe = addresType => {
    return addresType.to_address();
};

exports._addressBytesImpl = baseAddr => {
    return baseAddr.to_bytes();
};

exports._addressBech32Impl = baseAddr => {
    return baseAddr.to_bech32();
};
