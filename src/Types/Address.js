const CardanoWasm = require("cardano-serialization-lib");


// newBaseAddress :: NetworkId -> PubKeyHash -> StakeKeyHash -> BaseAddress
exports.newBaseAddress = netId => pkhBech32 => skhBech32 => () => {
    pkh = CardanoWasm.Ed25519KeyHash.from_bech32(pkhBech32);
    if (pkh == null){
        console.error(`error: Ed25519KeyHash.from_bech32(${pkhBech32}) returned null.`);
    }
    skh = CardanoWasm.Ed25519KeyHash.from_bech32(skhBech32);
    if (skh == null) {
        console.error(`error: Ed25519KeyHash.from_bech32(${skhBech32}) returned null.`);
    }
    const addr = CardanoWasm.BaseAddress.new(
        netId,
        CardanoWasm.StakeCredential.from_keyhash(pkh),
        CardanoWasm.StakeCredential.from_keyhash(skh));
    return addr;
};


// addressNetworkId :: BaseAddress -> NetworkId
exports.addressNetworkId = baseAddr => () => {
    return baseAddr.to_address().network_id();
};


// fromBech32 :: (forall x.x -> Maybe x) -> (forall x.Maybe x) -> Bech32String -> Maybe BaseAddress
exports.fromBech32Impl = just => nothing => bech32str => () => {
    try {
        addr = CardanoWasm.Address.from_bech32(bech32str);
        baseAddr = CardanoWasm.BaseAddress.from_address();
        return just(baseAddr);
    } catch (error) {
        console.log('BaseAddress.fromBech32 failed with error:', error);
        return nothing;
    }
};

// addressBech32 :: BaseAddress -> Bech32String
exports.addressBech32 = baseAddr => () => {
    return baseAddr.to_bech32();
};

// addressPubKeyHash :: BaseAddress -> PubKeyHash
exports.addressPubKeyHash = baseAddr => () => {
    return baseAddr.payment_cred().to_keyhash().to_bech32();
};

//addressStakeKeyHash :: BaseAddress -> StakeKeyHash
exports.addressStakeKeyHash = baseAddr => () => {
    return baseAddr.stake_cred().to_keyhash().to_bech32();
};
