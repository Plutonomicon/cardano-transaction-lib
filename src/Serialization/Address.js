const CardanoWasm = require("@ngua/cardano-serialization-lib-nodejs");


const callClassStaticMaybe = (classname, functionname) => maybe => input => {
    var ret = null;
    try {
        ret = CardanoWasm[classname][functionname](input);
    }
    catch (e) {
        // console.log(e);
    }
    if (ret == null) {
        return maybe.nothing;
    }
    return maybe.just(ret);
};

const callMethodParameterless = methodname => object => {
    return object[methodname]();
};
const callToAddress = callMethodParameterless("to_address");
const callToBytes = callMethodParameterless("to_bytes");
const callToBech32 = callMethodParameterless("to_bech32");
const callNetworkId = callMethodParameterless("network_id");
const callPaymentCred = callMethodParameterless("payment_cred");
const callStakeCred = callMethodParameterless("stake_cred");

// :: forall a. { onKeyHash:: Ed25519KeyHash -> a, onScriptHash :: ScriptHash -> a } -> StakeCredential -> a
exports.withStakeCredential = cbObj => stakeCred => {
    const keyhash = stakeCred.to_keyhash();
    return keyhash
        ? cbObj.onKeyHash(keyhash)
        : cbObj.onScriptHash(stakeCred.to_scripthash());
};

exports.keyHashCredential = CardanoWasm.StakeCredential.from_keyhash;
exports.scriptHashCredential = CardanoWasm.StakeCredential.from_scripthash;


exports.addressBytes = callToBytes;
exports.byronAddressBytes = callToBytes;
exports.stakeCredentialToBytes = callToBytes;

exports.addressBech32 = callToBech32;
exports.addressNetworkId = callNetworkId;
exports.byronAddressNetworkId = callNetworkId;

exports._addressFromBytes = callClassStaticMaybe('Address','from_bytes');
exports._stakeCredentialFromBytes = callClassStaticMaybe('StakeCredential', 'from_bytes');
exports._byronAddressFromBytes = callClassStaticMaybe('ByronAddress', 'from_bytes');

exports._addressFromBech32 = callClassStaticMaybe('Address', 'from_bech32');

exports._byronAddressFromBase58 = callClassStaticMaybe('ByronAddress', 'from_base58');

exports._baseAddressFromAddress = callClassStaticMaybe('BaseAddress', 'from_address');
exports._byronAddressFromAddress = callClassStaticMaybe('ByronAddress', 'from_address');
exports._enterpriseAddressFromAddress = callClassStaticMaybe('EnterpriseAddress', 'from_address');
exports._pointerAddressFromAddress = callClassStaticMaybe('PointerAddress', 'from_address');
exports._rewardAddressFromAddress = callClassStaticMaybe('RewardAddress', 'from_address');

exports.baseAddressToAddress = callToAddress;
exports.byronAddressToAddress = callToAddress;
exports.enterpriseAddressToAddress = callToAddress;
exports.pointerAddressToAddress = callToAddress;
exports.rewardAddressToAddress = callToAddress;

exports.baseAddressPaymentCred = callPaymentCred;
exports.rewardAddressPaymentCred = callPaymentCred;
exports.enterpriseAddressPaymentCred = callPaymentCred;
exports.pointerAddressPaymentCred = callPaymentCred;

exports.baseAddressDelegationCred = callStakeCred;

exports.byronAddressAttributes = callMethodParameterless('attributes');
exports.byronAddressIsValid = CardanoWasm.ByronAddress.is_valid;
exports.byronAddressToBase58 = callMethodParameterless('to_base58');
exports.byronProtocolMagic = callMethodParameterless('byron_protocol_magic');

exports.icarusFromKey = bip32pubkey => byronProtocolMagic => {
    return CardanoWasm.ByronAddress.icarus_from_key(bip32pubkey, byronProtocolMagic);
};

exports.pointerAddressStakePointer = pa => {
    const pointerForeign = pa.stake_pointer();
    return { slot: pointerForeign.slot(),
             txIx: pointerForeign.tx_index(),
             certIx: pointerForeign.cert_index() };
};

// newEnterpriseAddress :: { network:: NetworkId, paymentCred :: StakeCredential } -> EnterpriseAddress
exports.enterpriseAddress = inpRec => {
    return CardanoWasm.EnterpriseAddress.new(inpRec.network, inpRec.paymentCred);
};

// newRewardAddress :: { network:: NetworkId, paymentCred :: StakeCredential } -> RewardAddress
exports.rewardAddress = inpRec => {
    return CardanoWasm.RewardAddress.new(inpRec.network, inpRec.paymentCred);
};

// newBaseAddress ::
//    { network :: NetworkId, paymentCred :: StakeCredential, delegationCred :: StakeCredential } -> BaseAddress
exports.baseAddress = inpRec => {
    return CardanoWasm.BaseAddress.new(inpRec.network, inpRec.paymentCred, inpRec.delegationCred);
};

// newPointerAddress :: { network:: NetworkId, paymentCred :: StakeCredential, stakePointer :: Pointer } -> PointerAddress
exports.pointerAddress = inpRec => {
    const p =  inpRec.stakePointer;
    const pointerForeign = CardanoWasm.Pointer.new(p.slot,p.txIx, p.certIx);
    return CardanoWasm.PointerAddress.new(inpRec.network, inpRec.paymentCred, pointerForeign);
};
