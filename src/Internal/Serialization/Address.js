/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

const callClassStaticMaybe = (classname, functionname) => maybe => input => {
  let ret = null;
  try {
    ret = lib[classname][functionname](input);
  } catch (_) {
    // ignored
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

exports.withStakeCredential = cbObj => stakeCred => {
  const keyhash = stakeCred.to_keyhash();
  return keyhash
    ? cbObj.onKeyHash(keyhash)
    : cbObj.onScriptHash(stakeCred.to_scripthash());
};

exports.keyHashCredential = lib.StakeCredential.from_keyhash;
exports.scriptHashCredential = lib.StakeCredential.from_scripthash;

exports.addressBytes = callToBytes;
exports.byronAddressBytes = callToBytes;
exports.stakeCredentialToBytes = callToBytes;

exports.addressBech32 = callToBech32;
exports._addressNetworkId = toAdt => addr => {
  return toAdt(callNetworkId(addr));
};
exports._byronAddressNetworkId = toAdt => addr => {
  return toAdt(callNetworkId(addr));
};

exports._addressFromBytes = callClassStaticMaybe("Address", "from_bytes");
exports._stakeCredentialFromBytes = callClassStaticMaybe(
  "StakeCredential",
  "from_bytes"
);
exports._byronAddressFromBytes = callClassStaticMaybe(
  "ByronAddress",
  "from_bytes"
);

exports._addressFromBech32 = callClassStaticMaybe("Address", "from_bech32");

exports._byronAddressFromBase58 = callClassStaticMaybe(
  "ByronAddress",
  "from_base58"
);

exports._baseAddressFromAddress = callClassStaticMaybe(
  "BaseAddress",
  "from_address"
);
exports._byronAddressFromAddress = callClassStaticMaybe(
  "ByronAddress",
  "from_address"
);
exports._enterpriseAddressFromAddress = callClassStaticMaybe(
  "EnterpriseAddress",
  "from_address"
);
exports._pointerAddressFromAddress = callClassStaticMaybe(
  "PointerAddress",
  "from_address"
);
exports._rewardAddressFromAddress = callClassStaticMaybe(
  "RewardAddress",
  "from_address"
);

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

exports.byronAddressAttributes = callMethodParameterless("attributes");
exports.byronAddressIsValid = lib.ByronAddress.is_valid;
exports.byronAddressToBase58 = callMethodParameterless("to_base58");
exports.byronProtocolMagic = callMethodParameterless("byron_protocol_magic");

exports.icarusFromKey = bip32pubkey => byronProtocolMagic => {
  return lib.ByronAddress.icarus_from_key(bip32pubkey, byronProtocolMagic);
};

exports.pointerAddressStakePointer = pa => {
  const pointerForeign = pa.stake_pointer();
  return {
    slot: pointerForeign.slot_bignum(),
    txIx: pointerForeign.tx_index_bignum(),
    certIx: pointerForeign.cert_index_bignum(),
  };
};

exports._enterpriseAddress = netIdToInt => inpRec => {
  return lib.EnterpriseAddress.new(
    netIdToInt(inpRec.network),
    inpRec.paymentCred
  );
};

exports._rewardAddress = netIdToInt => inpRec => {
  return lib.RewardAddress.new(netIdToInt(inpRec.network), inpRec.paymentCred);
};

exports._baseAddress = netIdToInt => inpRec => {
  return lib.BaseAddress.new(
    netIdToInt(inpRec.network),
    inpRec.paymentCred,
    inpRec.delegationCred
  );
};

exports._pointerAddress = netIdToInt => inpRec => {
  const p = inpRec.stakePointer;
  const pointerForeign = lib.Pointer.new_pointer(p.slot, p.txIx, p.certIx);
  return lib.PointerAddress.new(
    netIdToInt(inpRec.network),
    inpRec.paymentCred,
    pointerForeign
  );
};
