import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";

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

export function withStakeCredential(cbObj) {
  return stakeCred => {
    return stakeCred.kind() == lib.StakeCredKind.Key
      ? cbObj.onKeyHash(stakeCred.to_keyhash())
      : cbObj.onScriptHash(stakeCred.to_scripthash());
  };
}

export const keyHashCredential = lib.StakeCredential.from_keyhash;
export const scriptHashCredential = lib.StakeCredential.from_scripthash;
export { callToBytes as addressBytes };
export { callToBytes as byronAddressBytes };
export { callToBytes as stakeCredentialToBytes };
export { callToBech32 as addressBech32 };

export function _addressNetworkId(toAdt) {
  return addr => {
    return toAdt(callNetworkId(addr));
  };
}

export function _byronAddressNetworkId(toAdt) {
  return addr => {
    return toAdt(callNetworkId(addr));
  };
}

export const _addressFromBytes = callClassStaticMaybe("Address", "from_bytes");

export const _stakeCredentialFromBytes = callClassStaticMaybe(
  "StakeCredential",
  "from_bytes"
);

export const _byronAddressFromBytes = callClassStaticMaybe(
  "ByronAddress",
  "from_bytes"
);

export const _addressFromBech32 = callClassStaticMaybe(
  "Address",
  "from_bech32"
);

export const _byronAddressFromBase58 = callClassStaticMaybe(
  "ByronAddress",
  "from_base58"
);

export const _baseAddressFromAddress = callClassStaticMaybe(
  "BaseAddress",
  "from_address"
);

export const _byronAddressFromAddress = callClassStaticMaybe(
  "ByronAddress",
  "from_address"
);

export const _enterpriseAddressFromAddress = callClassStaticMaybe(
  "EnterpriseAddress",
  "from_address"
);

export const _pointerAddressFromAddress = callClassStaticMaybe(
  "PointerAddress",
  "from_address"
);

export const _rewardAddressFromAddress = callClassStaticMaybe(
  "RewardAddress",
  "from_address"
);

export { callToAddress as baseAddressToAddress };
export { callToAddress as byronAddressToAddress };
export { callToAddress as enterpriseAddressToAddress };
export { callToAddress as pointerAddressToAddress };
export { callToAddress as rewardAddressToAddress };
export { callPaymentCred as baseAddressPaymentCred };
export { callPaymentCred as rewardAddressPaymentCred };
export { callPaymentCred as enterpriseAddressPaymentCred };
export { callPaymentCred as pointerAddressPaymentCred };
export { callStakeCred as baseAddressDelegationCred };
export const byronAddressAttributes = callMethodParameterless("attributes");
export const byronAddressIsValid = lib.ByronAddress.is_valid;
export const byronAddressToBase58 = callMethodParameterless("to_base58");
export const byronProtocolMagic = callMethodParameterless(
  "byron_protocol_magic"
);

export function icarusFromKey(bip32pubkey) {
  return byronProtocolMagic => {
    return lib.ByronAddress.icarus_from_key(bip32pubkey, byronProtocolMagic);
  };
}

export function pointerAddressStakePointer(pa) {
  const pointerForeign = pa.stake_pointer();
  return {
    slot: pointerForeign.slot_bignum(),
    txIx: pointerForeign.tx_index_bignum(),
    certIx: pointerForeign.cert_index_bignum()
  };
}

export function _enterpriseAddress(netIdToInt) {
  return inpRec => {
    return lib.EnterpriseAddress.new(
      netIdToInt(inpRec.network),
      inpRec.paymentCred
    );
  };
}

export function _rewardAddress(netIdToInt) {
  return inpRec => {
    return lib.RewardAddress.new(
      netIdToInt(inpRec.network),
      inpRec.paymentCred
    );
  };
}

export function _baseAddress(netIdToInt) {
  return inpRec => {
    return lib.BaseAddress.new(
      netIdToInt(inpRec.network),
      inpRec.paymentCred,
      inpRec.delegationCred
    );
  };
}

export function _pointerAddress(netIdToInt) {
  return inpRec => {
    const p = inpRec.stakePointer;
    const pointerForeign = lib.Pointer.new_pointer(p.slot, p.txIx, p.certIx);
    return lib.PointerAddress.new(
      netIdToInt(inpRec.network),
      inpRec.paymentCred,
      pointerForeign
    );
  };
}
