/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = await import("@emurgo/cardano-serialization-lib-nodejs");
}
// import gcWrapper from "@mlabs-haskell/csl-gc-wrapper";
// lib = gcWrapper(lib);

export function hashToBytes(hash) {
  return hash.to_bytes();
}

export function hashFromBytes(name) {
  return maybe => bytes => {
    return hashFromImpl(lib[name].from_bytes)(maybe)(bytes);
  };
}

export function hashToBech32Unsafe(prefix) {
  return hash => {
    return hash.to_bech32(prefix);
  };
}

const hashFromImpl = hashClassFrom => maybe => input => {
  let ret = null;
  try {
    ret = hashClassFrom(input);
  } catch (e) {
    // Do nothing
  }
  if (ret == null) {
    return maybe.nothing;
  }
  return maybe.just(ret);
};

export function hashToBech32Impl(maybe) {
  return prefix => hash => {
    let ret = null;
    try {
      ret = hash.to_bech32(prefix);
    } catch (e) {
      // Do nothing
    }
    if (ret == null) {
      return maybe.nothing;
    }
    return maybe.just(ret);
  };
}

export function _ed25519KeyHashFromBech32Impl(maybe) {
  return bech32str => {
    return hashFromImpl(lib.Ed25519KeyHash.from_bech32)(maybe)(bech32str);
  };
}

export function _scriptHashFromBech32Impl(maybe) {
  return bech32str => {
    return hashFromImpl(lib.ScriptHash.from_bech32)(maybe)(bech32str);
  };
}

export function nativeScriptHash(script) {
  return script.hash();
}
