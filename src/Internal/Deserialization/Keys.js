/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = await import("@emurgo/cardano-serialization-lib-nodejs");
}
// import gcWrapper from "@mlabs-haskell/csl-gc-wrapper";
// lib = gcWrapper(lib);

export function freshPrivateKey() {
  return lib.PrivateKey.generate_ed25519();
}

export function _publicKeyFromBech32(maybe) {
  return bech32 => {
    try {
      return maybe.just(lib.PublicKey.from_bech32(bech32));
    } catch (_) {
      return maybe.nothing;
    }
  };
}

export function _ed25519SignatureFromBech32(maybe) {
  return bech32 => {
    try {
      return maybe.just(lib.Ed25519Signature.from_bech32(bech32));
    } catch (_) {
      return maybe.nothing;
    }
  };
}

export function _privateKeyFromBytes(maybe) {
  return bytes => {
    try {
      return maybe.just(lib.PrivateKey.from_normal_bytes(bytes));
    } catch (_) {
      return maybe.nothing;
    }
  };
}

export function privateKeyToBech32(privateKey) {
  return privateKey.to_bech32();
}

export function _privateKeyFromBech32(maybe) {
  return bech32 => {
    try {
      return maybe.just(lib.PrivateKey.from_bech32(bech32));
    } catch (_) {
      return maybe.nothing;
    }
  };
}
