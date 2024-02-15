import * as lib from "@mlabs-haskell/cardano-serialization-lib-gc";
import bip39 from "bip39";

const HARDENED = 0x80000000;

export function _bip32PrivateKeyFromMnemonic(left) {
  return right => phrase => {
    try {
      return right(
        lib.Bip32PrivateKey.from_bip39_entropy(
          Uint8Array.from(
            Buffer.from(bip39.mnemonicToEntropy(phrase.toLowerCase()), "hex")
          ),
          new Uint8Array() // passphrase (not currently implemented)
        )
      );
    } catch (e) {
      return left(e.toString());
    }
  };
}

export function bip32ToPrivateKey(privateKey) {
  return privateKey.to_raw_key();
}

export function derivePrivateKey(path) {
  return hardened => privateKey =>
    privateKey.derive(path | (hardened ? HARDENED : 0));
}
