/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}
lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);
const bip39 = require("bip39");

const HARDENED = 0x80000000;

exports._bip32PrivateKeyFromMnemonic = left => right => phrase => {
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

exports.bip32ToPrivateKey = privateKey => privateKey.to_raw_key();

exports.derivePrivateKey = path => hardened => privateKey =>
  privateKey.derive(path | (hardened ? HARDENED : 0));
