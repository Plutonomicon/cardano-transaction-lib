/* global BROWSER_RUNTIME */

const bytesFromKey = key => key.as_bytes();

export { bytesFromKey as bytesFromPublicKey };
export { bytesFromKey as bytesFromPrivateKey };

export function publicKeyFromPrivateKey(private_key) {
  return private_key.to_public();
}

const bech32FromX = key => key.to_bech32();

export { bech32FromX as bech32FromPublicKey };
export { bech32FromX as bech32FromPrivateKey };
export { bech32FromX as bech32FromEd25519Signature };
