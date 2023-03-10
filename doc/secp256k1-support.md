# CTL Secp256k1 Support

This document is a reference/explainer for the new CTL features introduced with the Chang Hardfork.

**Table of contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Overview](#overview)
- [Usage](#usage)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->
## Overview

See the [What is SECP](https://iohk.io/en/blog/posts/2022/11/03/what-is-secp-and-how-it-drives-cross-chain-development-on-cardano/) for a general overview of what SECP is and how it effects the Cardano blockchain.

For a more in depth oversight please see [Cip-49](https://github.com/mlabs-haskell/CIPs/tree/c5bdd66fe49c19c341499f86cebaa2eef9e90b74/CIP-0049).

## Usage

[Cip-49](https://github.com/mlabs-haskell/CIPs/tree/c5bdd66fe49c19c341499f86cebaa2eef9e90b74/CIP-0049) provides two new Plutus builtin functions for signature verification.

Both functions take the following as Parameters:

- A verification key;
- An input to verify (either the message itself, or a hash);
- A signature.

The two functions are:

**1. A verification function for [ECDSA](https://en.bitcoin.it/wiki/Elliptic_Curve_Digital_Signature_Algorithm) signatures.**

**2. A verification function for [Schnorr](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki) signatures.**

CTL provides off-chain variants of these functions that work the same way (the only difference is that in CTL the arguments are typed, while in Plutus `BuiltinByteString`s are used).

Additionally, CTL exposes functions that allow to work with private keys (derive from bytes, generate) and public keys (derive from a private key), as well as to sign arbitrary data or data hashes.

All SECP256k1-related domain types (public keys, signatures and hashes) are made serialize-able to PlutusData to allow for simpler offchain/onchain interop.

[Public interface for ECDSA support in CTL](../src/Contract/Crypto/Secp256k1/ECDSA.purs)

[ECDSA verification usage example](../examples/ECDSA.purs)

[Public interface for Schnorr support in CTL](../src/Contract/Crypto/Secp256k1/Schnorr.purs)

[Schnorr verification usage example](../examples/Schnorr.purs)

Both examples show how a signature that is constructed off-chain can be passed for on-chain verification.
