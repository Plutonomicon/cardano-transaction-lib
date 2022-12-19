# CTL SECP256k1 Support

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

[Cip-49](https://github.com/mlabs-haskell/CIPs/tree/c5bdd66fe49c19c341499f86cebaa2eef9e90b74/CIP-0049) provides two new builtin functions:

Both functions take the following as Parameters:
- A verification key;
- An input to verify (either the message itself, or a hash);
- A signature.

The two functions are:

**1. A verification function for [ECDSA](https://en.bitcoin.it/wiki/Elliptic_Curve_Digital_Signature_Algorithm) signatures.**

[ECDSA usage example](../examples/PlutusV2/ECDSA.purs)\

[ECDSA source code](../src/Contract/Crypto/Secp256k1/ECDSA.purs)

**2. A verification function for [Schnorr](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki) signatures.**

[Schnorr usage example](../examples/PlutusV2/Schnorr.purs)

[Schnorr source code](../src/Contract/Crypto/Secp256k1/Schnorr.purs)
