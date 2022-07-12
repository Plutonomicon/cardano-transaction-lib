# cardano-transaction-lib

[![Hercules-ci][herc badge]][herc link]
[![Cachix Cache][cachix badge]][cachix link]

[herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[herc link]: https://hercules-ci.com/github/Plutonomicon/cardano-transaction-lib
[cachix badge]: https://img.shields.io/badge/cachix-public_plutonomicon-blue.svg
[cachix link]: https://public-plutonomicon.cachix.org

**cardano-transaction-lib** (CTL) is a Purescript library for building smart contract transactions on Cardano. It aims to port the functionality and interface of Plutus off-chain code to the browser environment.

**Table of Contents**

- [Roadmap](#roadmap)
  - [Light wallet support](#light-wallet-support)
- [Documentation](#documentation)
- [Architecture](#architecture)
- [Additional resources/tools:](#additional-resourcestools)

## Roadmap

- [x] **Stage 1** Build a simple transaction in the browser that works with at least one light wallet (Nami)
- [x] **Stage 2** Once we can construct a simple user-to-user transaction, we will try to use the library to submit the tx with nami
- [x] **Stage 3** Once we have a simple working transaction, we will seek to build a Plutus smart contract transaction with datum from scratch
- [x] **Stage 4** Once we can construct Plutus smart contract transactions, we will seek to build a library/DSL/interface such that transactions can be built using constraints and lookups - as close as possible to a cut-and-paste solution from Plutus' `Contract` monad code in Haskell (but with no guarantee that code changes are not necessary)
  - [ ] **Stage 4.1** Investigate supporting compatibility with the Vasil hardfork and improvements to our initial `Contract` API (**In progress**)
- [ ] **Stage 5** Once we have a basic `Contract`-style API, we will further refine its public interface, expand wallet support (see [below](#light-wallet-support)), expose a test interface (see [here](doc/plutip-testing.md)), provide a more ergonomic JS/TS API, support stake validators, and support CIP workflows on the public testnet
- [ ] **Stage 6** Once CTL's `Contract` interface has been stabilized, we will add support for even more wallets and attempt to deprecate CTL's currently required Haskell server

### Light wallet support

Support is planned for the following light wallets:

- [x] [Nami](https://namiwallet.io/)
- [x] [Gero](https://gerowallet.io/)
- [ ] [Flint](https://flint-wallet.com/)
- [ ] [Lace](https://www.lace.io/)
- [ ] [Typhon](https://typhonwallet.io/)
- [ ] [Yoroi](https://yoroi-wallet.com/)
- [ ] [Eternl (formerly CCvault)](https://eternl.io/)

## Documentation

Please explore our documentation to discover how to use CTL, how to set up its runtime, and how it compares to Plutus/PAB:

- [Migrating from Plutus to CTL](./doc/plutus-comparison.md)
- [Adding CTL as a dependency](./doc/ctl-as-dependency.md)
- [Getting started writing CTL contracts](./doc/getting-started.md)
- [CTL's runtime dependencies](./doc/runtime.md)
- [Developing on CTL](./doc/development.md)
- [Testing contracts with Plutip](./doc/plutip-testing.md)

You can also [generate Purescript documentation for CTL and its dependencies](./doc/development.md#generating-ps-documentation).

## Architecture

CTL is directly inspired by the Plutus Application Backend (PAB). Unlike PAB, however, CTL is a library and not a standalone process. Over the course of CTL's development, several questions have been raised as to how best create PAB-as-a-library:

1. How do we get the transaction in the right format?
   - This is handled by `cardano-serialization-lib`, a Rust library available as WASM
2. How do we query the chain?
   - This has been solved using Ogmios
   - We may, however, support a BlockFrost backend as well in the future
3. How do we query for datums (i.e. the datums themselves and not just their hashes)?
   - `ogmios-datum-cache` solves this problem
4. How do we submit the transaction?
   - This is done via browser-based light wallet integration in the browser based on CIP-30
5. How closely should we follow Plutus' `Contract` API?
   - CTL's `Contract` model is **significantly** less restrictive than Plutus' and allows for arbitrary effects within the `Contract` monad
   - Certain features cannot be directly translated into Purescript from Haskell due to differences between the two languages (e.g. CTL's `DatumType` and `RedeemerType` are type class with fundeps, as Purescript lacks any notion of type families/type-level functions)
6. A lingering concern remains around storage solutions, if needed
   - This can be in memory, in various browser storage solutions, or a decentralized DB like Fluree

## Additional resources/tools:

- [`cardano-serialization-lib`](https://github.com/Emurgo/cardano-serialization-lib/)
- [Ogmios](https://ogmios.dev) for chain queries
- [CIP-30](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030) (wallet interface - Nami partially implements this)
- [Nami docs](https://github.com/Berry-Pool/nami-wallet)
- [Alonzo CDDL spec](https://github.com/input-output-hk/cardano-ledger/blob/0738804155245062f05e2f355fadd1d16f04cd56/alonzo/impl/cddl-files/alonzo.cddl)

## Available support channels info

You can find help, more information and ongoing discusion about the project here:
- Plutonomicon Discord: https://discord.gg/c8kZWxzJ
- #ctl channel at MLabs' Slack
