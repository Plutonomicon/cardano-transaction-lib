# cardano-transaction-lib

[![Hercules-ci][herc badge]][herc link]
[![Cachix Cache][cachix badge]][cachix link]
[![PureScript code documentation][docs badge]][docs link]

[herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[herc link]: https://hercules-ci.com/github/Plutonomicon/cardano-transaction-lib
[cachix badge]: https://img.shields.io/badge/cachix-public_plutonomicon-blue.svg
[cachix link]: https://public-plutonomicon.cachix.org
[docs badge]: https://img.shields.io/badge/docs-PureScript%20code%20documentation-%2377F
[docs link]: https://plutonomicon.github.io/cardano-transaction-lib/

**cardano-transaction-lib** (CTL) is a Purescript library for building smart contract transactions on Cardano. It aims to port the functionality and interface of Plutus off-chain code to the browser environment and NodeJS.

**Table of Contents**

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Documentation](#documentation)
  - [Light wallet support](#light-wallet-support)
- [Roadmap](#roadmap)
- [Architecture](#architecture)
- [Additional resources/tools:](#additional-resourcestools)
- [Available support channels info](#available-support-channels-info)
- [Funding acknowledgements](#funding-acknowledgements)
- [Use in production](#use-in-production)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Documentation

Please explore our documentation to discover how to use CTL, how to set up its runtime, and how it compares to Plutus/PAB:

- [Super quick start](./doc/getting-started.md#setting-up-a-new-project)
- [Adding CTL as a dependency](./doc/ctl-as-dependency.md)
- [CTL's runtime dependencies](./doc/runtime.md)
- [Blockfrost support](./doc/blockfrost.md)
- [Getting started writing CTL contracts](./doc/getting-started.md)
- [Managing contract environment](./doc/contract-environment.md)
- [Using CTL from JS](./doc/using-from-js.md)
- [Importing Plutus Scripts](./doc/importing-scripts.md)
- [Migrating from Plutus to CTL](./doc/plutus-comparison.md)
- [Overview of testing approaches](./doc/testing.md)
  - [Testing contracts with Plutip](./doc/plutip-testing.md)
  - [End-to-end testing with headless browsers](./doc/e2e-testing.md)
  - [Utilities for testing](./doc/test-utils.md)
- [CIP-25 NFT standard support](./doc/cip-25-nfts.md)
- [Transaction balancing](./doc/balancing.md)
- [Transaction chaining](./doc/tx-chaining.md)
- [Ada staking support](./doc/staking.md)
- [Key management](./doc/key-management.md)
- [SECP256k1 support (CIP-49)](./doc/secp256k1-support.md)
- [Custom query layers](./doc/custom-query-layers.md)
- [FAQs](./doc/faq.md)
- [Feature overview video](./doc/video-intro.md)
- [Comparison with other frameworks (Lucid)](./doc/comparisons.md)
- [Development workflows for CTL](./doc/development.md)

You can also access [PureScript documentation for CTL and its dependencies](https://plutonomicon.github.io/cardano-transaction-lib/) for the most recent `develop` version, or [generate it yourself](./doc/development.md#generating-ps-documentation).

### Light wallet support

Support is planned for the following light wallets:

- [x] [Nami](https://namiwallet.io/)
- [x] [Gero](https://gerowallet.io/)
- [x] [Flint](https://flint-wallet.com/)
- [x] [Lode](https://lodewallet.io/)
- [x] [Eternl (formerly CCvault)](https://eternl.io/)
- [x] [NuFi](https://nu.fi/)
- [x] [Lace](https://www.lace.io/)
- [ ] [Typhon](https://typhonwallet.io/)
- [ ] [Yoroi](https://yoroi-wallet.com/)

## Roadmap

- [x] **Stage 1** Build a simple transaction in the browser that works with at least one light wallet (Nami)
- [x] **Stage 2** Once we can construct a simple user-to-user transaction, we will try to use the library to submit the tx with nami
- [x] **Stage 3** Once we have a simple working transaction, we will seek to build a Plutus smart contract transaction with datum from scratch
- [x] **Stage 4** Once we can construct Plutus smart contract transactions, we will seek to build a library/DSL/interface such that transactions can be built using constraints and lookups - as close as possible to a cut-and-paste solution from Plutus' `Contract` monad code in Haskell (but with no guarantee that code changes are not necessary)
  - [x] **Stage 4.1** Investigate supporting compatibility with the Vasil hardfork and improvements to our initial `Contract` API
- [x] **Stage 5** Once we have a basic `Contract`-style API, we will further refine its public interface, expand wallet support (see [below](#light-wallet-support)), expose a test interface (**DONE** - see [here](doc/plutip-testing.md)), provide a more ergonomic JS/TS API, support stake validators (**DONE**), and support CIP workflows on the public testnet (**In progress**)
- [x] **Stage 6** Once CTL's `Contract` interface has been stabilized, we will add support for even more wallets and attempt to deprecate CTL's currently required Haskell server (**DONE**)

## Architecture

CTL is directly inspired by the Plutus Application Backend (PAB). Unlike PAB, however, CTL is a library and not a standalone process. Over the course of CTL's development, several questions have been raised as to how best create PAB-as-a-library:

1. How do we get the transaction in the right format?
   - This is handled by `cardano-serialization-lib`, a Rust library available as WASM
2. How do we query the chain?
   - This has been solved using Ogmios & Kupo
   - Thanks to [Catalyst](https://cardano.ideascale.com/c/idea/420791), we now support an alternative [BlockFrost](https://blockfrost.io/) backend as well
3. How do we get wallet data?
   - This is done via browser-based light wallet integration in the browser based on CIP-30
4. How closely should we follow Plutus' `Contract` API?
   - CTL's `Contract` model is **significantly** less restrictive than Plutus' and allows for arbitrary effects within the `Contract` monad
   - Certain features cannot be directly translated into Purescript from Haskell due to differences between the two languages
   - Some of the Plutus conventions do not make sense for us, due to differences between on-chain and off-chain

## Additional resources/tools:

- [`cardano-serialization-lib`](https://github.com/Emurgo/cardano-serialization-lib/)
- [Ogmios](https://ogmios.dev) for chain queries
- [Kupo](https://cardanosolutions.github.io/kupo/) for chain queries
- [CIP-30](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030) (wallet interface - Nami partially implements this)
- [Nami docs](https://github.com/Berry-Pool/nami-wallet)
- [Alonzo CDDL spec](https://github.com/input-output-hk/cardano-ledger/blob/0738804155245062f05e2f355fadd1d16f04cd56/alonzo/impl/cddl-files/alonzo.cddl)

## Available support channels info

You can find help, more information and ongoing discusion about the project here:

- [Plutonomicon Discord](https://discord.gg/JhbexnV9Pc)
- #ctl channel at MLabs' Slack

## Funding acknowledgements

CTL is being developed by MLabs. The following companies/funds have contributed significant resources to development:

- [IOHK](https://iohk.io/en/about/)
- [Catalyst Fund8](https://cardano.ideascale.com/c/idea/396607)
- [Catalyst Fund9](https://cardano.ideascale.com/c/idea/420791)
- [Catalyst Fund10](https://cardano.ideascale.com/c/idea/101478)
- [MLabs](https://mlabs.city/)
- [Indigo Protocol](https://indigoprotocol.io/)
- [Equine](https://www.equine.gg/)
- [Liqwid Labs](https://liqwid.finance/)
- [PlayerMint](https://www.playermint.com/)
- Ardana

## Use in production

- [Indigo Protocol](https://indigoprotocol.io/)
- [Liqwid](https://liqwid.finance/)
- [Clarity](https://clarity.community/)
- [PlayerMint](https://www.playermint.com/)
- [SingularityNet](https://singularitynet.io/)
