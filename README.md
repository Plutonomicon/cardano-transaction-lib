# cardano-transaction-lib [![cardano-purescript](https://img.shields.io/badge/cardano--purescript?logo=cardano&logoColor=white&label=cardano-purescript&labelColor=blue&color=blue)](https://github.com/klntsky/cardano-purescript)


[![Hercules-ci][herc badge]][herc link]
[![Cachix Cache][cachix badge]][cachix link]
[![PureScript code documentation][docs badge]][docs link]

[herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[herc link]: https://hercules-ci.com/github/Plutonomicon/cardano-transaction-lib
[cachix badge]: https://img.shields.io/badge/cachix-public_plutonomicon-blue.svg
[cachix link]: https://public-plutonomicon.cachix.org
[docs badge]: https://img.shields.io/badge/docs-PureScript%20code%20documentation-%2377F
[docs link]: https://plutonomicon.github.io/cardano-transaction-lib/

**cardano-transaction-lib** (CTL) is a Purescript framework for building smart contract transactions on Cardano. It belongs to the the same category of tools as Lucid, Mesh.js, Atlas, Plutus Application Backend (PAB).

**Table of Contents**

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Documentation](#documentation)
- [Additional resources/tools:](#additional-resourcestools)
- [Available support channels info](#available-support-channels-info)
- [Funding acknowledgements](#funding-acknowledgements)
- [Use in production](#use-in-production)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Documentation

Please explore our documentation to discover how to use CTL, how to set up its runtime, and how it compares to other tools:

- [Super quick start](./doc/getting-started.md#setting-up-a-new-project)
- [Adding CTL as a dependency](./doc/ctl-as-dependency.md)
- [CTL's runtime dependencies](./doc/runtime.md)
- [Blockfrost support](./doc/blockfrost.md)
- [Getting started writing CTL contracts](./doc/getting-started.md)
- [Managing contract environment](./doc/contract-environment.md)
- [Using CTL from JS](./doc/using-from-js.md)
- [Importing Plutus Scripts](./doc/importing-scripts.md)
- [Migrating from Plutus Application Backend to CTL](./doc/plutus-comparison.md)
- [Overview of testing approaches](./doc/testing.md)
  - [Testing on local Cardano testnets](./doc/cardano-testnet-testing.md)
  - [End-to-end dApp testing with headless browsers](./doc/e2e-testing.md)
  - [Assertion utilities for testing](./doc/test-utils.md)
  - [Using a directory with private keys to run tests](./doc/keydir.md)
- [Transaction balancing](./doc/balancing.md)
- [Transaction chaining](./doc/tx-chaining.md)
- [Ada staking support](./doc/staking.md)
- [Key management](./doc/key-management.md)
- [SECP256k1 support (CIP-49)](./doc/secp256k1-support.md)
- [Custom query layers](./doc/custom-query-layers.md)
- [Going to production with CTL](./doc/production.md)
- [FAQs](./doc/faq.md)
- [Feature overview video](./doc/video-intro.md)
- [Comparison with other frameworks (Lucid)](./doc/comparisons.md)
- [Development workflows for CTL](./doc/development.md)

You can also access [PureScript documentation for CTL and its dependencies](https://plutonomicon.github.io/cardano-transaction-lib/) for the most recent `develop` version, or [generate it yourself](./doc/development.md#generating-ps-documentation).

## Additional resources/tools:

- [`cardano-serialization-lib`](https://github.com/Emurgo/cardano-serialization-lib/)
- [Ogmios](https://ogmios.dev) for chain queries
- [Kupo](https://cardanosolutions.github.io/kupo/) for chain queries
- [CIP-30](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030) (wallet interface)
- [Alonzo CDDL spec](https://github.com/input-output-hk/cardano-ledger/blob/0738804155245062f05e2f355fadd1d16f04cd56/alonzo/impl/cddl-files/alonzo.cddl)

## Available support channels info

You can find help, more information and ongoing discusion about the project here:

- [Plutonomicon Discord](https://discord.gg/JhbexnV9Pc)
- #ctl channel at MLabs' Slack

## Funding acknowledgements

CTL is being developed by MLabs. The following companies/funds have contributed significant resources (development time or funding):

- [IOHK](https://iohk.io/en/about/)
- [Catalyst Fund8](https://cardano.ideascale.com/c/idea/396607)
- [Catalyst Fund9](https://cardano.ideascale.com/c/idea/420791)
- [Catalyst Fund10](https://cardano.ideascale.com/c/idea/101478)
- [Intersect MBO](https://docs.intersectmbo.org/intersect-community-grants/grant-projects)
- [MLabs](https://mlabs.city/)
- [Indigo Protocol](https://indigoprotocol.io/)
- [Equine](https://www.equine.gg/)
- [Liqwid Labs](https://liqwid.finance/)
- PlayerMint
- [Fourier Labs](https://fourierlabs.io/)
- Ardana

## Use in production

- [Indigo Protocol](https://indigoprotocol.io/)
- [Liqwid](https://liqwid.finance/)
- [Clarity](https://clarity.community/)
- PlayerMint
- [SingularityNet](https://singularitynet.io/)
