<!-- H1 is used to skip the first header in the TOC -->
<h1>CTL vs. other offchain solutions</h1>

This document highlights key differences between CTL and other Cardano offchain frameworks.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Plutus Application Backend](#plutus-application-backend)
- [Lucid](#lucid)
  - [Wallet support](#wallet-support)
  - [Query layer differences](#query-layer-differences)
  - [Supported backends](#supported-backends)
  - [Staking support](#staking-support)
  - [Testing](#testing)
  - [API design](#api-design)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Plutus Application Backend

CTL is directly inspired by the Plutus Application Backend (PAB). Unlike PAB, however, CTL is a library and not a standalone process. Over the course of CTL's development, several questions have been raised as to how best create PAB-as-a-library:

1. How do we get the transaction in the right format?
   - This is handled by `cardano-serialization-lib`, a Rust library available as WASM
2. How do we query the chain?
   - This has been solved using Ogmios & Kupo
   - Thanks to [Catalyst](https://cardano.ideascale.com/c/idea/420791), we now support an alternative [BlockFrost](https://blockfrost.io/) backend as well ([docs](./blockfrost.md))
3. How do we get wallet data?
   - This is done via browser-based light wallet integration in the browser based on CIP-30 ([`purescript-cip30-typesafe`](https://github.com/mlabs-haskell/purescript-cip30-typesafe))
4. How closely should we follow Plutus' `Contract` API?
   - CTL's `Contract` model is **significantly** less restrictive than Plutus' and allows for arbitrary effects within the `Contract` monad
   - Certain features cannot be directly translated into Purescript from Haskell due to differences between the two languages
   - Some of the Plutus conventions do not make sense for us, due to differences between on-chain and off-chain
   - Our API scope is a lot more extensive, as we provide support for wallet interactions, staking operations, and more support for balancer tweaking.

For a more in-depth explanation of the differences between PAB and CTL (with code examples), refer to [`plutus-comparison.md`](./plutus-comparison.md).

## Lucid

### Wallet support

- Both CTL and Lucid support using [seed phrases and private keys](./key-management.md).
- Lucid allows to use any address without a private key for querying - CTL does not allow that, but it's still possible to build transactions for other wallets to sign via [other means](./balancing.md).

### Query layer differences

Lucid uses a [`Provider` class](https://deno.land/x/lucid@0.10.5/mod.ts?s=Provider) that defines all available queries. CTL query methods are defined in [`QueryHandle`](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/src/Internal/Contract/QueryHandle/Type.purs#L36).

CTL supports the following queries that Lucid does not:

- `getScriptByHash`
- `getTxMetadata`
- `getChainTip`
- `getCurrentEpoch`
- `evaluateTx` (not needed for Lucid)
- `getEraSummaries`
- `getPoolIds`

Lucid, on the other hand, provides a way to get a UTxO that contains a specified NFT (`getUtxoByUnit`).

### Supported backends

- Both CTL and Lucid support [Blockfrost](./blockfrost.md) and [Kupo+Ogmios](./runtime.md)
- Lucid also supports [Maestro](https://www.gomaestro.org/)
- Both CTL and Lucid allow for custom backends - Lucid via `Provider` interface implementation, and CTL via a custom `QueryHandle`.

### Staking support

Both [CTL](./staking.md) and [Lucid](https://lucid.spacebudz.io/docs/getting-started/delegate/) support all operations with ADA delegations.

### Testing

CTL uses [Cardano Testnet](./cardano-testnet-testing.md), while Lucid uses an [emulator](https://lucid.spacebudz.io/docs/getting-started/test-emulate/).

Additionally, CTL supports [testing with real wallets](./e2e-testing.md) via headless browsers and provides [an assertion library](./test-utils.md).

### API design

Lucid aims for simplicity, while CTL allows more fine-grained control over transaction building process without losing the benefits of declarativeness.

- CTL uses [`cardano-serialization-lib`](https://github.com/Emurgo/cardano-serialization-lib/), while Lucid uses a fork of [`cardano-multiplatform-lib`](https://github.com/berry-pool/cardano-multiplatform-lib). Lucid allows to use CML's `TxBuilder` or [call CML directly](https://lucid.spacebudz.io/docs/advanced/cml/), while CTL allows to alter the transaction arbitrarily as PureScript data type either before or after balancing. In CTL, CSL types and method wrappers are a part of the internal interface, but technically they can be used as well.
- Plutus Data conversion is handled via a [schema-enabled API](https://lucid.spacebudz.io/docs/advanced/type-casting/) in Lucid. CTL allows for automatic `ToData` / `FromData` deriving for some types, via `HasPlutusSchema`.
