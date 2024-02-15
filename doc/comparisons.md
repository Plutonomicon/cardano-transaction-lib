<!-- H1 is used to skip the first header in the TOC -->
<h1>CTL vs. other offchain solutions</h1>

This document highlights key differences between CTL and other Cardano offchain frameworks.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Lucid](#lucid)
  - [Wallet support](#wallet-support)
  - [Query layer differences](#query-layer-differences)
  - [Supported backends](#supported-backends)
  - [Staking support](#staking-support)
  - [Testing](#testing)
  - [API design](#api-design)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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

CTL uses [Plutip](./plutip-testing.md), which is a tool to spawn real Cardano testnets on the fly, while Lucid uses an [emulator](https://lucid.spacebudz.io/docs/getting-started/test-emulate/).

Additionally, CTL supports [testing with real wallets](./e2e-testing.md) via headless browsers and provides [an assertion library](./test-utils.md).

### API design

Lucid aims for simplicity, while CTL allows more fine-grained control over transaction building process without losing the benefits of declarativeness.

- CTL uses [`cardano-serialization-lib`](https://github.com/Emurgo/cardano-serialization-lib/), while Lucid uses a fork of [`cardano-multiplatform-lib`](https://github.com/berry-pool/cardano-multiplatform-lib). Lucid allows to use CML's `TxBuilder` or [call CML directly](https://lucid.spacebudz.io/docs/advanced/cml/), while CTL allows to alter the transaction arbitrarily as PureScript data type either before or after balancing. In CTL, CSL types and method wrappers are a part of the internal interface, but technically they can be used as well.
- Plutus Data conversion is handled via a [schema-enabled API](https://lucid.spacebudz.io/docs/advanced/type-casting/) in Lucid. CTL allows for automatic `ToData` / `FromData` deriving for some types, via `HasPlutusSchema`.
