<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Custom query layers in CTL](#custom-query-layers-in-ctl)
  - [Adding a new backend](#adding-a-new-backend)
  - [Replacing some queries in existing backend](#replacing-some-queries-in-existing-backend)
- [Configuring CTL's default query layer (Kupo/Ogmios)](#configuring-ctls-default-query-layer-kupoogmios)
  - [Kupo](#kupo)
    - [Faster sync for Kupo](#faster-sync-for-kupo)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Custom query layers in CTL

## Adding a new backend

CTL can be extended with custom (user) query layers if needed. At this moment it can be done by forking.

Every query CTL uses, except of backend-specific ones, goes through a [query handle](https://github.com/Plutonomicon/cardano-transaction-lib/blob/10a88faa2e6237aafc90568e3488f3421517af63/src/Internal/Contract/QueryHandle/Type.purs#L36).

A new [backend option](https://github.com/Plutonomicon/cardano-transaction-lib/blob/10a88faa2e6237aafc90568e3488f3421517af63/src/Internal/Contract/QueryBackend.purs#L57) should be added, with corresponding [initialization code](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/src/Internal/Contract/QueryHandle.purs)

## Replacing some queries in existing backend

Substituting the query layer for a few functions only (assuming the original backend remains available for initial connection) can be done without forking CTL.

`ContractEnv` contains a QueryHandle (inside a `Reader`), so a [`local`](https://pursuit.purescript.org/packages/purescript-transformers/6.0.0/docs/Control.Monad.Reader.Class#v:local) call with a function that replaces some `QueryHandle` record entries will just work.

# Configuring CTL's default query layer (Kupo/Ogmios)

Optimising storage requirements and performance of the query layer, unfortunately, depends on your app query needs.

## Kupo

Kupo is used for "utxos-at-address" query in CTL.

This query is mainly used in two contexts:

- manual `utxosAt` calls
- wallet <-> backend synchronization (see [here](./query-layers.md))

If you don't need either, consider removing Kupo from the runtime and [disabling wallet <-> backend synchronization](./query-layers.md) (**IMPORTANT!**).

If your app works in a way that only requires UTxO lookups on certain addresses (e.g. script addresses), consider using [`--match-pattern`](https://cardanosolutions.github.io/kupo/#section/Getting-started/-match-pattern) to drastically reduce DB size. The downside would be that wallet<->backend sync will have to be disabled, because it requires looking up wallet's addresses via the backend.

### Faster sync for Kupo

Use [`--since` flag](https://cardanosolutions.github.io/kupo/#section/Getting-started/-since-slot-no.header_hash) to perform partial incomplete sync. Find block parameters on [a block explorer](https://cexplorer.io/block). It will let you sync things faster (than in ~4 days for mainnet), for the price of Kupo not seeing UTxOs created after the specified slot. If you disable wallet<->backend sync, and depending on your use case, that might be acceptable for production.
