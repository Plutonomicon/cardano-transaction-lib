<!-- DOCTOC SKIP -->

# Transaction chaining with CTL

Transaction chaining on Cardano is the ability to send transactions that depend on each other in quick succession, without waiting for the previous transactions to be confirmed (not to be confused with transaction batching).

In case the transactions come from multiple actors, some off-chain data delivery mechanism should be used - it's up to the application developers to implement it.

The only piece of data that is actually needed is the additional UTxOs that the CTL query layer is not (yet) aware of. `mustUseAdditionalUtxos` [balancer constraint](./balancing.md) can be used for that, as shown in the [transaction chaining example](../examples/TxChaining.purs). Additionally, `mustNotSpendUtxosWithOutRefs` can be used to forbid spending UTxOs that have been consumed.

