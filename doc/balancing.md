<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Configuring the balancing process](#configuring-the-balancing-process)
  - [Balancer constraints](#balancer-constraints)
  - [Concurrent spending](#concurrent-spending)
  - [Balancing a Tx for other wallet](#balancing-a-tx-for-other-wallet)
  - [Synchronization](#synchronization)
  - [Balancing process limitations](#balancing-process-limitations)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Configuring the balancing process

Transaction balancing in Cardano is the process of finding a set of inputs and outputs that that sum up to zero, covering all the required fees for the transaction to be valid.

## Balancer constraints

CTL allows tweaking the default balancer behavior by letting the user impose constraints on the UTxO set that is used in the process (`balanceTxWithConstraints`):

- Using arbitrary address as user's own (for transaction balancing): `mustUseUtxosAtAddresses` / `mustUseUtxosAtAddress`
- Providing additional UTxOs to use: `mustUseAdditionalUtxos`
- Bypassing wallet's collateral selection and selecting collateral UTxOs from a given set: `mustUseCollateralUtxos`
- Overriding change address: `mustSendChangeToAddress`
- Preventing certain UTxOs from being spent: `mustNotSpendUtxosWithOutRefs` / `mustNotSpendUtxoWithOutRef`
- Distributing token outputs equally between change UTxOs: `mustGenChangeOutsWithMaxTokenQuantity`

## Concurrent spending

Attempting to spend UTxOs concurrently leads to some of the transactions being rejected. To ensure that no concurrent spending is happening, CTL uses it's own UTxO locking machinery. `balanceTxs` or `balanceTxsWithConstraints` can be used to construct multiple transactions at once, ensuring that the sets of inputs do not intersect.

Obviously, the number of available UTxOs must be greater than the number of transactions. CTL will throw an exception if it's not the case.

## Balancing a Tx for other wallet

Setting `mustUseUtxosAtAddress`, `mustSendChangeToAddress` and `mustUseCollateralUtxos` at the same time allows to build a transaction without any connection to the current wallet. For example, it's possible to balance it on server-side and send to the user to sign, or balance a Tx on one user's side while leaving fees at the expense of some other user.

## Synchronization

It's possible to make CTL try to synchronize the wallet state with the query layer, i.e. wait until all UTxOs that the wallet returns are visible in the query layer. Thus the situation when the query layer refuses to validate a Tx (either during ex-units evaluation or on Tx submission) is only possible due to a rollback or a synchronization timeout. Please see [our docs for query layer synchronization](./query-layers.md).

## Balancing process limitations

It may be surprising at first, but balancing a transaction on Cardano is generally undecidable.

This is because transaction fees depend on execution unit budgets of the validator scripts, and the execution paths of the scripts depend on the set of inputs, that the balancer attempts to provide to, in turn, cover the fees. It is a recursive process that continues until the cycle of adding new inputs, evaluating the fees and generating change outputs converges to some configuration of inputs and outputs, where sum of all inputs and outputs minus fees is zero.

It is possible to intentionally craft a script with execution fees depending on the number of inputs in a non-trivial way, causing the balancer to enter a potentially infinite loop.

Another problem is that the intermediate evaluations of the script that happen during balancer may fail. The balancer provides a guarantee that all script executions will only run on "pre-balanced" transactions (in our terminology). That means that CTL supports the scripts that rely on the transaction contexts they validate to be balanced.
