<!-- DOCTOC SKIP -->

# Custom query layers in CTL

## Adding a new backend

CTL can be extended with custom (user) query layers if needed. At this moment it can be done by forking.

Every query CTL uses, except of backend-specific ones, goes through a [query handle](https://github.com/Plutonomicon/cardano-transaction-lib/blob/10a88faa2e6237aafc90568e3488f3421517af63/src/Internal/Contract/QueryHandle/Type.purs#L36).

A new [backend option](https://github.com/Plutonomicon/cardano-transaction-lib/blob/10a88faa2e6237aafc90568e3488f3421517af63/src/Internal/Contract/QueryBackend.purs#L57) should be added, with corresponding [initialization code](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/src/Internal/Contract/QueryHandle.purs)

## Replacing some queries in existing backend

Substituting the query layer for a few functions only (assuming the original backend remains available for initial connection) can be done without forking CTL.

`ContractEnv` contains a QueryHandle (inside a `Reader`), so a [`local`](https://pursuit.purescript.org/packages/purescript-transformers/6.0.0/docs/Control.Monad.Reader.Class#v:local) call with a function that replaces some `QueryHandle` record entries will just work.
