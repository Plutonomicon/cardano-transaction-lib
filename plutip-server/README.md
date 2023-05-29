# plutip-server

Plutip-server is a simple HTTP interface for [Plutip](https://github.com/mlabs-haskell/plutip) to start and stop local plutip clusters on demand.

CTL handles communication with Plutip via this server and there's usually no need to use it directly, though `plutip-server` can be useful if you need to control Plutip from a different service (although [`plutip-local-cluster`](https://github.com/mlabs-haskell/plutip/tree/master/local-cluster), which is a CLI program, is probably more convenient for that).

Server exposes two POST endpoints for starting and stopping clusters (only up to one active cluster is allowed at a time).

You can configure cluster parameters like slot length, max tx size, etc. and specify how many wallets to create, including Ada UTxO distribution in each wallet (see [here](../doc/plutip-testing.md#cluster-configuration-options) for how to configure this via CTL).

On a successful cluster startup `plutip-server` responds with a list of keys of the newly created wallets, node configuration, directory with public and private keys for the wallets and a path to the socket of one of the nodes from the cluster (usually it's the one that finished startup the first).

`plutip-server` uses Plutip as a [Haskell library](https://github.com/mlabs-haskell/plutip/tree/master#as-a-library) in Servant API handlers via `startFundedCluster` and `stopCluster` functions.
