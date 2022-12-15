# CTL's Runtime Dependencies

In order to run CTL's `Contract` effects, several services are required. These can be configured through a `ContractEnv` that holds websocket connections, information about server hosts/ports, and other requisite information.

**Table of Contents**

- [Current services](#current-services)
- [Using NixOS module](#using-nixos-module)
- [Using CTL's `runtime` overlay](#using-ctl-s--runtime--overlay)
- [Changing network configurations](#changing-network-configurations)
- [Wallet requirements](#wallet-requirements)

### Current services

The services that are currently **required** are:

- [Ogmios](https://ogmios.dev)
  - You **must** use Ogmios v5.2.0 or greater with CTL
  - Ogmios itself requires a running Cardano node, so you may also need to deploy a node. Node v1.34.0 or greater is recommended
  - You can also use [our fork](https://github.com/mlabs-haskell/ogmios) which has improved Nix integration
- [Kupo](https://github.com/CardanoSolutions/kupo)
  - Required to query UTxOs and resolve inline datums and reference scripts
  - You **must** use Kupo v2.2.0 or greater with CTL
  - Like Ogmios, Kupo requires a running Cardano node
- [`ogmios-datum-cache`](https://github.com/mlabs-haskell/ogmios-datum-cache)
  - This is required to query for datums, which Ogmios itself does not support
  - This in turn requires a PostgreSQL DB

### Using NixOS module

See [../nix/test-nixos-configuration.nix](../nix/test-nixos-configuration.nix) for example usage and TODO for module options.

### Using CTL's `runtime` overlay

CTL's `overlays.runtime` (contained in [runtime.nix](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/nix/runtime.nix)) provides some mechanisms for conveniently launching all runtime services using [Arion](https://docs.hercules-ci.com/arion) (itself a wrapper around `docker-compose`). To use this, you must have docker installed, the docker deamon running and you must have a setup based on Nix flakes (recommended as well for [using CTL as a dependency for Purescript projects](./ctl-as-dependency.md)).

The project template [flake](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/templates/ctl-scaffold/flake.nix) uses the `runtime` overlay to launch all of the required services.

For launching services for developing CTL itself, see our documentation on [development](./development.md#launching-services-for-development).

### Changing network configurations

CTL supports using networks other than the public preview testnet to provide different development environments.

To choose a different network, you must specify an alternative way of getting the network configuration. The [cardano-configurations](https://github.com/input-output-hk/cardano-configurations) repo provides configs for `mainnet`, `preprod` and a few other networks. The `network.name` parameter of `buildCtlRuntime` determines which of the network config directories is chosen.

You can also specify your own fork of `cardano-configurations` like this:

```
inputs.cardano-transaction-lib.inputs.cardano-configurations.follows = "...";
```

When changing networks, make sure that `network.magic` is correctly synchronized with value in config (see `protocolConsts.protocolMagic` in `byron.json`).

### Wallet requirements

In order to run most `Contract` actions in the browser, **you must use one of the supported wallets**. The following steps must be taken to ensure that you can run CTL contracts:

1. Make sure that your wallet is running on the testnet (some wallets can be configured via a toggle in the settings menu, other provide a separate extension)
2. Fund the wallet using the [Testnet Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet/)
3. Make sure that you have set collateral for the wallet, which wallets reserve apart from other UTxOs
