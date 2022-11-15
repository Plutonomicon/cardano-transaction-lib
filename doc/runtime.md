# CTL's Runtime Dependencies

In order to run CTL's `Contract` effects, several services are required. These can be configured through a `ContractEnv` that holds websocket connections, information about server hosts/ports, and other requisite information.

**Table of Contents**

- [Current services](#current-services)
- [Using CTL's `runtime` overlay](#using-ctls-runtime-overlay)
- [Changing network configurations](#changing-network-configurations)
- [Other requirements](#other-requirements)
  - [With Nami:](#with-nami)
  - [With Gero:](#with-gero)

### Current services

The services that are currently **required** are:

- [Ogmios](https://ogmios.dev)
  - You **must** use Ogmios v5.2.0 or greater with CTL
  - Ogmios itself requires a running Cardano node, so you may also need to deploy a node. Node v1.34.0 or greater is recommended
  - You can also use [our fork](https://github.com/mlabs-haskell/ogmios) which has improved Nix integration
- [`ogmios-datum-cache`](https://github.com/mlabs-haskell/ogmios-datum-cache)
  - This is required to query for datums, which Ogmios itself does not support
  - This in turn requires a PostgreSQL DB

Optional services:

- [Our Haskell server](../server/README.md)
  - We hope to deprecate this in the future, but we use it at the moment to apply arguments to Plutus scripts, which is hard to implement on front-end.
  - To build the server project, run the following from the repository root: `nix build -L .#ctl-server:exe:ctl-server`

### Using NixOS module

`ctl-server` and its dependencies can be configured and started via NixOS modules. See [../nix/test-nixos-configuration.nix](../nix/test-nixos-configuration.nix) for example usage and [../nix/ctl-server-nixos-module.nix](../nix/ctl-server-nixos-module.nix) for module options.

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

### Other requirements

In order to run most `Contract` actions in the browser, **you must use Nami or Gero wallet**. The following steps must be taken to ensure that you can run CTL contracts:

#### With Nami:

1. Install [Nami extension](https://chrome.google.com/webstore/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo)
   - Due to limitations with Nami itself, only Chromium-based browsers are supported
2. Make sure that you have an active wallet
3. Make sure that your wallet is running on the testnet (can be configured via a toggle in the settings menu)
4. Make sure that you have set collateral for the wallet, which Nami reserves apart from other wallet UTxOs

#### With Gero:

1. Install [GeroWallet Testnet extension](https://chrome.google.com/webstore/detail/gerowallet-testnet/iifeegfcfhlhhnilhfoeihllenamcfgc)
   - Due to limitations with Gero itself, only Chromium-based browsers are supported
2. Make sure that you have an active wallet
3. Make sure that you have set collateral for the wallet, which Gero reserves apart from other wallet UTxOs
