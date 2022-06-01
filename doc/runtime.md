# CTL's Runtime Dependencies

In order to run CTL's `Contract` effects, several services are required. These can be configured through a `ContractConfig` that holds websocket connections, information about server hosts/ports, and other requisite information.

**Table of Contents**

- [Current services](#current-services)
- [Using the CTL overlay](#using-the-ctl-overlay)
- [Changing network configurations](#changing-network-configurations)
- [Other requirements](#other-requirements)

### Current services

The services that are currently required are:

- [Ogmios](https://ogmios.dev)
  - You **must** use Ogmios v5.2.0 or greater with CTL
  - Ogmios itself requires a running Cardano node, so you may also need to deploy a node. Node v1.34.0 or greater is recommended
  - You can also use [our fork](https://github.com/mlabs-haskell/ogmios) which has improved Nix integration
- [`ogmios-datum-cache`](https://github.com/mlabs-haskell/ogmios-datum-cache)
  - This is required to query for datums, which Ogmios itself does not support
  - This in turn requires a PostgreSQL DB
- [Our Haskell server](/server/README.md)
  - We hope to deprecate this in the future, but we use it at the moment for certain Cardano libraries that have no Purescript analogue
  - To build the server project, run the following from the repository root: `nix build -L .#ctl-server:exe:ctl-server`

### Using the CTL overlay

CTL's overlay (contained in its flake `outputs`) provides some mechanisms for conveniently launching all runtime services using [Arion](https://docs.hercules-ci.com/arion)(itself a wrapper around `docker-compose`). To use this, you must have a setup based on Nix flakes (recommended as well for [using CTL as a dependency for Purescript projects](#using-ctl-as-a-dependency)).

Here is an example that uses the overlay to launch runtime services:

```nix
{

  inputs = {
    # You should probably pin this to a specific revision, especially if using
    # it for Purescript projects
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib";

    # To use the same version of `nixpkgs` as we do
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
  };

  outputs = { self, cardano-transaction-lib, nixpkgs, ... }:
    # some boilerplate
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      # generate `pkgs` with the CTL overlay applied. This gives you access to
      # various additional packages, using the same versions of CTL, including:
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ cardano-transaction-lib.overlay.${system} ];
      };

      # The configuration for the CTL runtime, which will be passed to the
      # expression that builds the JSON file used by Arion. This value can be
      # shared between `buildCtlRuntime` and `launchCtlRuntime`, as shown below
      #
      # You can refer to the final configuration value by passing a function
      # that takes a single arugment. Alternatively, you can pass an attrset
      # directly
      runtimeConfig = final: with final; {
        network = {
          name = "testnet";
          magic = 1097911063;
        };
        # *All* of these values are optional, and shown with their default
        # values. If you need even more customization, you can use `overideAttrs`
        # to change the values after calling `buildCtlRuntime` (e.g. a secrets
        # volume for the `postgres` service)
        node = { port = 3001; };
        ogmios = { port = 1337; };
        ctlServer = { port = 8081; };
        postgres = {
          port = 5432;
          user = "ctxlib";
          password = "ctxlib";
          db = "ctxlib";
        };
        # These values will generate the `config.toml` required by ogmios-datum-cache
        datumCache = {
          port = 9999;
          # If you override some part of `postgres` above, `dbConnectionString`
          # is automatically updated
          dbConnectionString = nixpkgs.lib.concatStringsSep
            " "
            [
              "host=postgres"
              "port=${toString postgres.port}"
              "user=${postgres.user}"
              "dbname=${postgres.db}"
              "password=${postgres.password}"
            ];
          blockFetcher = {
            firstBlock = {
              slot = 54066900;
              id = "6eb2542a85f375d5fd6cbc1c768707b0e9fe8be85b7b1dd42a85017a70d2623d";
            };
            autoStart = true;
            startFromLast = false;
            filter = builtins.toJSON { const = true; };
          };
        };
      };
    in

    {
      # `launchCtlRuntime` will generate a Nix expression from the provided
      # config, build it into a JSON file, and then run it with Arion
      #
      # Use `nix run .#<APP>` to run the services (e.g. `nix run .#ctl-runtime`)
      apps = perSystem (system: {
        ctl-runtime = (nixpkgsFor system).launchCtlRuntime runtimeConfig;
      });

      # `buildCtlRuntime` will generate a Nix expression that, when built with
      # `pkgs.arion.build`, outputs a JSON file compatible with Arion. This can
      # be run directly with Arion or passed to another derivation. Or you can
      # use `buildCtlRuntime` with `runArion` (from the `hercules-ci-effects`)
      # library
      #
      # Use `nix build .#<PACKAGE` to build. To run with Arion (i.e. in your
      # shell): `arion --prebuilt-file ./result up`
      packages = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          ctl-runtime = pkgs.arion.build {
            inherit pkgs;
            modules = [ (pkgs.buildCtlRuntime runtimeConfig) ];
          };
        });
    };
}
```

For launching services for developing CTL itself, see our documentation on [development](#launching-services-for-development).

### Changing network configurations

CTL supports using networks other than the public testnet to provide different development environments.

To choose a different network, you must specify an alternative way of getting the network configuration. The [cardano-configurations](https://github.com/input-output-hk/cardano-configurations) repo provides configs for `mainnet`, `staging` and a few other networks. The `network.name` parameter of `buildCtlRuntime` determines which of the network config directories is chosen.

You can also specify your own fork of `cardano-configurations` like this:

```
inputs.cardano-transaction-lib.inputs.cardano-configurations.follows = "...";
```

When changing networks, make sure that `network.magic` is correctly synchronized with value in config (see `protocolConsts.protocolMagic` in `byron.json`).

### Other requirements

In order to run most `Contract` actions, **you must use Nami wallet for the time being**. The following steps must be taken to ensure that you can run CTL contracts:

1. Install the [Nami extension](https://chrome.google.com/webstore/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo)
   - Due to limitations with Nami itself, only Chromium-based browsers are supported
2. Make sure that you have an active wallet
3. Make sure that you have set collateral for the wallet, which Nami reserves apart from other wallet UTxOs
4. Make sure that your wallet is running on the testnet (can be configured via a toggle in the settings menu)
