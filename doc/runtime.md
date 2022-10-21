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

### Using CTL's `runtime` overlay

CTL's `overlays.runtime` (contained in its flake `outputs`) provides some mechanisms for conveniently launching all runtime services using [Arion](https://docs.hercules-ci.com/arion) (itself a wrapper around `docker-compose`). To use this, you must have a setup based on Nix flakes (recommended as well for [using CTL as a dependency for Purescript projects](./ctl-as-dependency.md)).

Here is an example that uses the `runtime` overlay to launch all of the required services:

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
      defaultSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      # generate `pkgs` with CTL's overlays applied. This gives you access to
      # various additional packages, using the same versions of CTL
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          cardano-transaction-lib.overlays.runtime
          # This one is optional. If you set `ctl-server.enable = true;`
          # in the runtime config as shown below, you must enable this
          # overlay. `ctl-server` itself is **only** required when using
          # CTL's `applyArgs` effect
          cardano-transaction-lib.overlays.ctl-server
          # you probably want this one too, although it's not demonstrated
          # in this example
          cardano-transaction-lib.overlays.purescript
        ];
      };

      # The configuration for the CTL runtime, which will be passed to the
      # expression that builds the JSON file used by Arion. This value can be
      # shared between `buildCtlRuntime` and `launchCtlRuntime`, as shown below
      #
      # You can refer to the final configuration value by passing a function
      # that takes a single arugment. Alternatively, you can pass an attrset
      # directly
      runtimeConfig = final: with final; {
        # You can add new services to the runtime. These should correspond to
        # Arion's `service` definition. The default is the empty attribute set
        extraServices = {
          # an image from dockerhub
          foo = {
            service = {
              image = "bar:foo";
              command = [
                "baz"
                "--quux"
              ];
            };

            # Or a Nix-based image
            foo2 = {
              service = {
                useHostStore = true;
                command = [
                  "${(nixpkgsFor system).baz}/bin/baz"
                  "--quux"
                ];
              };
            };
          };
        };
        # This corresponds to `docker-compose.raw` from Arion. You can add new
        # volumes, etc... using this
        extraDockerCompose = { volumes = { someVol = { }; }; };
        # This is the default. You can override this to run using different
        # configurations: see ./runtime.md#changing-network-configurations
        network = {
          name = "testnet";
          magic = 1097911063;
        };
        # *All* of these values are optional, and shown with their default
        # values. If you need even more customization, you can use `overideAttrs`
        # to change the values after calling `buildCtlRuntime` (e.g. a secrets
        # volume for the `postgres` service)
        node = {
          port = 3001;
          # the version of the node to use, corresponds to the image version tag,
          # # i.e. `"inputoutput/cardano-node:${tag}"`
          tag = "1.35.2";
        };
        ogmios = { port = 1337; };
        # If you don't need to use `applyArgs` (i.e. you're not using parameterized
        # scripts), you can disable CTL's server entirely in the runtime using
        # `{ ctlServer.enable = false; }`. Currently we default to enabling it
        # by default for backwards compatibility
        ctlServer = { enable = true; port = 8081; };
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

For launching services for developing CTL itself, see our documentation on [development](./development.md#launching-services-for-development).

### Changing network configurations

CTL supports using networks other than the public testnet to provide different development environments.

To choose a different network, you must specify an alternative way of getting the network configuration. The [cardano-configurations](https://github.com/input-output-hk/cardano-configurations) repo provides configs for `mainnet`, `staging` and a few other networks. The `network.name` parameter of `buildCtlRuntime` determines which of the network config directories is chosen.

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
