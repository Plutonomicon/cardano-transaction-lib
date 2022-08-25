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

The services that are currently required are:

- [Ogmios](https://ogmios.dev)
  - You **must** use Ogmios v5.2.0 or greater with CTL
  - Ogmios itself requires a running Cardano node, so you may also need to deploy a node. Node v1.34.0 or greater is recommended
  - You can also use [our fork](https://github.com/mlabs-haskell/ogmios) which has improved Nix integration
- [`ogmios-datum-cache`](https://github.com/mlabs-haskell/ogmios-datum-cache)
  - This is required to query for datums, which Ogmios itself does not support
  - This in turn requires a PostgreSQL DB
- [Our Haskell server](/server/README.md)
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

For launching services for developing CTL itself, see our documentation on [development](./development.md#launching-services-for-development).

### Manually starting the runtime components

It is _not_ necessary to use the flake app approach above (i.e. with `launchCtlRuntime`). You can start the runtime manually by running all of the components individually in your shell. This might be necessary if your platform is not supported by Arion or `docker-compose` (macOS users in particular have reported this).

The following steps should help you start the runtime without using `launchCtlRuntime`. **Note**: the following steps require Docker, but not `docker-compose`

#### Enter a development shell

CTL's `purescriptProject` function will produce a `devShell` suitable for use with your own flake. Pass the `withRuntime` flag to put all of the required runtime components in your development environment. Then, run `nix develop`. You will now have the necessary packages to manually start the runtime (`ogmios`, `ogmios-datum-cache`, and `ctl-server`). (_Note_: you don't need to use `purescriptProject`, you can still add the packages yourself to a `devShell` that you define yourself.)

Make sure that you have applied the necessary `overlays` provided by CTL, as outlined above.

#### Set the correct location for `cardano-configurations` and other node data

To launch Ogmios and, depending on the chosen method, `cardano-node` (see the steps below), you will need to point to a local copy of the correct revision of `cardano-configurations`. `cardano-configurations` is included in CTL's `runtime` overlay. You can add the following to `purescriptProject.shell.shellHook` to export the correct paths:

```nix
''
  # You can set this to any value you'd like, this is simply an example
  export NODE_DATA_DIR=.node
  # These subdirectories will be required in later steps as well
  mkdir -p "$NODE_DATA_DIR/{config,data,socket}"
  ln -s ${pkgs.cardano-configurations}/network/testnet "$NODE_DATA_DIR/config/testnet/"
  export NODE_CONFIG_PATH="$NODE_DATA_DIR/config/testnet"
''
```

You can also clone `cardano-configurations` locally, check out the correct revision, and export the path pointing to the local copy directly in your shell (rather than doing so via a `shellHook`).

#### Start `cardano-node`

##### With Docker

The simplest way to start `cardano-node` is using Docker. However, this may not work on macOS (please see the note below). To launch it via Docker:

```
docker run --rm \
  -e NETWORK=testnet \
  -v "$NODE_DATA_DIR"/socket:/ipc \
  -v "$NODE_DATA_DIR"/data:/data \
  inputoutput/cardano-node:<TAG>
```

Where `<TAG>` is the current tag supported by the CTL runtime (you can check CTL's own flake to find the current default tag).

This will mount the volumes required to communicate with the node container (specifically `/ipc`) under the `$NODE_DATA_DIR` we defined earlier (again, this can be any path you'd like; in the example above, it was set to `.node`).

**Note**: Depending on your Docker installation, you may need to `chmod` the mount point for the IPC volume on the host (in the example above, `$NODE_DATA_DIR/socket`). Ogmios must be able to read and write to the socket, and Docker often runs as root. If you don't set the correct permissions, Ogmios will be unable to communicate with the node. macOS users have reported that `chmod` does **not** resolve this problem; if this is the case for you, please refer to starting the node directly.

##### Directly

We have had Nix issues when using the `cardano-node` repository as a flake input without also setting `flake = false;` for it. Accordingly, we do **not** include any of the packages the repo defines in our development environment.

To add the packages to your `$PATH`, the easiest way is to use `nix shell`. You do not need to leave your shell environment provided by CTL: you can run the commands in the same shell session:

```
$ nix shell 'github:input-output-hk/cardano-node?ref=<TAG>#cardano-node'
```

Where `TAG` matches the version tag that CTL uses in its runtime (this corresponds to the Docker tag above).

The `cardano-node` package should now be available in your shell environment. Run the following to start the node:

```
$ cardano-node run --config "$NODE_CONFIG_PATH/cardano-node/config.json" \
  --topology "$NODE_CONFIG_PATH/cardano-node/topology.json" \
  --database-path "$NODE_DATA_DIR"/data/db \
  --socket-path "$NODE_DATA_DIR"/socket/node.socket \
  --host-addr 127.0.0.1 --port 3001
```

#### Start Ogmios

```
$ ogmios
    --host <HOST> \
    --port <PORT> \
    --node-socket .node/socket \
    --node-config "$NODE_CONFIG_PATH/cardano-node/config.json"
```

Where `$NODE_CONFIG_PATH` points to the path containing `cardano-configurations` as noted above.

#### Start Postgres

This is required for `ogmios-datum-cache`. Any combination of username, password, and DB name will work, but make sure that it matches what you provide to `ogmios-datum-cache`.

```
$ docker run -d --rm \
    -e "POSTGRES_USER=user" \
    -e "POSTGRES_PASSWORD=password" \
    -e "POSTGRES_DB=dbname" \
    -p 127.0.0.1:5432:5432 \
    postgres:13
```

#### Start `ogmios-datum-cache`

`ogmios-datum-cache` can take a large number of command-line arguments. The most important are `--server-port`, which corresponds to the Ogmios port, and the `--db-*` options, which must match those provided in step 4 above.

```
$ ogmios-datum-cache
    --server-api '' \
    --server-port 9999 \
    --ogmios-address 127.0.0.1 \
    --ogmios-port 1337 \
    --db-port 5432 \
    --db-host localhost \
    --db-user user \
    --db-name dbname \
    --db-password password \
    --block-slot 54066900 \
    --block-hash 6eb2542a85f375d5fd6cbc1c768707b0e9fe8be85b7b1dd42a85017a70d2623d \
    --block-filter ''
```

#### Start `ctl-server`

Finally, start `ctl-server`. **Note**: in the future, this service will be optional and only required if you depend on CTL's `applyArgs` effect.

```
$ ctl-server
```

### Changing network configurations

CTL supports using networks other than the public testnet to provide different development environments.

To choose a different network, you must specify an alternative way of getting the network configuration. The [cardano-configurations](https://github.com/input-output-hk/cardano-configurations) repo provides configs for `mainnet`, `staging` and a few other networks. The `network.name` parameter of `buildCtlRuntime` determines which of the network config directories is chosen.

You can also specify your own fork of `cardano-configurations` like this:

```
inputs.cardano-transaction-lib.inputs.cardano-configurations.follows = "...";
```

When changing networks, make sure that `network.magic` is correctly synchronized with value in config (see `protocolConsts.protocolMagic` in `byron.json`).

### Other requirements

In order to run most `Contract` actions in the browser, **you must use one of our supported wallets** (currently Nami, Gero, and Flint). The following steps must be taken to ensure that you can run CTL contracts:

1. Install the correct browser extension

- [Nami](https://chrome.google.com/webstore/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo)
- [Gero (testnet version)](https://chrome.google.com/webstore/detail/gerowallet-testnet/iifeegfcfhlhhnilhfoeihllenamcfgc)
- [Flint](https://chrome.google.com/webstore/detail/flint-wallet/hnhobjmcibchnmglfbldbfabcgaknlkj)

2. Make sure to set up an active wallet by creating a new one (or importing an existing one) and that the wallet has funds available
3. Make sure that you are running on the testnet (for development)

- In Nami and Flint, this can be toggled in the settings menu
- For Gero, different extensions are published for different networks

4. Make sure that collateral has been set for the wallet

- Each light wallet reserves these separately from other UTxOs
- For Nami and Gero, you can split existing UTxOs into smaller ones and use a part as collateral
- For Flint, you must have a 5-20 Ada UTxO available in your wallet to use. If you do not have such a UTxO, you must send yourself a UTxO of the correct size
