# cardano-transaction-lib

[![Hercules-ci][herc badge]][herc link]
[![Cachix Cache][cachix badge]][cachix link]

[herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[herc link]: https://hercules-ci.com/github/Plutonomicon/cardano-transaction-lib
[cachix badge]: https://img.shields.io/badge/cachix-public_plutonomicon-blue.svg
[cachix link]: https://public-plutonomicon.cachix.org

**cardano-transaction-lib** (CTL) is a Purescript library for building smart contract transactions on Cardano. It aims to port the functionality and interface of Plutus off-chain code to the browser environment.

**Table of Contents**

- [Goals:](#goals)
  - [Light wallet support](#light-wallet-support)
- [Setup and dev environment](#setup-and-dev-environment)
  - [Required services](#required-services)
  - [Other requirements](#other-requirements)
  - [Nix environment](#nix-environment)
  - [Launching services for development](#launching-services-for-development)
  - [Building/testing the PS project and running it in the browser](#buildingtesting-the-ps-project-and-running-it-in-the-browser)
  - [Generating PS documentation](#generating-ps-documentation)
  - [Adding PS/JS dependencies](#adding-psjs-dependencies)
    - [Purescript](#purescript)
    - [JS](#js)
- [Using CTL as a dependency](#using-ctl-as-a-dependency)
- [Architecture](#architecture)
- [Additional resources/tools:](#additional-resourcestools)

## Goals:

- [x] **Stage 1** Build a simple transaction in the browser that works with at least one light wallet (Nami)
- [x] **Stage 2** Once we can construct a simple user-to-user transaction, we will try to use the library to submit the tx with nami
- [x] **Stage 3** Once we have a simple working transaction, we will seek to build a Plutus smart contract transaction with datum from scratch
- [ ] **Stage 4** Once we can construct Plutus smart contract transactions, we will seek to build a library/DSL/interface such that transactions can be built using constraints and lookups - as close as possible to a cut-and-paste solution from Plutus' `Contract` monad code in Haskell (but with no guarantee that code changes are not necessary) (**In progress**)
- [ ] **Stage 5** Once we have a basic `Contract`-style API, we will further refine its public interface, expand wallet support (see [below](#light-wallet-support)), expose a test interface, provide a more ergonomic JS/TS API, support stake validators, and support CIP workflows on the public testnet
- [ ] **Stage 6** Once CTL's `Contract` interface has been stabilized, we will add support for even more wallets and attempt to deprecate CTL's currently required Haskell server

### Light wallet support

Support is planned for the following light wallets, roughly in order of implementation:

- [x] [Nami](https://namiwallet.io/)
- [ ] [Gero](https://gerowallet.io/)
- [ ] [Typhon](https://typhonwallet.io/)
- [ ] [Yoroi](https://yoroi-wallet.com/)
- [ ] [Eternl (formerly CCvault)](https://eternl.io/)

## Setup and dev environment

### Required services

In order to run CTL's `Contract` effects, several services are required. These can be configured through a `ContractConfig` that holds websocket connections, information about server hosts/ports, and other requisite information.

Services that are currently required:

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

CTL's overlay (contained in its flake `outputs`) provides some mechanisms for conveniently launching all runtime services using [Arion](https://docs.hercules-ci.com/arion)(itself a wrapper around `docker-compose`). To use this, you must have a setup based on Nix flakes (recommended as well for [using CTL as a dependency for Purescript projects](#using-ctl-as-a-dependency)).

Here is an example that uses the overlay to launch runtime services:

``` nix
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
      runtimeConfig = {
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
          # If you override some part of `postgres` above, you may also need to
          # modify the `dbConnectionString`
          dbConnectionString = nixpkgs.lib.concatStringsSep
            " "
            [
              "host=postgres"
              "port=${toString postgres.port}"
              "user=${postgres.user}"
              "dbname=${postgres.db}"
              "password=${postgres.password}"
            ];
          saveAllDatums = true;
          firstFetchBlock = {
            slot = 44366242;
            id = "d2a4249fe3d0607535daa26caf12a38da2233586bc51e79ed0b3a36170471bf5";
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

For launching services for developing CTL itself, see [below](#launching-services-for-development).

### Other requirements

In order to run most `Contract` actions, **you must use Nami wallet for the time being**. The following steps must be taken to ensure that you can run CTL contracts:

1. Install the [Nami extension](https://chrome.google.com/webstore/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo)
   - Due to limitations with Nami itself, only Chromium-based browsers are supported
2. Make sure that you have an active wallet
3. Make sure that you have set collateral for the wallet, which Nami reserves apart from other wallet UTxOs
4. Make sure that your wallet is running on the testnet (can be configured via a toggle in the settings menu)

### Nix environment

This project uses Nix flakes. In order to use flakes, you will need Nix version 2.4 or greater. You also need to enable additional experimental features. Make sure you have the following enabled in your `nix.conf` (typically located in `/etc/nix/` or `~/.config/nix/`) or in `nix.extraOptions` in your NixOS configuration:

```
experimental-features = nix-command flakes
```

You may also choose to enable these every time you use `nix` commands (and without modifying your `nix.conf`) by passing the following command-line options:

```
nix <COMMAND> --extra-experimental-features nix-command --extra-experimental-features flakes
```

Running `nix develop` in the root of the repository will place you in a development environment with all of the necessary executables, tools, config, etc... to:

- build the project or use the repl with `spago`
- use `npm` and related commands; all of the project's JS dependencies are symlinked from the Nix store into `node_modules` in the repository root
- use Ogmios and other tools with the correct configurations, socket path, etc... These are also set in the `devShell`'s `shellHook`

**NOTE**: As the Nix `devShell` currently symlinks the project's `node_modules`, **do not** use `npm install` in order to develop with `npm`. Use `nix develop` as noted above

### Launching services for development

To develop locally, you can use one the CTL flake to launch all required services (using default configuration values):

- The easiest way: `nix run -L .#ctl-runtime` will both build and run the services
- The same, but indirectly in your shell:
  ```
  $ nix build -L .#ctl-runtime
  $ arion --prebuilt-file ./result up
  ```

### Building/testing the PS project and running it in the browser

- To build the project **without bundling and for a NodeJS environment**:
  - `nix build` _or_
  - `spago build`
- To test the project, currently only supported when running in a NodeJS environment:
  - `spago test` _or_ `npm run test` will run both the integration and unit tests
  - `nix build .#checks.<SYSTEM>.ctl-unit-test` will build and run the unit tests (useful for CI)
- To run or build/bundle the project for the browser:
  - `make run-dev` _or_ `npm run dev` will start a Webpack development server at `localhost:4008`
  - `make run-build` _or_ `npm run build` will output a Webpack-bundled example module to `dist`
  - `nix build -L .#ctl-example-bundle-web` will build an example module using Nix and Webpack

By default, Webpack will build a [small Purescript example](examples/Pkh2Pkh.purs). Make sure to follow the [instructions for setting up Nami](#other-requirements) before running the examples. You can point Webpack to another Purescript entrypoint by changing the `ps-bundle` variable in the Makefile or in the `main` argument in the flake's `packages.ctl-examples-bundle-web`.

**Note**: The `BROWSER_RUNTIME` environment variable must be set to `1` in order to build/bundle the project properly for the browser (e.g. `BROWSER_RUNTIME=1 webpack ...`). For Node environments, leave this variable unset or set it to `0`.

### Generating PS documentation

- To build the documentation as HTML:
  - `spago docs`
- To build and open the documentation in your browser:
  - `spago docs --open`
- To build the documentation as Markdown:
  - `spago docs --format markdown`

The documentation will be generated in the `./generated_docs` folder, which contains an `index.html` which lists all modules by default. At this index is a checkbox to toggle viewing by package, and all the modules defined in our package will be available under `cardano-transaction-lib`.

Alternatively, you can view the documentation with `nix run -L .#docs` and opening `localhost:8080` in your browser. `nix build -L .#docs` will produce a `result` folder containg the documentation. Note that using the flake to generate the documentation will not include search indexing or viewing modules by package.

### Adding PS/JS dependencies

#### Purescript

Unfortunately, we rely on `spago2nix`, which requires autogenerated Nix code (`spago-packages.nix`). This means that it is possible for our declared Purescript dependencies to drift from the autogen Nix code we import in to build Purescript derivations. If you add either a Purescript dependency, make sure to run `spago2nix generate` from within the Nix shell to update the autogen code from `spago2nix`. Do **not** edit `spago-packages.nix` by hand, or the build will likely break.

#### JS

If you add a dependency to `package.json`, make sure to update the lockfile with `npm i --package-lock-only` _before_ entering a new dev shell, otherwise the `shellHook` will fail. You'll need to remove the existing symlinked `node_modules` to do this (for some reason `npm` will _still_ try to write to the `node_modules`, but will fail because they're symlinked to the Nix store).

## Using CTL as a dependency

CTL can be imported as an additional dependency into a Purescript project built with Spago (i.e. by listing the project in your `packages.dhall`). Note that the following always applies when using CTL from your project:

1. Only bundling with Webpack is supported (this is due to our internal dependency on `cardano-serialization-lib` which uses WASM; only Webpack is reliably capable of bundling this properly)
2. The environment variable `BROWSER_RUNTIME` determines which version of `cardano-serialization-lib` is loaded by CTL, so you must use it as well (i.e. set it to `1` for the browser; leave it unset for NodeJS)

Furthermore, CTL exposes an `overlay` from its flake. You can use this in the Nix setup of your own project to use the same setup as we do, e.g. the same packages and PS builders. Here is an example `flake.nix` that takes CTL as a dependency:

```nix
{
  inputs = {
    cardano-transaction-lib = {
      type = "github";
      owner = "Plutonomicon";
      repo = "cardano-transaction-lib";
      # NOTE
      # This should match the same revision as the one in your `packages.dhall` to ensure
      # the greatest compatibility
      rev = "f65eb08656f9da4ad1b83b09d25422bcf4835e9c";
    };

    # To use the same version of `nixpkgs` as we do
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
  };

  outputs = { self, cardano-transaction-lib, ... }:
    # some boilerplate
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      # generate `pkgs` with the CTL overlay applied. This gives you access to
      # various additional packages, using the same versions of CTL, including:
      #   - all of `easy-purescript-nix`
      #   - Ogmios and `ogmios-datum-cache`
      #   - `cardano-cli`
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ cardano-transaction-lib.overlay.${system} ];
      };

      # The overlay also include several tools for generating a PS project
      # using the same approach as CTL, under `purescriptProject`
      psProjectFor = system:
        let
          pkgs = nixpkgsFor system;
          # This is the root of the project. Typically, this would be `self`
          # for flakes-based projects
          #
          # You may also want to filter this to avoid bloat or unecessary
          # rebuilds when copying the source into derivations
          src = self;
        in
        pkgs.purescriptProject {
          inherit pkgs src;

          # Will be used to generate derivation name
          projectName = "your-project";

          # Optional arg to override the version of `nodejs` used, defaulting to
          # the version used by CTL itself. This will be used throughout
          # `purescriptProject`
          #
          # Note that the version of `purs` is not configurable, as CTL
          # will currently break with any other version (it uses 0.14.5
          # internally)
          nodejs = pkgs.nodejs-14_x;

          # Also optional; the path to the packages generated by `spago2nix`,
          # defaulting to:
          spagoPackages = ./spago-packages.nix;

          # The optional `shell` lets you configure the `devShell` that is
          # generated by `purescriptProject`
          #
          # All of the attrs below are entirely optional (shown here with
          # their default values)
          shell = {
            # Extra packages to include in the shell environment. By default
            # a common version of `nodejs`, `purs`, `spago`, and more are
            # included
            packages = [ ];

            # This will be appended to the `shellHook` that runs. By default,
            # the `shellHook` loads generated `node_modules` and exports a
            # modified `NODE_PATH` and `PATH`
            shellHook = "";

            # The same as `pkgs.mkShell.inputsFrom`
            inputsFrom = [ ];

            # Which formatter to be made available, `purty` is another option
            formatter = "purs-tidy";

            # If `purescript-language-server` should be included in the shell
            pursls = true;
          };
        };

    in
    {
      packages = perSystem (system:
        {
          # `buildPursProject` just builds all of the project's PS sources;
          # it can be helpful for CI to ensure the project builds or to
          # pass to another derivation
          your-project = (psProjectFor system).buildPursProject {
            # A list of directories to copy into the builder, relative to the
            # root provided in `purescriptProject.src`, and defaulting to
            # `["src"]`. If you have files needed at runtime, you must include
            # them as well
            sources = ["src"];
          };

          # `bundlePursProject` creates a JS bundle with webpack
          your-project-bundle = (psProjectFor system).bundlePursProject {
            # A list of directories to copy into the builder, relative to the
            # root provided in `purescriptProject.src`, and defaulting to
            # `["src"]`. If you have files needed at runtime, you must include
            # them as well
            sources = ["src" "exe"];
            # All of the following are optional and show with default values:
            #
            # The main Purscript module entrypoint
            main = "Main";
            # The JS entrypoint (must correspond to the one listed in the
            # webpack config), relative to the `src`
            entrypoint = "index.js";
            # The HTML template to render the bundle to (must correspond to
            # the template listed in the webpack config)
            htmlTemplate = "index.html";
            # If this should be bundled for the browser
            browserRuntime = true;
            # The path to the webpack config to use
            webpackConfig = "webpack.config.js";
            # The module that `spago bundle-module` should write to (must
            # match the one that is imported in your JS entrypoint). Is
            # relative to the `src` argument provided to `purescriptProject`
            bundledModuleName = "output.js";
          };
        });

      checks = perSystem (system:
        {
          # Build and run a test, also useful for CI
          your-project = (psProjectFor system).runPursTest {
            # Optional arg, the default value is:
            testMain = "Test.Main";
            # See note about `sources` above
            sources = [ "src" "test" "fixtures" ];
          };
        });


      devShell = perSystem
        (system:
          # This corresponds to the `shell` argument given above
          (psProjectFor system).devShell
        );
    };
}
```

We have recenly set up a small scaffolding repository for projects wishing to adopt CTL: https://github.com/mlabs-haskell/ctl-scaffold. More documentation and resources will be added soon to the repo

## Architecture

CTL is directly inspired by the Plutus Application Backend (PAB). Unlike PAB, however, CTL is a library and not a standalone process. Over the course of CTL's development, several questions have been raised as to how best create PAB-as-a-library:

1. How do we get the transaction in the right format?
   - This is handled by `cardano-serialization-lib`, a Rust library available as WASM
2. How do we query the chain?
   - This has been solved using Ogmios
   - We may, however, support a BlockFrost backend as well in the future
3. How do we query for datums (i.e. the datums themselves and not just their hashes)?
   - `ogmios-datum-cache` solves this problem
4. How do we submit the transaction?
   - This is done via browser-based light wallet integration in the browser based on CIP-30
5. How closely should we follow Plutus' `Contract` API?
   - CTL's `Contract` model is **significantly** less restrictive than Plutus' and allows for arbitrary effects within the `Contract` monad
   - Certain features cannot be directly translated into Purescript from Haskell due to differences between the two languages (e.g. CTL's `DatumType` and `RedeemerType` are type class with fundeps, as Purescript lacks any notion of type families/type-level functions)
6. A lingering concern remains around storage solutions, if needed
   - This can be in memory, in various browser storage solutions, or a decentralized DB like Fluree

## Additional resources/tools:

- [`cardano-serialization-lib`](https://github.com/SundaeSwap-finance/cardano-serialization-lib) (Sundae fork)
- [Ogmios](https://ogmios.dev) for chain queries
- [CIP-30](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030) (wallet interface - Nami partially implements this)
- [Nami docs](https://github.com/Berry-Pool/nami-wallet)
- [Alonzo CDDL spec](https://github.com/input-output-hk/cardano-ledger/blob/0738804155245062f05e2f355fadd1d16f04cd56/alonzo/impl/cddl-files/alonzo.cddl)
