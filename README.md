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

**NOTE**: CTL does **not** launch or provide these services for you. You must configure them and provide the appropriate values to the `ContractConfig` that you create to run your contracts. This repository contains [Makefile targets](#launching-services-for-development) to launch all required services on localhost, but this is intended for local development of CTL itself and is not suitable for deployment

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

There are a few Makefile targets provided for convenience, all of which require being in the Nix shell environment:

- `make run-testnet-node` starts the node in a Docker container
- `make run-testnet-ogmios` starts our fork of `ogmios` with the correct flags (i.e. config and node socket locations)
- `make query-testnet-sync` checks the node's sync status. If the node is fully synced, you will see:
  ```
  {  "epoch": 1005,
     "hash": "<HASH>",
     "slot": 7232440,
     "block": 322985,
     "era": "Alonzo",
     "syncProgress": "100.00"
  }
  ```
- `make run-datum-cache-postgres` runs a PostgreSQL docker container with the same username, password, and DB name as required for `ogmios-datum-cache`
- `make run-datum-cache-postgres-console` runs `psql` to access the datum cache DB directly; useful for debugging the datum cache

If you prefer to run these services locally without `make`, the environment variables `CARDANO_NODE_SOCKET_PATH` and `CARDANO_NODE_CONFIG` are also exported in the shell pointing to the correct locations as noted in the previous section.

### Building/testing the PS project and running it in the browser

- To build the project **without bundling and for a NodeJS environment**:
  - `nix build` _or_
  - `spago build`
- To test the project, currently only supported when running in a NodeJS environment:
  - `spago test` _or_
  - `npm run test`
- To run or build the project for the browser:
  - `npm run dev` will start a Webpack development server at `localhost:4008`
  - `npm run build` will output the Webpack-bundled project in `dist`

By default, Webpack will build a [small Purescript example](examples/Pkh2Pkh.purs). Make sure to follow the [instructions for setting up Nami](#other-requirements) before running the examples. You can point Webpack to another Purescript entrypoint by editing `examples/index.js`.

**Note**: The `BROWSER_RUNTIME` environment variable must be set to `1` in order to build/bundle the project properly for the browser (e.g. `BROWSER_RUNTIME=1 webpack ...`). For Node environments, leave this variable unset or set it to `0`.

### Adding PS/JS dependencies

#### Purescript

Unfortunately, we rely on `spago2nix`, which requires autogenerated Nix code (`spago-packages.nix`). This means that it is possible for our declared Purescript dependencies to drift from the autogen Nix code we import in to build Purescript derivations. If you add either a Purescript dependency, make sure to run `spago2nix generate` from within the Nix shell to update the autogen code from `spago2nix`. Do **not** edit `spago-packages.nix` by hand, or the build will likely break.

#### JS

If you add a dependency to `package.json`, make sure to update the lockfile with `npm i --package-lock-only` _before_ entering a new dev shell, otherwise the `shellHook` will fail. You'll need to remove the existing symlinked `node_modules` to do this (for some reason `npm` will _still_ try to write to the `node_modules`, but will fail because they're symlinked to the Nix store).

## Using CTL as a dependency

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
