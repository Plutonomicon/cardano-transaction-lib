# Development

This document outlines development workflows for CTL itself. You may also wish to read our documentation on CTL's [runtime dependencies](./runtime.md), which are a prerequisite for most development.

**Table of Contents**

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Nix environment](#nix-environment)
- [Launching services for development](#launching-services-for-development)
- [Workflows](#workflows)
  - [Building](#building)
  - [Testing](#testing)
    - [With NodeJS](#with-nodejs)
    - [With Nix](#with-nix)
    - [Nix checks](#nix-checks)
  - [Bundling for the browser](#bundling-for-the-browser)
- [Generating PS documentation](#generating-ps-documentation)
- [Adding PS/JS dependencies](#adding-psjs-dependencies)
  - [Purescript](#purescript)
  - [JS](#js)
- [Switching development networks](#switching-development-networks)
- [Maintaining the template](#maintaining-the-template)
- [Updating the template](#updating-the-template)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Nix environment

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

## Launching services for development

To develop locally, you can use one the CTL flake to launch all required services (using default configuration values):

- The easiest way: `npm run start-runtime` will both build and run the services
- The same, but indirectly in your shell:
  ```
  $ nix build -L .#ctl-runtime
  $ arion --prebuilt-file ./result up
  ```

## Workflows

### Building

To **build** the project **without bundling and for a NodeJS environment**:

- `nix build` _or_
- `spago build`

### Testing

#### With NodeJS

- `npm run unit-test` for unit tests (no need for a runtime) -  [entry point](../test/Unit.purs)
- `npm run integration-test` for integration tests (requires a [runtime](./runtime.md#ctl-backend)) -  [entry point](../test/Integration.purs)
- `npm run testnet-test` for Cardano Testnet integration tests (does not require a runtime) - [entry point](../test/Testnet.purs)
- `npm run staking-test` to run [Cardano Testnet](./cardano-testnet-testing.md)-powered tests for ADA staking functionality - [entry point](../test/Testnet/Staking.purs)
- `npm run blockfrost-test` for [Blockfrost-powered tests](./blockfrost.md) (does not require a runtime, but needs [some setup](./blockfrost.md#setting-up-a-blockfrost-powered-test-suite)) - [entry point](../test/Blockfrost/Contract.purs)
- `npm run blockfrost-local-test` for self-hosted [Blockfrost-powered tests](./blockfrost.md) (requires a [local Blockfrost runtime](./blockfrost.md#running-blockfrost-locally)) - [entry point](../test/Blockfrost/Contract.purs)
- `npm run e2e-test` for [tests with a headless browser](./e2e-testing.md) (requires a runtime and the tests served via HTTP: `npm run start-runtime` and `npm run e2e-serve` or `esbuild-serve`)

#### With Nix

Here and below, `<SYSTEM>` should be replaced with [one of the supported systems](https://github.com/Plutonomicon/cardano-transaction-lib/blob/15fd9c5b683df47134dce4a0479f1edc30d4b6f7/flake.nix#L51) that you use, e.g. `x86_64-linux`.

- Unit tests: `nix build .#checks.<SYSTEM>.ctl-unit-test`
- [E2E tests in Nix with wallet mocks](./e2e-testing.md#using-cip-30-mock-with-cardano-testnet): `nix build -L .#checks.<SYSTEM>.ctl-e2e-test`
- Contract tests ([Cardano Testnet](./cardano-testnet-testing.md)): `nix build -L .#checks.<SYSTEM>.ctl-local-testnet-test`
- [Staking](./staking.md) tests ([Cardano Testnet](./cardano-testnet-testing.md)): `nix build -L .#checks.<SYSTEM>.ctl-staking-test`

#### Nix checks

- Check formatting: `nix build -L .#checks.<SYSTEM>.formatting-check`
- Check correctness of `package.json` dependencies in template: `nix build -L .#checks.<SYSTEM>.template-deps-json`
- Check correctness of Spago dependencies in template: `nix build -L .#checks.<SYSTEM>.template-dhall-diff`
- Check that CTL revisions match in Spago package set and Nix in template: `nix build -L .#checks.<SYSTEM>.template-version`
- Check that examples don't use internal modules: `nix build -L .#checks.<SYSTEM>.examples-imports-check`

### Bundling for the browser

To run or build/bundle the project for the browser:

- `npm run {webpack|esbuild}-serve` will start a Webpack development server at `localhost:4008`, which is required for [E2E tests](./e2e-testing.md)
- `npm run {webpack|esbuild}-bundle` will output a bundled example module to `dist` (or `nix build -L .#ctl-example-bundle-web-{webpack|esbuild}` to build an example module using Nix into `./result/`)

By default, the bundler will build a [Purescript module](../examples/ByUrl.purs) that serves multiple example `Contract`s depending on URL (see [here](./e2e-testing.md#serving-the-contract-to-be-tested)). You can point the bundler to another Purescript entrypoint by changing the `ps-bundle` variable in the Makefile or in the `main` argument in the flake's `packages.ctl-examples-bundle-web`.

You will also need a light wallet extension pre-configured for the correct Cardano network to run a `Contract`.

**Note**: The `BROWSER_RUNTIME` environment variable must be set to `1` in order to build/bundle the project properly for the browser (e.g. `BROWSER_RUNTIME=1 webpack ...`). For NodeJS environments, leave this variable unset.

**Note**: The `KUPO_HOST` environment variable must be set to the base URL of the Kupo service in order to successfully serve the project for the browser (by default, `KUPO_HOST=http://localhost:1442`).

## Generating PS documentation

CTL PureScript docs are publicly deployed [here](https://plutonomicon.github.io/cardano-transaction-lib/).

- To build the documentation as HTML:
  - `spago docs`
- To build and open the documentation in your browser:
  - `spago docs --open`
- To build the documentation as Markdown:
  - `spago docs --format markdown`

The documentation will be generated in the `./generated_docs/html` directory, which contains an `index.html` which lists all modules by default. At this index is a checkbox to toggle viewing by package, and all the modules defined in our package will be available under `cardano-transaction-lib`.

Alternatively, you can view the documentation with `nix run -L .#docs` and opening `localhost:8080` in your browser. `nix build -L .#docs` will produce a `result` folder containing the documentation.

## Adding PS/JS dependencies

### Purescript

Unfortunately, we rely on `spago2nix`, which requires autogenerated Nix code (`spago-packages.nix`). This means that it is possible for our declared Purescript dependencies to drift from the autogen Nix code we import in to build Purescript derivations. If you add either a Purescript dependency, make sure to run `spago2nix generate` from within the Nix shell to update the autogen code from `spago2nix`. Do **not** edit `spago-packages.nix` by hand, or the build will likely break.

Don't forget to update the [template `packages.dhall`](../templates/ctl-scaffold/packages.dhall) to use the exact same versions.

### JS

If you add a dependency to `package.json`, make sure to update the lockfile with `npm i --package-lock-only` _before_ entering a new dev shell, otherwise the `shellHook` will fail. You'll need to remove the existing symlinked `node_modules` to do this (for some reason `npm` will _still_ try to write to the `node_modules`, but will fail because they're symlinked to the Nix store).

Don't forget to update the [template `package.json`](../templates/ctl-scaffold/package.json) to use the exact same versions, and then make sure the [`package-lock.json` file](../templates/ctl-scaffold/package-lock.json) is updates as well.

## Switching development networks

Set new `network.name` and `network.magic` in `runtime.nix`. Also see [Changing network configurations](./runtime.md#changing-network-configurations)

## Maintaining the template

[The template](../templates/ctl-scaffold/) must be kept up-to-date with the repo. Although there are some checks for common problems in CI, it's still possible to forget to update the `package-lock.json` file.

## Updating the template

1. Update the revision of CTL in the template's `flake.nix`
2. Update the npm packages in the `package.json` (if needed)
3. Run `npm i` to update the lockfile (if there are NPM dependency version changes)
4. Update the revisions in the template's `packages.dhall` (CTL version must match the one in `flake.nix`)
5. Run `spago2nix generate`
6. Run `nix develop`

[This helper script](../scripts/template-check.sh) can be used to make sure the template can be initialized properly from a given revision.
