# Adding CTL as a Dependency

CTL can be imported as an additional dependency into a Purescript project built with Spago (i.e. by listing the project in your `packages.dhall`). Running CTL contracts requires several [runtime dependencies](./runtime.md) as well.

**Table of Contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Using CTL's Nix overlays](#using-ctls-nix-overlays)
- [Upgrading CTL](#upgrading-ctl)
- [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Using CTL's Nix overlays

CTL exposes two `overlay`s from its flake. You can use these in the Nix setup of your own project to use the same setup as we do, e.g. the same packages and PS builders:

- `overlays.purescript` contains Purescript builders to compile Purescript sources, build bundles with Webpack/esbuild (`bundlePursProject`), run unit tests using NodeJS (`runPursTest`), and run CTL contracts using Cardano Testnet (`runLocalTestnetTest`).
- `overlays.runtime` contains various packages and other tools used in CTL's runtime, including `ogmios`, `kupo`, and `cardano-node`. It also defines `buildCtlRuntime` and `launchCtlRuntime` to help you quickly launch all runtime services (see the [runtime docs](./runtime.md))

We've split the overlays into two components to allow users to more easily choose which parts of CTL's Nix infrastructure they would like to directly consume. For example, some users do not require a pre-packaged runtime and would prefer to build it themselves with more control over its components (e.g. by directly using `ogmios` from their own `inputs`). Such users might still like to use our `purescript` overlay -- splitting the `overlays` allows us to support this. `overlays.runtime` also contains several haskell.nix packages which may cause issues with `hackage.nix` versions in your own project.

Do note that `runLocalTestnetTest` in `overlays.purescript` requires the presence of all of our runtime components. If you choose not to consume `overlays.runtime`, please ensure that your package set contains these (e.g. by adding them to your own `overlays` when instantiating `nixpkgs`). You can find a complete list of the required runtime services [here](./cardano-testnet-testing.md#architecture).

To see an example project that uses both `overlays`, please refer to our [scaffolding template](../templates/ctl-scaffold/flake.nix). You can also use this template to conveniently initialize a new CTL-based project (`nix flake init -t github:Plutonomicon/cardano-transaction-lib` in a new directory). It will take a significant amount of time for spago to download the dependencies.

## Upgrading CTL

Unfortunately, the process of upgrading CTL is fairly involved. This is in part due to the complexity of the project and in part due to features inherent to Spago's approach to dependency management. The following assumes that you are using a project based on Nix flakes and using our overlays as outlined above.

Make sure to perform **all** of the following steps, otherwise you **will** encounter difficult-to-debug issues:

1. **Update your flake input**

- Update the `rev` you're using for CTL in your flake `inputs`
  - **Note**: Nix might throw an error about CTL following a "non-existent input" after doing this. The best way to solve this is to upgrade the version of Nix that you're using. Otherwise, `nix flake lock --update-input <NAME>`, where `NAME` corresponds to CTL in your flake's `inputs`, should solve this

2. **Update your Purescript dependencies**

- Update the CTL `version` in your `packages.dhall`. Make sure that this is the exact same revision as in your flake inputs
- Possibly update the `dependencies` section for CTL in your `packages.dhall`

  - You can find a list of CTL's dependencies in our own `spago.dhall` (but make sure to check at the correct revision)
  - You might also need to add new transitive git dependencies if CTL has added any to its own direct dependencies (i.e. you need to copy the matching stanzas from CTL's `packages.dhall` to your own; these are contained in the `additions` record in CTL's `packages.dhall`)

    - For example, if the following package `foo` is added to CTL's `additions` (in `packages.dhall`) between your old revision and the one you're upgrading to:

      ```dhall

      let additions =
            { foo =
              { dependencies =
                [ "bar"
                , "baz"
                ]
              }
            , repo = "https://github.com/quux/foo.git"
            , version = "0000000000000000000000000000000000000000"
            -- ...
            }
      ```

      You also need to add the same package, identically, to your own `packages.dhall`, otherwise the compiler will not be able to find it

- Run `spago2nix generate` and make sure to stage and commit the resulting `spago-packages.nix` if it has changed

3. **Update your JS dependencies**

- The NPM dependencies your project has must have the exact same versions CTL's own `package.json` specifies.
- You have to update `package-lock.json` by running `npm install`. If you are in a nix shell and use our setup that symlinks `./node_modules`, npm will complain about inability to write to the filesystem, use `npm i --package-lock-only` to skip writing to `node_modules`. If your `node_modules` are managed by Nix, you will have to re-enter the shell for the changes to apply.

4. **Update your webpack/esbuild config**

- Sometimes WebPack or esbuild configurations also come with breaking changes. Refer to the CHANGELOG.

## See also

- [How to use CTL-based apps from JS](./using-from-js.md)
- [Managing Contract environment correctly](./contract-environment.md)
- [Alternative setup with purs-nix instead of spago2nix](https://github.com/LovelaceAcademy/cardano-transaction-lib/tree/develop/templates/la-scaffold)
