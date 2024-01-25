# Adding CTL as a Dependency

CTL can be imported as an additional dependency into a Purescript project built with Spago (i.e. by listing the project in your `packages.dhall`). Running CTL contracts requires several [runtime dependencies](./runtime.md) as well.

**Table of Contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Caveats](#caveats)
- [Using CTL's overlays](#using-ctls-overlays)
- [Upgrading CTL](#upgrading-ctl)
- [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Caveats

The following caveats alway applies when using CTL from your project:

1. Only bundling with Webpack is supported (this is due to our internal dependency on `cardano-serialization-lib` which uses WASM with top-level `await`; only Webpack is reliably capable of bundling this properly)
2. The environment variable `BROWSER_RUNTIME` determines which version of `cardano-serialization-lib` is loaded by CTL, so you must use it as well (i.e. set it to `1` for the browser; leave it unset for NodeJS)

## Using CTL's overlays

CTL exposes two `overlay`s from its flake. You can use these in the Nix setup of your own project to use the same setup as we do, e.g. the same packages and PS builders:

- `overlays.purescript` contains Purescript builders to compile Purescript sources, build bundles with Webpack (`bundlePursProject`), run unit tests using NodeJS (`runPursTest`), and run CTL contracts on a private testnet using Plutip (`runPlutipTest`).
- `overlays.runtime` contains various packages and other tools used in CTL's runtime, including `ogmios`, `kupo`, and `plutip-server`. It also defines `buildCtlRuntime` and `launchCtlRuntime` to help you quickly launch all runtime services (see the [runtime docs](./runtime.md))

We've split the overlays into two components to allow users to more easily choose which parts of CTL's Nix infrastructure they would like to directly consume. For example, some users do not require a pre-packaged runtime and would prefer to build it themselves with more control over its components (e.g. by directly using `ogmios` from their own `inputs`). Such users might still like to use our `purescript` overlay -- splitting the `overlays` allows us to support this. `overlays.runtime` also contains several haskell.nix packages which may cause issues with `hackage.nix` versions in your own project.

Do note that `runPlutipTest` in `overlays.purescript` requires the presence of all of our runtime components. If you choose not to consume `overlays.runtime`, please ensure that your package set contains these (e.g. by adding them to your own `overlays` when instantiating `nixpkgs`). You can find a complete list of the required runtime services [here](./plutip-testing.md#architecture).

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

- If CTL has added any JS dependencies, these will also need to be added to your own `package.json`
- Similarly, if any of CTL's JS dependencies have changed versions, you will need to use the **exact** same version in your own `package.json`
- That is, avoid using the `~` or `^` prefixes (e.g use versions like `"1.6.51"` instead of `"^1.6.51"`)
- If you're using a `package-lock.json` (which is _highly_ recommended), you can update the lockfile with `npm i --package-lock-only`

4. **Update your webpack config**

- Sometimes the WebPack configuration also comes with breaking changes. Common source of problems are changes to `resolve.fallback`, `plugins` and `experiments` fields of the WebPack config. Use `git diff old-revision new-revision webpack.config.js` in the root of a cloned CTL repo, or use `git blame`.

## See also

- [How to use CTL-based apps from JS](./using-from-js.md)
- [Managing Contract environment correctly](./contract-environment.md)
- [Alternative setup with purs-nix instead of spago2nix](https://github.com/LovelaceAcademy/cardano-transaction-lib/tree/develop/templates/la-scaffold)
