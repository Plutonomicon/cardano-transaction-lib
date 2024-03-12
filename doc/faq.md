# CTL FAQ

This document lists common problems encountered by CTL users and developers.

**Table of Contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Bundling-related](#bundling-related)
  - [Q: `lib.something` is not a function, why?](#q-libsomething-is-not-a-function-why)
  - [Q: I see `spago: Error: Remote host not found`, why?](#q-i-see-spago-error-remote-host-not-found-why)
  - [Q: I see `WebAssembly module is included in initial chunk.` error, why?](#q-i-see-webassembly-module-is-included-in-initial-chunk-error-why)
  - [Q: I see `Cannot use 'import.meta' outside a module` error in the browser, why?](#q-i-see-cannot-use-importmeta-outside-a-module-error-in-the-browser-why)
  - [Q: I see `Module not found: Error: Can't resolve 'utf-8-validate'` error when bundling, why?](#q-i-see-module-not-found-error-cant-resolve-utf-8-validate-error-when-bundling-why)
- [Common Contract execution problems](#common-contract-execution-problems)
  - [Q: What are the common reasons behind BalanceInsufficientError?](#q-what-are-the-common-reasons-behind-balanceinsufficienterror)
  - [Q: CTL consumed my collateral](#q-ctl-consumed-my-collateral)
- [Time-related](#time-related)
  - [Q: Time-related functions behave strangely, what's the reason?](#q-time-related-functions-behave-strangely-whats-the-reason)
  - [Q: Time/slot conversion functions return `Nothing`. Why is that?](#q-timeslot-conversion-functions-return-nothing-why-is-that)
  - [Q: I'm getting `Uncomputable slot arithmetic; transaction's validity bounds go beyond the foreseeable end of the current era: PastHorizon`](#q-im-getting-uncomputable-slot-arithmetic-transactions-validity-bounds-go-beyond-the-foreseeable-end-of-the-current-era-pasthorizon)
- [Ecosystem](#ecosystem)
  - [Q: Why `aeson` and not `argonaut`?](#q-why-aeson-and-not-argonaut)
- [Environment-related](#environment-related)
  - [Q: I use wayland, the E2E browser fails on startup](#q-i-use-wayland-the-e2e-browser-fails-on-startup)
  - [Q: How to keep the number of WebSocket connections to a minimum?](#q-how-to-keep-the-number-of-websocket-connections-to-a-minimum)
  - [Package 'chromium-105.0.5195.125' is not supported on 'x86_64-darwin'](#package-chromium-10505195125-is-not-supported-on-x86_64-darwin)
- [Miscellaneous](#miscellaneous)
  - [Q: Why am I getting `Error: (AtKey "coinsPerUtxoByte" MissingValue)`?](#q-why-am-i-getting-error-atkey-coinsperutxobyte-missingvalue)
  - [Q: Why do I get an error from `foreign.js` when running Plutip tests locally?](#q-why-do-i-get-an-error-from-foreignjs-when-running-plutip-tests-locally)
  - [Q: How can I write my own Nix derivations using the project returned by `purescriptProject`?](#q-how-can-i-write-my-own-nix-derivations-using-the-project-returned-by-purescriptproject)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->
## Bundling-related

### Q: `lib.something` is not a function, why?

This is probably because npm is used directly. This is something users have reported when using `npm install` instead of having Nix manage the node dependencies (done automatically with `nix develop`, but if you have `node_modules` present in the working directory it will shadow the ones from the Nix store).

You can prevent `npm` from ever installing to local `node_modules` by enabling the `packageLockOnly` flag in the `shell` argument to `purescriptProject`. When enabled, `npm i` will always act as if the `--package-lock-only` flag has been passed. This is not enabled by default, but we recommend enabling it.

### Q: I see `spago: Error: Remote host not found`, why?

An error like this one:

```
spago:
Error: Remote host not found

URL: https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220224/packages.dhall
```

means that the CTL overlay hasn't been properly applied. Add `ctl.overlays.spago`.

### Q: I see `WebAssembly module is included in initial chunk.` error, why?

You may be trying to use `require` instead of `import` in the app entry point, see [here](./using-from-js.md).

### Q: I see `Cannot use 'import.meta' outside a module` error in the browser, why?

`type="module"` is required in the HTML script import, see [here](./using-from-js.md).

Other possible cause may be that you are trying to run a browser-targeted bundle in NodeJS.

### Q: I see `Module not found: Error: Can't resolve 'utf-8-validate'` error when bundling, why?

You probably forgot to set `BROWSER_RUNTIME=1` for the `webpack` command, see [here](./using-from-js.md).

## Common Contract execution problems

### Q: What are the common reasons behind BalanceInsufficientError?

Most contracts require at least two UTxOs to run (one will be used as a collateral). If you use a wallet with only one UTxO, e.g. a new wallet you just funded from the faucet, you need to send yourself at least 5 Ada to create another UTxO for the collateral.

Another thing to keep in mind is that due to [min-ada requirements](https://docs.cardano.org/native-tokens/minimum-ada-value-requirement/), the amount of Ada that is required to be *consumed* by a Tx is higher than the amount that must be *spent*, because CTL creates change UTxOs. The amount of Ada that should be present on a wallet depends on a number of factors, including the amount and quantity of tokens in the users wallet.

### Q: CTL consumed my collateral

CTL does not consume wallet collateral normally, but it still can happen.

In order to get the collateral UTxO, CTL uses the wallet and then marks the returned UTxO as locked internally. But some wallets (e.g. Gero) do not return the collateral the moment it is set, waiting for Tx confirmation first. In case a collateral is set right before the contract is started, CTL can accidentally spend the collateral, because we rely on CTL's own query layer to get a list of available UTxOs, and the wallet state may lag behind it, not returning the collateral to filter out at that moment.

## Time-related

### Q: Time-related functions behave strangely, what's the reason?

Local `cardano-node` lags behind the global network time, so when using time conversion functions (`slotToPosixTime`, `posixTimeToSlot`, etc.) users should be aware that the node sees time differently from the OS. During normal runs, the lag can be somewhere between 0 and 200 seconds.

To do anything time-related, it's best to rely on local node chain tip time, instead of using `Date.now()` as a source of truth. This is often a requirement when using `mustValidateIn`, because the node will reject the transaction if it appears too early.

### Q: Time/slot conversion functions return `Nothing`. Why is that?

Time/slot conversion functions depend on `eraSummaries` [Ogmios local state query](https://ogmios.dev/mini-protocols/local-state-query/), that returns era bounds and slotting parameters details, required for proper slot arithmetic. The most common source of the problem is that Ogmios does not return enough epochs into the future. [A possible symptom](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1057) is `CannotFindTimeInEraSummaries` in the error message.

When using Plutip, a solution may be [to increase the `epochSize` parameter](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1057#issuecomment-1450692539).

### Q: I'm getting `Uncomputable slot arithmetic; transaction's validity bounds go beyond the foreseeable end of the current era: PastHorizon`

Ensure your transaction's validity range does not go over `SafeZone` slots of the current era. The reason for this kind of errors is that time-related estimations are slot-based, and future forks may change slot lengths. So there is only a relatively small time window in the future during which it is known that forks cannot occur.

## Ecosystem

### Q: Why `aeson` and not `argonaut`?

Haskell's `aeson` library encodes long integers as JSON numbers, which leads to numeric truncation on decoder side if JS `Number` is used. Unfortunately, `purescript-argonaut` does not allow to use another type, because the truncation happens during `JSON.parse` call. `purescript-aeson` is our custom solution that bypasses this limitation by storing numbers as strings. It exposes a very similar API.

## Environment-related

### Q: I use wayland, the E2E browser fails on startup

We are aware of two error messages that can be show to you if you are using wayland.

<details>
  <summary>You can get something like this if you try to open the e2e browser</summary>

    cardano-transaction-lib@3.0.0~e2e-browser: Args: [
      '-c',
      "source ./test/e2e.env && spago run --main Test.Ctl.E2E -a 'e2e-test browser'"
    ]
    cardano-transaction-lib@3.0.0~e2e-browser: Returned: code: 1  signal: null
    cardano-transaction-lib@3.0.0~e2e-browser: Failed to exec e2e-browser script
    Error: cardano-transaction-lib@3.0.0 e2e-browser: `source ./test/e2e.env && spago run --main Test.Ctl.E2E -a 'e2e-test browser'`
    Exit status 1
        at EventEmitter.<anonymous> (/nix/store/lrvrir70n3966jybpjqw91smhcwlyn00-nodejs-14.20.0/lib/node_modules/npm/node_modules/npm-lifecycle/index.js:332:16)
        at EventEmitter.emit (events.js:400:28)
        at ChildProcess.<anonymous> (/nix/store/lrvrir70n3966jybpjqw91smhcwlyn00-nodejs-14.20.0/lib/node_modules/npm/node_modules/npm-lifecycle/lib/spawn.js:55:14)
        at ChildProcess.emit (events.js:400:28)
        at maybeClose (internal/child_process.js:1088:16)
        at Process.ChildProcess._handle.onexit (internal/child_process.js:296:5)
    pkgid cardano-transaction-lib@3.0.0
</details>

<details>
  <summary>This error message can be found while trying to run `e2e-test-debug`</summary>

    E2E tests
    ✗ plutip:http://localhost:4008/?plutip-nami-mock:OneShotMinting:

      Error: Failed to launch the browser process!
    [76104:76104:1207/234245.704016:ERROR:ozone_platform_x11.cc(238)] Missing X server or $DISPLAY
    [76104:76104:1207/234245.704036:ERROR:env.cc(255)] The platform failed to initialize.  Exiting.</details>
</details>

If you are under wayland you need to add `--ozone-platform=wayland` to the arguments for the browser. You can use the `--extra-browser-args` argument for this, as in `e2e-test browser --extra-browser-args="--ozone-platform=wayland"` or the `E2E_EXTRA_BROWSER_ARGS` environment variable.

### Q: How to keep the number of WebSocket connections to a minimum?

Use only one `ContractEnv` value. They are implicitly created every time `runContract` is called, so avoid using this function if you need to run multiple `Contract`s.

See [here](./contract-environment.md) for more info.

### Package 'chromium-105.0.5195.125' is not supported on 'x86_64-darwin'

Chromium is used in [E2E test suite](./e2e-testing.md). Chromium is pinned in nix shell by default, because system versions of chromium may be affected by [this bug](https://bugs.chromium.org/p/chromium/issues/detail?id=706008#c39)

To disable, set `withChromium` to `false` in [`purescriptProject`'s `shell` argument](https://github.com/Plutonomicon/cardano-transaction-lib/blob/946818b72e3ac1321feebe2944ca2986da2ddc01/templates/ctl-scaffold/flake.nix#L96).

## Miscellaneous

### Q: Why am I getting `Error: (AtKey "coinsPerUtxoByte" MissingValue)`?

This is because the node hasn't fully synced. The protocol parameter name changed from `coinsPerUtxoWord` to `coinsPerUtxoByte` in Babbage. CTL only supports the latest era, but Ogmios returns different protocol parameters format depending on current era of a local node.

### Q: Why do I get an error from `foreign.js` when running Plutip tests locally?

The most likely reason for this is that spawning the external processes from `Contract.Test.Plutip` fails. Make sure that all of the required services are on your `$PATH` (see more [here](./runtime.md); you can also set `shell.withRuntime = true;` to ensure that these are always added to your shell environment when running `nix develop`).

### Q: How can I write my own Nix derivations using the project returned by `purescriptProject`?

If the different derivation builders that `purescriptProject` gives you out-of-the-box (e.g. `runPursTest`, `bundlePursProject`, etc...) are not sufficient, you can access the compiled project (all of the original `src` argument plus the `output` directory that `purs` produces) and the generated `node_modules` using the `compiled` and `nodeModules` attributes, respectively. These can be used to write your own derivations without needing to recompile the entire project (that is, the generated output can be shared between all of your Nix components). For example:

```nix
{
  project = pkgs.purescriptProject { /* snip */ };

  # `purescriptProject` returns a number of specialized builders
  bundle = project.bundlePursProject { /* snip */ };

  # And attributes allowing you to create your own without
  # needing to deal with `spago2nix` or recompiling your
  # project in different components
  specialPackage = pkgs.runCommand "my-special-package"
    {
    }
    ''
      cp -r ${project.compiled}/* .
      # Do more stuff ...
    '';
}

```
