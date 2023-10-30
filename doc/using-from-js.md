<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Using CTL-based projects from JavaScript](#using-ctl-based-projects-from-javascript)
  - [`Makefile` guide](#makefile-guide)
    - [Configuration](#configuration)
    - [Commands](#commands)
  - [Nix tooling guide](#nix-tooling-guide)
  - [Defining APIs in PureScript](#defining-apis-in-purescript)
  - [Bundling](#bundling)
    - [WebAssembly and conditional imports](#webassembly-and-conditional-imports)
  - [Using from NodeJS](#using-from-nodejs)
    - [Bundling for NodeJS](#bundling-for-nodejs)
      - [Using spago](#using-spago)
      - [Using spago and a JS bundler](#using-spago-and-a-js-bundler)
    - [Calling from NodeJS](#calling-from-nodejs)
  - [Using from the browser](#using-from-the-browser)
    - [Bundling for the browser](#bundling-for-the-browser)
    - [Calling in the browser](#calling-in-the-browser)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Using CTL-based projects from JavaScript

Normally, dApps involve three parts:

- on-chain logic (Plutus, Aiken or Plutarch scripts)
- off-chain logic (in our case, implemented using CTL)
- user interface (usually, front-ends or NodeJS scripts)

Building CTL-based JavaScript SDKs is the simplest way to connect user interfaces (most commonly, web apps) with off-chain logic. These SDKs expose app-specific APIs for web developers to plug into the user interface.

This document guides you through important details related to the bundling process, but you don't need to adapt the code snippets: the bundler setup is already available in [the project template](../templates/ctl-scaffold/) via a [`Makefile`](../templates/ctl-scaffold/Makefile) (for use from the shell) or via our [Nix tooling](../nix/default.nix).

## `Makefile` guide

### Configuration

The [Makefile we provide in the project template](../templates/ctl-scaffold/Makefile) contains three configuration variables:

- `PS_ENTRY_POINT` - The name of the main Purescript module
- `PS_ENTRY_POINT_FUNCTION` - The entry point function in the main PureScript module. Leave empty to bundle as API module and not as executable script.
- `BROWSER_RUNTIME` - Whether to bundle for the browser Use "1" for "true" and leave empty for "false".

### `Makefile` Commands

- **Bundle** with either tool: `esbuild-bundle`  and `webpack-bundle`
- **Serve** the bundles via a webserver: `esbuild-serve` and `webpack-serve`. Note that both commands proxy Kupo requests to avoid CORS restrictions in development environment. Serving is a pre-requsite for [E2E testing](./e2e-testing.md).

## Nix tooling guide

We provide a number of functions that can be utilized in user's build pipelines.

Nix code is located [here](../nix/default.nix).

- `buildPursProject` is an equivalent of `spago build`, but builds dependencies separately.
- `bundlePursProjectEsbuild` is an equivalent of `make esbuild-bundle`
- `bundlePursProjectWebpack` is an equivalent of `make webpack-bundle`
- `runPlutipTest` lets one set up a Plutip-powered test suite easily.

However, these commands do NOT use the `Makefile`.

One notable restriction of Nix is that the build runtime is not allowed to access the internet.

## Defining APIs in PureScript

Developers should start from reading [this PureScript guide](https://book.purescript.org/chapter10.html#calling-purescript-from-javascript) that shows how to call PureScript from JS.

Suppose we want to wrap a single `Contract` into an interface to call it from JS with Nami wallet.

We have to expose [functions to manage contract environment](./contract-environment.md) - initialization and finalization, as well as a config value we will use.

```purescript
module Api where

import Prelude

import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.JsSdk (mkContractEnvJS, stopContractEnvJS)
import Contract.Monad (ContractEnv, runContractInEnv)
import Control.Promise (Promise, fromAff)
import Data.Function.Uncurried (Fn1, mkFn1)
import Effect.Unsafe (unsafePerformEffect)
import Scaffold (contract) -- our contract

initialize :: Fn1 ContractParams (Promise ContractEnv)
initialize = mkContractEnvJS

finalize :: Fn1 ContractEnv (Promise Unit)
finalize = stopContractEnvJS

run :: Fn1 ContractEnv (Promise Unit)
run = mkFn1 \env ->
  unsafePerformEffect $ fromAff $ runContractInEnv env contract

config  :: ContractParams
config = testnetNamiConfig -- use Nami wallet
```

- `Fn1` - `Fn10` types are wrappers that represent uncurried JavaScript functions with multiple arguments, and `mkFn1` - `mkFn10` are their constructors.
- `Contract.JsSdk` is a module containing synonyms for some `Contract.Monad` functions, but adapted for use in JS SDKs.
- `fromAff` converts `Aff a` to `Effect (Promise a)`, and `unsafePerformEffect` removes the `Effect` wrapper that is not needed on the JS side.

For more info on PureScript Foreign Function Interface (FFI) see [here](https://github.com/purescript/documentation/blob/master/guides/FFI.md) and [here](https://github.com/purescript/documentation/blob/master/guides/FFI-Tips.md).

## Bundling

CTL supports two bundlers: [esbuild](https://esbuild.github.io/) and [WebPack](https://webpack.js.org/), and two bundling targets: NodeJS and the browsers.

There are two bundling modes: with an executable entry point (the output is a runnable script) and without it (as an API library).

Bundling can be done either via Nix machinery or via `npm`/`Makefile` commands (that call the appropriate tools).

**IMPORTANT** NodeJS bundles created via Nix must be copied from `./result/` before use. [Explanation](./faq.md#q-syntaxerror-cannot-use-import-statement-outside-a-module).

### WebAssembly and conditional imports

We depend on bundler's source rewriting plugins to conditionally load either NodeJS or browser variant of dependencies that have WebAssembly parts.

That means that CTL _requires_ bundling it the same way when used as a dependency, as we do in development. If you intend to use a bundler we don't support, something like WebPack's `DefinePlugin` should be used to transform the import headers from this:

```javascript
let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}
```

to only one of the import variants, so that your final bundle does not include both versions of the lib.

Our bundler configs use `BROWSER_RUNTIME` environment variable to differentiate between two bundling targets.

[WebPack](../webpack.config.cjs):

```js
  plugins: [
    new webpack.DefinePlugin({
      BROWSER_RUNTIME: !!process.env.BROWSER_RUNTIME,
    }),
```

[Esbuild](../esbuild/config.js):

```js
...
config = {
  define: {
    BROWSER_RUNTIME: !!process.env.BROWSER_RUNTIME ? "true" : '""',
  },
...
```

In our [nix tooling](../nix/default.nix), look for `browserRuntime` parameter.

## Using from NodeJS

### Bundling for NodeJS

#### Using spago

To prepare the module [defined above](#defining-apis-in-purescript) for use from other NodeJS code, `spago bundle-module` should be used:

```bash
spago bundle-module -m Api --to output.js
```

The resulting bundle will NOT include its NodeJS dependencies in the same file.

It can be distributed via NPM by pointing `package.json` to it:

```js
{
  ...
  "main": "output.js",
  ...
  "dependencies": {
    // same dependencies as CTL itself uses should be put here
    // plus any additional dependencies your app needs
  }
}
```

However, this method does not minify the scripts and does not put them into a single file.

#### Using spago and a JS bundler

In order to bundle with minification for NodeJS, use webpack or esbuild as usual, but disable the browser runtime option.

The resulting bundle will still NOT include its NodeJS dependencies. If you want to do it for some reason, CTL does not provide tooling for that - but you can adjust the bundler options as needed.

### Calling from NodeJS

The module above can be imported like this from NodeJS:

```javascript
const { initialize, config, run, finalize } = await import('./output.js');

(async () => {
    const env = await initialize(config);
    try {
        await run(env);
    } finally {
        await finalize(env);
    }
})();
```

Notice that we used `finally` to finalize - this is because a running contract environment would prevent the script from exiting otherwise. Please read [this guide](./contract-environment.md) for info on how to manage the runtime environment correctly.

## Using from the browser

### Bundling for the browser

Bundling for the browser includes all JS dependencies into the bundle. The output will be a directory with multiple files - JS and WASM blobs. Dynamic loading code in the bundle ensures that they are loaded on demand.

If you want to bundle the API bundle above, you should unset the entrypoint PS function in the makefile:

```
PS_ENTRY_POINT_FUNCTION= make ...
```

(which is equivalent to setting `psEntryPoint = null;` in Nix).

See [WebPack config file](../templates/ctl-scaffold/webpack.config.cjs) or [Esbuild config file](../templates/ctl-scaffold/esbuild/config.js).

### Calling in the browser

Assuming we want to use the example app from above, the script can be populated like this:

```js
import("./dist/index.js").then(
    async ({ initialize, config, run, finalize }) => {

    const env = await initialize(config);
    try {
        await run(env);
    } finally {
        await finalize(env);
    }
});
```

Note that `import` returns a `Promise`.

## See also

- [FAQ answers for bundling-related questions](./faq.md)
- [How to import serialized Plutus scripts for NodeJS and the browser](./importing-scripts.md)
- [PureScript Foreign Function Interface (FFI) guide](https://github.com/purescript/documentation/blob/master/guides/FFI.md) and [FFI tips](https://github.com/purescript/documentation/blob/master/guides/FFI-Tips.md) for interop with JS.
- There's [a claim that Vite bundler can also be used](https://github.com/Plutonomicon/cardano-transaction-lib/issues/79#issuecomment-1257036068), although we don't officially support this method.
