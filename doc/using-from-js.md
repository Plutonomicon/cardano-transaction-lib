<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Building JavaScript SDKs with CTL](#building-javascript-sdks-with-ctl)
  - [SDK packaging for NodeJS](#sdk-packaging-for-nodejs)
  - [SDK bundling for the browser](#sdk-bundling-for-the-browser)
  - [Defining SDK APIs in PureScript](#defining-sdk-apis-in-purescript)
  - [Calling the SDK API](#calling-the-sdk-api)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Building JavaScript SDKs with CTL

Normally, dApps involve three parts:

- on-chain logic (Plutus, Plutarch or Aiken scripts)
- **off-chain logic** (in our case, implemented using CTL)
- user interface

Providing CTL-based JavaScript SDKs is the simplest way to connect user interfaces (most commonly, web apps) with off-chain logic. These SDKs expose app-specific APIs for web developers to plug into the user interface. SDKs are normally consumable as NPM packages.

Explore [the template](../templates/ctl-scaffold/package.json) or CTL itself for an example setup. See [the WebPack config](../webpack.config.cjs) or the [esbuild config](../esbuild/config.js) we provide.

## SDK packaging for NodeJS

NodeJS apps do not require to be bundled in order to be run (however, it is possible, see the [Makefile](../Makefile) options).

An NPM package with `main` set to the compiled JS entry point (e.g. `./output/ApiModuleName.js`) is sufficient, but the runtime dependencies of said package must include the same package versions CTL itself uses in its `package.json`.

## SDK bundling for the browser

SDKs must be bundled to be usable in the browser. We support two bundlers: esbuild and WebPack. There are two options how to approach bundling and packaging:

1. bundling a CTL-based SDK *before* consuming it as dependency in the app, i.e. putting the bundled sources in an NPM package. Bundling twice is not a good practice, and it is hard to even make it work, so in case this path is chosen, the developer should ensure that the SDK does not get bundled twice by the second bundler.

2. **[recommended]** bundling a CTL-based SDK together with the UI part of the app. This is simpler, but in case a bundler different from esbuild or WebPack is used, problems may arise due to bundler differences. It should be possible to use other bundlers, as long as they support async top-level imports, WebAssembly and [`browser` package.json field](https://github.com/defunctzombie/package-browser-field-spec).

## Defining SDK APIs in PureScript

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

## Calling the SDK API

The module above can be imported like this:

```javascript
import { initialize, config, run, finalize } from 'your-api-package';

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

## See also

- [How to import Plutus scripts into CTL for NodeJS and the browser](./importing-scripts.md)
