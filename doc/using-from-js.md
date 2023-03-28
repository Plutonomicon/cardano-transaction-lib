<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [JavaScript SDKs](#javascript-sdks)
  - [Defining APIs in PureScript](#defining-apis-in-purescript)
  - [Using from NodeJS](#using-from-nodejs)
    - [Bundling for NodeJS](#bundling-for-nodejs)
    - [Calling from NodeJS](#calling-from-nodejs)
  - [Using from the browser](#using-from-the-browser)
    - [Bundling for the browser](#bundling-for-the-browser)
      - [WebAssembly and conditional imports](#webassembly-and-conditional-imports)
    - [Calling in the browser](#calling-in-the-browser)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# JavaScript SDKs

Normally, dApps involve three parts:

- on-chain logic (Plutus or Plutarch scripts)
- off-chain logic (in our case, implemented using CTL)
- user interface

Building CTL-based JavaScript SDKs is the simplest way to connect user interfaces (most commonly, web apps) with off-chain logic. These SDKs expose app-specific APIs for web developers to plug into the user interface. SDKs are normally consumable as NPM packages.

## Defining APIs in PureScript

Developers should start from reading [this PureScript guide](https://book.purescript.org/chapter10.html#calling-purescript-from-javascript) that shows how to call PureScript from JS. Our (older) PureScript version is using CommonJS modules and not ES modules, so `import` statements should be replaced with `require`.

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

## Using from NodeJS

### Bundling for NodeJS

To prepare the module defined above for use from other NodeJS code, `spago bundle-module` should be used:

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
  }
}
```

### Calling from NodeJS

The module above can be imported like this from NodeJS:

```javascript
const { initialize, config, run, finalize }  = require('./output.js');

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

The recommended way to bundle CTL for the browser is to use WebPack.

#### WebAssembly and conditional imports

We depend on WebPack's `DefinePlugin` to conditionally load either NodeJS or browser variant of dependencies that have WebAssembly parts.

That means that CTL _requires_ bundling it the same way when used as a dependency, as we do in development. If you intend to use another bundler, something like `DefinePlugin` should be used to transform the import headers from this:

```javascript
let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}
```

to only one of the import variants.

Our default [WebPack config](../webpack.config.js) uses `BROWSER_RUNTIME` environment variable to differentiate between two bundling options:

```js
  plugins: [
    new webpack.DefinePlugin({
      BROWSER_RUNTIME: !!process.env.BROWSER_RUNTIME,
    }),
```

There's [a claim that Vite bundler can also be used](https://github.com/Plutonomicon/cardano-transaction-lib/issues/79#issuecomment-1257036068), although we don't officially support this method.

### Calling in the browser

Webpack config contains `entry` field, pointing to the main file of the app.

Assuming we want to use the example app from above, it can be populated like this:

```js
import("./output.js").then(
    async ({ initialize, config, run, finalize }) => {

    const env = await initialize(config);
    try {
        await run(env);
    } finally {
        await finalize(env);
    }
});
```

The config also contains some setup for output target:

```js
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "bundle.js",
  },
```

But to actually build a page that can be opened in a browser, we use `HtmlWebpackPlugin`:

```js
    new HtmlWebpackPlugin({
      title: "ctl-scaffold",
      template: "./index.html",
      inject: false, // See stackoverflow.com/a/38292765/3067181
    }),
```

The HTML page should contain this import, pointing to output bundle filename:

```html
<script type="module" src="./bundle.js"></script>
```

`type="module"` is required here.

The whole webpage can be served with `BROWSER_RUNTIME=1 webpack-dev-server --progress` or built with `BROWSER_RUNTIME=1 webpack --mode=production`

## See also

- [How to import serialized Plutus scripts for NodeJS and the browser](./importing-scripts.md)
