# Getting Started with CTL

This guide will help you get started writing contracts with CTL. Please also see our guide [comparing CTL with Plutus/PAB](./plutus-comparison.md) which offers a more abstract overview of the project.

**Table of Contents**

- [Prerequisites](#prerequisites)
  * [Setting up a new project](#setting-up-a-new-project)
  * [Other prerequisites](#other-prerequisites)
- [Importing CTL modules](#importing-ctl-modules)
  * [The `Contract` interface](#the--contract--interface)
  * [Our `Prelude`](#our--prelude-)
- [Executing contracts and the `ContractEnv`](#executing-contracts-and-the--contractenv-)
  * [Making the `ContractEnv`](#making-the--contractenv-)
- [Building and submitting transactions](#building-and-submitting-transactions)
  * [Using compiled scripts](#using-compiled-scripts)
- [Testing](#testing)
  * [Without a light wallet](#without-a-light-wallet)
  * [With a light wallet](#with-a-light-wallet)

## Prerequisites

### Setting up a new project

The easiest way to create a new CTL project is to use our `ctl-scaffold` flake template. This lives in the CTL repo -- you can have a look [here](../templates/ctl-scaffold). It contains a simple, yet complete, flakes-based scaffolding project with example `outputs` for a CTL project, including its runtime.

A new project can be initialized as follows:

```
$ mkdir new-project && cd new-project
$ nix flake init -t github:Plutonomicon/cardano-transaction-lib
$ git init
$ git commit -a -m 'Initial commit'
```

**Note**: Nix flakes are just source trees with a `flake.nix` file in them, so initializing a `git` repo as illustrated above is necessary to have a working project. Source files not tracked by a VCS are invisible to Nix when using flakes, so do not skip that (or a similar) step!

You can learn more about using CTL as a dependency [here](./ctl-as-dependency.md).

### Other prerequisites

You will also need to become familiar with [CTL's runtime](./runtime.md) as its runtime services are required for executing virtually all contracts.

## Importing CTL modules

### The `Contract` interface

CTL's public interface is contained in the `Contract.*` namespace. We recommend to always prefer imports from the `Contract` namespace. That is, **avoid importing any CTL modules in `Ctl.Internal.*`**. Importing internal modules will make your code more brittle and susceptible to breakages when upgrading CTL versions.

For example, avoid the following:

```purescript

-- Anything not in in the `Contract` namespace should be considered an
-- **internal** CTL module

import Ctl.Internal.Types.TokenName (TokenName)
import Ctl.Internal.Types.Scripts (MintingPolicy)

```

Instead, prefer:

```purescript

import Contract.Value (TokenName)
import Contract.Scripts (MintingPolicy)

```

### Our `Prelude`

Unlike Haskell, Purescript's `Prelude` is not imported implicitly in every module and is much smaller in scope (for example, common non-primitive types like `Maybe` are contained in their own packages, rather than in the `Prelude`). Rather than require users to import Purescript's `Prelude` and other common modules directly, we offer a `Contract.Prelude` that re-exports Purescript's `Prelude`, several common modules (e.g. `Data.Maybe`, `Data.Either`, etc...), and CTL-specific functionality. **We recommend using `Contract.Prelude` as a replacement for `Prelude` in projects using CTL**, particularly for developers who are less familiar with Purescript and its divergences from Haskell.

## Executing contracts and the `ContractEnv`

Unlike [Plutus/PAB](plutus-comparison.md#the-contract-type), CTL is structued internally around a familiar `mtl`-style monad transformer stack. As such, contracts written in CTL are called from other Purescript code (i.e. CTL has no concept of "endpoints" or "activation"). The top-level of your program written in CTL will look like the following:

```purescript
main :: Effect Unit
main = ...
```

(`Effect` is Purescript's synchronous effect monad.)

Internally, CTL uses Purescript's `Aff` monad, which represents asynchronous effects. Thus, you must first call the eliminator for `Aff` to run your `Contract` code:

```purescript
main :: Effect Unit
main = Contract.Monad.launchAff_ do -- we re-export this for you
 ...
```

Then use the eliminator `Contract.Monad.runContract` with a config specifying network and wallet:

```purescript
main :: Effect Unit
main = Contract.Monad.launchAff_ do
  runContract Contract.Config.testnetNamiConfig do
    ...
```

### Making the `ContractEnv`

The `ContractEnv` type contains configuration values and websocket connections that are required to execute contracts written in CTL. The users should not construct it directly - `Contract.Config.ConfigParams` should be used instead.

For local development and testing, we provide `Contract.Config.testnetConfig` where all service hosts are set to `localhost` and the `logLevel` is set to `Trace`.

It is **not recommended to directly construct or manipulate a `ContractEnv` yourself** as the process of making a new config initializes websockets. Instead, use `Contract.Monad.ConfigParams` with `runContract`.

As explained in the [Plutus/PAB comparison](plutus-comparison.md#the-contract-type), the `ContractEnv` environment uses Purescript's extensible records. This can also be done via `ConfigParams`, which holds an `extraConfig` field corresponding to the `Row Type` argument to `ContractEnv` (and by extension, `Contract`).

A special `Contract.Config.WalletSpec` type is used to specify which wallet to use during the `Contract` lifetime.

An example of building a `Contract` via `ConfigParams` is as follows:

```purescript
main :: Effect Unit
main = Contract.Monad.launchAff_ do -- we re-export this for you
  let
    (config :: ConfigParams (apiKey :: String)) =
      { ogmiosConfig: defaultOgmiosWsConfig
      , datumCacheConfig: defaultDatumCacheWsConfig
      , ctlServerConfig: defaultServerConfig
      , networkId: TestnetId
      , logLevel: Trace
      , extraConfig: { apiKey: "foo" }
      , walletSpec: Just ConnectToNami
      , customLogger: Nothing
      }
  runContract config someContractWithApiKeyInEnv

-- As we provided `(apiKey :: String)` to the `extraConfig` above, we can now
-- access it in the reader environment of any `Contract` actions call using
-- `askConfig`.
someContractWithApiKeyInEnv
  :: forall. Contract (apiKey :: String) Unit
  -- We can also retain polymorphism by adding `| r` to the row type:
  --   :: forall (r :: Row Type). Contract (apiKey :: String | r) Unit
someContractWithApiKeyInEnv = do
  { apiKey } <- askConfig
  ...
```

When using custom environments (e.g. in production), services can be configured to point to the same port with different paths (a webserver is needed to set that up):

```purescript
customOgmiosWsConfig :: ServerConfig
customOgmiosWsConfig =
  { port: UInt.fromInt 80
  , host: "localhost"
  , secure: false
  , path: Just "/api/ogmios"
  }

customDatumCacheWsConfig :: ServerConfig
customDatumCacheWsConfig =
  { port: UInt.fromInt 80
  , host: "localhost"
  , secure: false
  , path: Just "/api/ogmios-datum-cache"
  }
```

## Building and submitting transactions

Unlike PAB, CTL obscures less of the build-balance-sign-submit pipeline for transactions and most of the steps are called individually. The general workflow in CTL is similar to the following:

- Build a transaction using `Contract.ScriptLookups`, `Contract.TxConstraints` and `Contract.BalanceTxConstraints` (it is also possible to directly build a `Transaction` if you require even greater low-level control over the process, although we recommend the constraints/lookups approach for most users):

  ```purescript
  contract = do
    let
      constraints :: TxConstraints Unit Unit
      constraints = 
        TxConstraints.mustPayToScript vhash unitDatum
          (Value.lovelaceValueOf $ BigInt.fromInt 2_000_000)

      lookups :: ScriptLookups PlutusData
      lookups = ScriptLookups.validator validator

      balanceTxConstraints :: BalanceTxConstraints.BalanceTxConstraintsBuilder
      balanceTxConstraints =
        BalanceTxConstraints.mustBalanceTxWithAddress address
          <> BalanceTxConstraints.mustNotSpendUtxoWithOutRef nonSpendableOref
        
    -- `liftedE` will throw a runtime exception on `Left`s
    unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    ...
  ```

- Balance it using `Contract.Transaction.balanceTx` (or `Contract.Transaction.balanceTxWithConstraints` if you need to adjust the balancer behaviour) and then sign it using `signTransaction`:
  ```purescript
  contract = do
    ...
    -- `liftedE` will throw a runtime exception on `Left`s
    balancedTx <-
      liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
    balancedSignedTx <- signTransaction balancedTx 
    ...
  ```

- Submit using `Contract.Transaction.submit`:

  ```purescript
  contract = do
    ...
    txId <- submit balancedSignedTx
    logInfo' $ "Tx ID: " <> show txId
  ```

### Using compiled scripts

To use your own scripts, compile them to any subdirectory in the root of your project (where `webpack.config.js` is located) and add a relative path to `webpack.config.js` under the `resolve.alias` section. In CTL, we have the `Scripts` alias for this purpose. Note the capitalization of `Scripts`: it is necessary to disambiguate it from local folders.

First, in your `webpack.config.js`, define an `alias` under `module.exports.resolve.alias` in order to `require` the compiled scripts from JS modules:

```javascript
const path = require("path");

module.exports = {
  // ...
  resolve: {
    modules: [process.env.NODE_PATH],
    extensions: [".js"],
    fallback: {
      // ...
    },
    alias: {
      // You should update this path to the location of your compiled scripts,
      // relative to `webpack.config.js`
      Scripts: path.resolve(__dirname, "fixtures/scripts"),
    },
  },
};
```

You must also add the following to `module.exports.module.rules`:

```javascript
module.exports = {
  // ...
  module: {
    rules: [
      {
        test: /\.plutus$/i,
        type: "asset/source",
      },
      // ...
    ],
  },
};
```

This enables inlining your serialized scripts in `.js` files, to then be loaded in Purescript via the FFI:

```javascript
// inline .plutus file as a string
exports.myscript = require("Scripts/myscript.plutus");
```

And on the purescript side, the script can be loaded like so:

```purescript
foreign import myscript :: String

parseValidator :: Contract () Validator
parseValidator = wrap <<< wrap
  <$> Contract.TextEnvelope.textEnvelopeBytes myscript PlutusScriptV1

myContract cfg = runContract_ cfg $ do
  validator <- parseValidator
  ...
```

This way you avoid hardcoding your scripts directly to .purs files which could lead to synchronization issues should your scripts change.

**Note**: The `alias` method above will only work in the browser when bundling with Webpack. In order to load the scripts for both browser and NodeJS environments, you can use the `BROWSER_RUNTIME` environment variable like so:

```javascript
let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/my-script.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve(__dirname, "../../fixtures/scripts/my-script.plutus"),
    "utf8"
  );
}
exports.myScript = script;
```

Note that the relative path passed to `path.resolve` for the NodeJS case starts from the `output` directory that the Purescript compiler produces.

## Testing

### Without a light wallet

We provide `KeyWallet` to enable testing outside of the browser, or in-browser without a light wallet installed. To generate a key, you can use `cardano-cli` as follows:

```shell
$ cardano-cli address key-gen --normal-key --signing-key-file payment.skey --verification-key-file payment.vkey
```

The signing key can be loaded to CTL using `WalletSpec`'s `UseKeys` constructor. See `examples/Pkh2PkhKeyWallet.purs`.

From here you can submit transactions that will be signed with your private key, or perhaps export transactions to be tested with external tools such as [`plutip` testing tool](https://github.com/mlabs-haskell/plutip). We are currently working on integration with the plutip. These will be included in an upcoming release of CTL.

### With a light wallet

For full testing with browser-based light wallets, tools such as [`puppeteer`](https://github.com/puppeteer/puppeteer) or its [Purescript bindings](https://pursuit.purescript.org/packages/purescript-toppokki) might be useful.
