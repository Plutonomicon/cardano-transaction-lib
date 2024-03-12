# Importing scripts for use with CTL

**Table of Contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Exporting scripts from Plutus or Plutarch](#exporting-scripts-from-plutus-or-plutarch)
  - [Using Plutonomy](#using-plutonomy)
- [Importing serialized scripts](#importing-serialized-scripts)
- [Serializing Plutus scripts](#serializing-plutus-scripts)
  - [PlutusTx](#plutustx)
  - [Plutarch](#plutarch)
  - [plutarch-ctl-bridge](#plutarch-ctl-bridge)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Exporting scripts from Plutus or Plutarch

Usually projects use a Haskell binary called *exporter* that outputs a pre-compiler UPLC bundle into a file.

The output file should be a Cardano envelope:

```json
{
    "cborHex": "4e4d01000033222220051200120011",
    "description": "always-succeeds",
    "type": "PlutusScriptV2"
}
```

- An example of a Plutus exporter can be found [here](https://github.com/Mr-Andersen/ctl-multisign-mre/blob/main/onchain/exporter/Main.hs).
- For Plutarch, see [the-plutus-scaffold](https://github.com/mlabs-haskell/the-plutus-scaffold)'s [exporter](https://github.com/mlabs-haskell/the-plutus-scaffold/tree/main/onchain/exporter).


### Using Plutonomy

It makes sense to use [Plutonomy](https://github.com/well-typed/plutonomy) (an optimizer for UPLC) in the exporter:

```haskell
import Plutonomy (aggressiveOptimizerOptions, optimizeUPLCWith)

script :: Script
script = fromCompiledCode $
  optimizeUPLCWith aggressiveOptimizerOptions $$(PlutusTx.compile [||policy||])
```

## Importing serialized scripts

To use your own scripts, compile them to any subdirectory in the root of your project (where `webpack.config.js` is located) and add a relative path to `webpack.config.js` under the `resolve.alias` section. In CTL, we have the `Scripts` alias for this purpose. Note the capitalization of `Scripts`: it is necessary to disambiguate it from local folders.

First, in your `webpack.config.js`, define an `alias` under `module.exports.resolve.alias` in order to `require` the compiled scripts from JS modules:

```javascript
const path = require("path");

module.exports = {
  // ...
  resolve: {
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

parseValidator :: Contract Validator
parseValidator = liftMaybe (error "Error decoding myscript") do
    envelope <- decodeTextEnvelope myscript
    Validator <$> Contract.TextEnvelope.plutusScriptV1FromEnvelope envelope
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

## Serializing Plutus scripts

### PlutusTx

Take `alwaysSucceeds` as an example:
```haskell
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(PlutusTx.compile [|| alwaysSucceeds ||])
```
For older plutus commits use:
```haskell
import Plutus.V1.Ledger.Api (fromCompiledCode)
import Codec.Serialise (serialise)

scriptToCBOR :: CompiledCode a -> ByteString
scriptToCBOR = B.toStrict . serialise . fromCompiledCode
```
and for newer (match on plutus-ledger-api import):
```haskell
import PlutusLedgerApi.Common (serialiseCompiledCode)
import Data.ByteString.Short (fromShort)

scriptToCBOR = Short.fromShort . serialiseCompiledCode
```
Then save a script to a file:
```haskell
import Cardano.Api (writeFileTextEnvelope, PlutusScriptV1, SerialiseAsCBOR (deserialiseFromCBOR), AsType (AsScript, AsPlutusScriptV1), Script)

main =
  case deserialiseFromCBOR (AsScript AsPlutusScriptV1) $ scriptToCBOR alwaysSucceedsCompiled of
    Left err -> print err
    Right script ->
      writeFileTextEnvelope @(Script PlutusScriptV1) "always-succeeds.plutus" (Just "My script") script
      >>= either print return
```
Note that we specified plutus version.

### Plutarch

You can use [`ply`](https://github.com/mlabs-haskell/ply) and [`ply-ctl`](https://github.com/mlabs-haskell/ply-ctl).

### plutarch-ctl-bridge

You can use [`plutarch-ctl-bridge`](https://github.com/mlabs-haskell/plutarch-ctl-bridge) to generate Purescript types from your Haskell type definitions and typed script wrappers from parametrized Plutarch scripts. See [example module](https://github.com/mlabs-haskell/plutarch-ctl-bridge/blob/main/example/Main.hs).
