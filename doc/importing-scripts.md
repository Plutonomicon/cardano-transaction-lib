# Importing scripts for use with CTL

**Table of Contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Exporting scripts from Plutus or Plutarch](#exporting-scripts-from-plutus-or-plutarch)
  - [Using Plutonomy](#using-plutonomy)
- [Serializing Plutus scripts](#serializing-plutus-scripts)
  - [PlutusTx](#plutustx)
  - [Plutarch](#plutarch)
  - [plutarch-ctl-bridge](#plutarch-ctl-bridge)
- [Importing serialized scripts](#importing-serialized-scripts)

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

## Importing serialized scripts

To use your own scripts, compile them to any subdirectory in the root of your project and either dynamically load them from file (NodeJS) or use your bundler to include them as fixtures into the bundle ([instructions for WebPack](https://webpack.js.org/guides/asset-modules/), [for esbuild](https://esbuild.github.io/content-types/#external-file)).
