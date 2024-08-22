# Comparing CTL and Plutus

This document outlines the core differences between CTL and Plutus Application Backend. CTL is directly inspired by PAB and we have attempted to preserve a high degree of similarity between the two APIs. Nevertheless, CTL and Plutus differ in several important ways, as outlined below.

**Table of Contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Core conceptual differences](#core-conceptual-differences)
  - [Library vs. process](#library-vs-process)
  - [The `Contract` type](#the-contract-type)
- [API differences](#api-differences)
  - [DEPRECATION WARNING](#deprecation-warning)
  - [**DEPRECATED** Transaction manipulation API](#deprecated-transaction-manipulation-api)
  - [**DEPRECATED** Constraints and lookups](#deprecated-constraints-and-lookups)
    - [**DEPRECATED** Babbage-era constraints](#deprecated-babbage-era-constraints)
  - [Working with scripts](#working-with-scripts)
    - [Using scripts from the frontend](#using-scripts-from-the-frontend)
    - [Applying arguments to parameterized scripts](#applying-arguments-to-parameterized-scripts)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->
## Core conceptual differences

### Library vs. process

Unlike contracts written for PAB, which are compiled to a single process, CTL is a library. CTL itself can be [imported as a Purescript library](./ctl-as-dependency.md) and offchain contracts written in CTL compile to Javascript that can be run in the browser or NodeJS. Accordingly, there is no need to activate endpoints in CTL -- contracts are executed by calling effectful functions written using the library. This distinction has influenced our adaption of Plutus' `Contract` type, as outlined [below](#the-contract-type).

Note, however, that CTL still requires a number of runtime dependencies. In some respects, this is similar to PAB, which also needs to communicate with plutus-chain-index and a running node. Please see the [runtime documentation](./runtime.md) for more details.

### The `Contract` type

Both CTL and Plutus define `Contract` monads for constructing, balancing, and submitting transactions (not to be confused with smart contracts). There are considerable differences between the two, however:

**CTL**:

```purescript
newtype Contract (a :: Type) = Contract (ReaderT ContractEnv Aff a)
```

where

- `ContractEnv` is an internal type containing references to various backend services, configurations and ledger constants
- `Aff` (Purescript's monad for asynchronous effects, with no Haskell analogue, the closest being IO).

**Plutus**:

```haskell
newtype Contract (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) = Contract { ... }
```

where

- `(w :: Type)` is usually some `Monoid` used for `Writer`-like effects
- `(s :: Row Type)` (note that `Row` comes from the `row-types` package) represents the contract schema
- `(e :: Type)` represents the errors that the contract can throw

As this direct comparison illustrates, CTL's `Contract` is significantly simpler than Plutus'. Importantly, CTL's `Contract` **allows for arbitrary effects**. This makes `Writer` capabilities redundant in CTL, for instance, as all communication between `Contract`s can be done in a more direct manner (e.g. logging or HTTP calls).

CTL also has no concept of a "schema" for `Contract`s as there is no equivalent of an endpoint as in PAB. That is, effects written in `Contract` are not activated in some way and can instead be called normally from Purescript code.

For library users, CTL's `Contract` is less opaque than Plutus'. The `Contract` newtype can be unwrapped to expose the inner monad transformer stack, whose internal structure will be immediately recognizable to developers familiar `transformers`-style stacks. `Contract` also has instances for various typeclasses, making it possible to write `mtl`-style effects.

In contrast to the free-monad approach used by Plutus, CTL's `Contract` has uses a standard monadic eliminator to execute its effects (`Contract.Monad.runContract`), keeping with its more conventional transformer-oriented structure.

Finally, CTL's `Contract` is not parameterized by an error type as in Plutus. `Contract` actions defined by the library signal failure in various ways (e.g. returning `Either`s, `Maybe`s, or in the case of unrecoverable errors, throwing exceptions). Standardizing our error-handling approach is on our roadmap for future releases, however, most likely with a standardized error type containing polymorphic variants so that users may freely extend existing errors in the `Contract` interface.

## API differences

### DEPRECATION WARNING

The original constraints interface has been deprecated and will be removed. Use [`cardano-transaction-builder`](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder) for any new code.

### **DEPRECATED** Transaction manipulation API

| Plutus                      | CTL                           |
| --------------------------- | ----------------------------- |
| `submitTxConstraintsWith`   | `submitTxFromConstraints`     |

### **DEPRECATED** Constraints and lookups

CTL has adapted Plutus' Alonzo-era constraints/lookups interface fairly closely and it functions largely the same. One key difference is that CTL does not, and cannot, have the notion of a "current" script. All scripts must be explicitly provided to CTL (serialized as CBOR, see below). This has led us to depart from Plutus' naming conventions for certain constraints/lookups:

| Plutus                 | CTL                           |
| ---------------------- | ----------------------------- |
| `mustPayToTheScript`   | _removed_                     |
| `mustPayToOtherScript` | `mustPayToScript`             |
| `otherScript`          | `validator`                   |
| `otherData`            | `datum`                       |
| _none_                 | `mustPayToNativeScript`       |
| _none_                 | `mustSpendNativeScriptOutput` |

Additionally, we implement `NativeScript` (multi-signature phase-1 script) support, which is not covered by Plutus.

#### **DEPRECATED** Babbage-era constraints

CIPs 0031-0033 brought several improvements to Plutus and are supported from the Babbage era onwards:

- [Reference inputs](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0031)
- [Inline datums](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0032)
- [Reference scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033)

CTL has upgraded its constraints interface to work with these new features. At the time of writing, however, `plutus-apps` has not yet upgraded their constraints/lookups interface to support these new features. This means a direct comparison between `plutus-apps` and CTL regarding Babbage-era features is not currently possible. It also implies that, moving forward, CTL's constraints implementation will increasingly no longer match that of `plutus-apps`' to the same degree.

### Working with scripts

#### Using scripts from the frontend

As noted above, all scripts and various script newtypes (`Validator`, `MintingPolicy`, etc...) must be explicitly passed to CTL. Unlike Plutus, where on- and off-chain code can freely share Haskell values, scripts must be provided to CTL in a serialized format. The easiest way to do this is using `Contract.TextEnvelope` along with the JS FFI. See the [getting started guide](getting-started.md#using-compiled-scripts) for more details.

#### Applying arguments to parameterized scripts

We support applying arguments to parameterized scripts with `Cardano.Plutus.ApplyArgs.applyArgs` (from [purescript-uplc-apply-args](https://github.com/mlabs-haskell/purescript-uplc-apply-args)). It allows you to apply a list of `PlutusData` arguments to a `PlutusScript`. Using this allows you to dynamically apply arguments during contract execution, but also implies the following:

- All of your domain types must have `Contract.PlutusData.ToData` instances (or some other way of converting them to `PlutusData`)
- You must employ a workaround, illustrated by the following examples, in your off-chain code to ensure that the applied scripts are valid for both on- and off-chain code. This essentially consists of creating an wrapper which accepts `Data` arguments for your parameterized scripts:

  - PlutusTx:

    ```haskell
    mkTestValidator :: Integer -> BuiltinData -> BuiltinData -> BuiltinData -> ()
    mkTestValidator _ _ _ _ = ()

    -- This is the wrapper function
    mkTestValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
    mkTestValidatorUntyped p =
      mkTestValidator
        (unsafeFromBuiltinData p)

    testScript :: Script
    testScript =
      fromCompiledCode
      $$(PlutusTx.compile [|| mkTestValidatorUntyped ||])

    testValidator :: Integer -> Validator
    testValidator params =
      mkValidatorScript
      -- `toBuiltinData` is redundant here but it makes the signature match
      ($$(PlutusTx.compile [|| mkTestValidatorUntyped ||]) `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData params))
    ```

  - Plutarch:

    ```haskell
    mkTestValidator :: Term s (PInteger :--> PData :--> PData :--> PScriptContext :--> POpaque)
    mkTestValidator = plam $ \_i _datm _redm _ctx -> popaque $ pconstant ()

    mkTestValidatorUntyped :: Term s (PData :--> PData :--> PData :--> PScriptContext :--> POpaque)
    mkTestValidatorUntyped = plam $ \iData -> ptryFrom @(PAsData PInteger) iData $ \(_, i) -> mkTestValidator # i

    testScript :: Script
    testScript = compile mkTestValidatorUntyped

    testValidator :: Integer -> Validator
    testValidator i = mkValidator $ mkTestValidator # pconstant i

    ```
