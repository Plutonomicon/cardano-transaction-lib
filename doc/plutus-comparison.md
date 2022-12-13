# Comparing CTL and Plutus

This document outlines the core differences between CTL and Plutus (particularly in the context of the Plutus Application Backend [PAB]). CTL is of course directly inspired by Plutus and PAB and we have attempted to preserve a high degree of similarity between the two APIs. In many cases, it should be possible to copy-paste existing code written for PAB deployments and adjust it for CTL fairly easily (accounting of course for existing differences between Haskell and Purescript). Nevertheless, CTL and Plutus differ in several important ways, as outlined below.

Note that differences between Haskell and Purescript, while also relevant to such a comparison, is beyond the scope of this document unless such differences have a direct bearing on divergences between the two CTL and Plutus.

**Table of Contents**

- [Core conceptual differences](#core-conceptual-differences)
  + [Library vs. process](#library-vs-process)
  + [The `Contract` type](#the--contract--type)
- [API differences](#api-differences)
  + [Constraints and lookups](#constraints-and-lookups)
    - [Babbage-era constraints](#babbage-era-constraints)
  + [Typed scripts](#typed-scripts)
  + [Working with scripts](#working-with-scripts)
    - [Using scripts from the frontend](#using-scripts-from-the-frontend)
    - [Applying arguments to parameterized scripts](#applying-arguments-to-parameterized-scripts)

## Core conceptual differences

### Library vs. process

Unlike contracts written for PAB, which are compiled to a single process, CTL is a library. CTL itself can be [imported as a Purescript library](./ctl-as-dependency.md) and contracts written in CTL compile to Javascript that can be run in the browser or NodeJS. Accordingly, there is no need to activate endpoints in CTL -- contracts are executed by calling effectful functions written using the library. This distinction has influenced our adaption of Plutus' `Contract` type, as outlined [below](#the-contract-type).

Note, however, that CTL still requires a number of runtime dependencies. In some respects, this is similar to PAB, which also needs to communicate with plutus-chain-index and a running node. Please see the [documentation](./runtime.md) for more details on CTL's runtime.

### The `Contract` type

Both CTL and Plutus define `Contract` monads for constructing, balancing, and submitting transactions. There are considerable differences between the two, however:

**CTL**:

```purescript
newtype Contract (r :: Row Type) (a :: Type) = Contract (QueryMExtended r a)
```

where

- `QueryMExtended` is an internal monad transformer stack based on `ReaderT` over `Aff` (Purescript's monad for asynchronous effects, with no Haskell analogue).
- `r` extends the record serving as the reader environment

**Plutus**:

```haskell
newtype Contract (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) = Contract { ... }
```

where

- `(w :: Type)` is usually some `Monoid` used for `Writer`-like effects
- `(s :: Row Type)` (note that `Row` comes from the `row-types` package) represents the contract schema
- `(e :: Type)` represents the errors that the contract can throw

As this direct comparison illustrates, CTL's `Contract` is significantly simpler than Plutus'. Importantly, CTL's `Contract` **allows for arbitrary effects**. This makes `Writer` capabilities redundant in CTL, for instance, as all communication between `Contract`s can be done in a more direct manner (e.g. logging or HTTP calls).

CTL also has no concept of a "schema" for `Contract`s as there is no equivalent of an endpoint as in PAB. That is, effects written in `Contract` are not activated in some way and can instead be called normally from Purescript code. Note that despite the similar appearance of the kind signatures above, where `Row Type` appears in the signature of both `Contract`s, they are practically and conceptually unrelated. The extensible record contained in CTL's `Contract` allows users to easily extend the reader environment in which their contracts execute. For instance, you may wish to expose some global state across all of your contracts, perhaps a unique `CurrencySymbol` that will be created in one contract and read by several others. Instead of threading this state throughout as parameters, you could use a `Ref (Maybe CurrencySymbol)` (`Ref`s are analogous to Haskell's `IORef`) and write your contracts with types signatures similar to `forall (r :: Row Type). Contract (cs :: Ref (Maybe CurrencySymbol) | r) Unit`. `cs` would then be accessible from your contracts using normal `MonadReader` methods (or more precisely those of `MonadAsk` in the case of Purescript).

For library users, CTL's `Contract` is less opaque than Plutus'. The `Contract` newtype can be unwrapped to expose the inner monad transformer stack, whose internal structure will be immediately recognizable to developers familiar `transformers`-style stacks. `Contract` also has instances for various typeclasses, making it possible to write `mtl`-style effects.

In contrast to the free-monad approach used by Plutus, CTL's `Contract` has uses a standard monadic eliminator to execute its effects (`Contract.Monad.runContract`), keeping with its more conventional transformer-oriented structure.

Finally, CTL's `Contract` is not parameterized by an error type as in Plutus. `Contract` actions defined by the library signal failure in various ways (e.g. returning `Either`s, `Maybe`s, or in the case of unrecoverable errors, throwing exceptions). Standardizing our error-handling approach is on our roadmap for future releases, however, most likely with a standardized error type containing polymorphic variants so that users may freely extend existing errors in the `Contract` interface.

## API differences

### Constraints and lookups

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

#### Babbage-era constraints

CIPs 0031-0033 brought several improvements to Plutus and are supported from the Babbage era onwards:

- [Reference inputs](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0031)
- [Inline datums](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0032)
- [Reference scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033)

CTL has upgraded its constraints interface to work with these new features. At the time of writing, however, `plutus-apps` has not yet upgraded their constraints/lookups interface to support these new features. This means a direct comparison between `plutus-apps` and CTL regarding Babbage-era features is not currently possible. It also implies that, moving forward, CTL's constraints implementation will increasingly no longer match that of `plutus-apps`' to the same degree.

### Typed scripts

Another difference between Plutus and CTL is our implementation of typed scripts. Recall that Plutus' `ValidatorTypes` class:

```haskell
class ValidatorTypes (a :: Type) where
    type RedeemerType a :: Type
    type DatumType a :: Type

    type instance RedeemerType a = ()
    type instance DatumType  a = ()
```

Purescript lacks most of Haskell's more advanced type-level faculties, including type/data families. Purescript does, however, support functional dependencies, allowing us to encode `ValidatorTypes` as follows:

```purescript
class ValidatorTypes :: Type -> Type -> Type -> Constraint
class
  ( DatumType validator datum
  , RedeemerType validator redeemer
  ) <=
  ValidatorTypes validator datum redeemer

class DatumType :: Type -> Type -> Constraint
class DatumType validator datum | validator -> datum

class RedeemerType :: Type -> Type -> Constraint
class RedeemerType validator redeemer | validator -> redeemer
```

### Working with scripts

#### Using scripts from the frontend

As noted above, all scripts and various script newtypes (`Validator`, `MintingPolicy`, etc...) must be explicitly passed to CTL. Unlike Plutus, where on- and off-chain code can freely share Haskell values, scripts must be provided to CTL in a serialized format. The easiest way to do this is using `Contract.TextEnvelope.textEnvelope` along with the JS FFI. See the [getting started guide](getting-started.md#using-compiled-scripts) for more details.

#### Applying arguments to parameterized scripts

We support applying arguments to parameterized scripts with `Contract.Scripts.applyArgs`. It allows you to apply a list of `PlutusData` arguments to any type isomorphic to a `PlutusScript`. Using this allows you to dynamically apply arguments during contract execution, but also implies the following:

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
