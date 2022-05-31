# Comparing CTL and Plutus

This document outlines the core differences between the APIs of CTL and Plutus code (particularly in the context of the Plutus Application Backend [PAB]). CTL is of course directly inspired by Plutus and PAB and we have attempted to preserve a high degree of similarity between the two APIs. In many cases, it should be possible to copy-paste existing code written for PAB deployments and adjust it for CTL fairly easily (accounting of course for syntactic differences between Haskell and Purescript). Nevertheless, CTL and Plutus differ in several important ways, as outlined below.

Note that differences between Haskell and Purescript, while also relevant to such a comparison, is beyond the scope of this document unless such differences have a direct bearing on divergences between the two APIs.

## The `Contract` type

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

CTL also has no concept of a "schema" for `Contract`s as there is no equivalent of an endpoint as in PAB. That is, effects written in `Contract` are not activated in some way and can instead be called normally from Purescript code. Note that despite the similar appearance of the kind signatures above, where `Row Type` appears in the signature of both `Contract`s, they are practically and conceptually unrelated.

For library users, CTL's `Contract` is less opaque than Plutus'. The `Contract` newtype can be unwrapped to expose the inner monad transformer stack, whose internal structure will be immediately recognizable to developers familiar `transformers`-style stacks. `Contract` also has instances for various typeclasses, making it possible to write `mtl`-style effects.

In contrast to the free-monad approach used by Plutus, CTL's `Contract` has uses a standard monadic eliminator to execute its effects (`Contract.Monad.runContract`), keeping with its more conventional transformer-oriented structure.

Finally, CTL's `Contract` is not parameterized by an error type as in Plutus. `Contract` actions defined by the library signal failure in various ways (e.g. returning `Either`s, `Maybe`s, or in the case of unrecoverable errors, throwing exceptions). Standardizing our error-handling approach is on our roadmap for future releases, however, most likely with a standardized error type containing polymorphic variants so that users may freely extend existing errors in the `Contract` interface.

## API differences

### Constraints and lookups

CTL has adapted Plutus' constraints/lookups interface fairly closely and it functions largely the same. One key difference is that CTL does not, and cannot, have the notion of a "current" script. All scripts must be explicitly provided to CTL (serialized as CBOR, see below). This has led us to depart from Plutus' naming conventions for certain constraints/lookups:

| Plutus                 | CTL               |
| ---------------------- | ----------------- |
| `mustPayToTheScript`   | _removed_         |
| `mustPayToOtherScript` | `mustPayToScript` |
| `otherScript`          | `validator`       |
| `otherData`            | `datum`           |
|                        |                   |

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
class
  ( DatumType a b
  , RedeemerType a b
  ) <=
  ValidatorTypes (a :: Type) (b :: Type)
  | a -> b

class DatumType (a :: Type) (b :: Type) | a -> b

class RedeemerType (a :: Type) (b :: Type) | a -> b
```

## Working with scripts

### Using scripts from the frontend

As noted above, all scripts and various script newtypes (`Validator`, `MintingPolicy`, etc...) must be explicitly passed to CTL. Unlike Plutus, where on- and off-chain code can freely share Haskell values, scripts must be provided to CTL in a serialized format. The easiest way to do this is via `Contract.Scripts.PlutusScript`'s `DecodeAeson`, instance which decodes the script as JSON. Note that this method is offered solely for convenience; it merely converts a hexadecimal string into `CborBytes` (a `ByteArray` that represents a value encoded as CBOR), which could also be achieved manually.

Using JSON is probably the simplest way of providing scripts to your CTL contracts. Two possible ways of achieving this:

- Storing the script JSON as part of your build configuration, to be read from disk upon application startup
- Embedding the scripts into a Purescript module directly, using the JS FFI. For example:

  ```purescript
  mintingPolicy :: Either JsonDecodeError MintingPolicy
  mintingPolicy = wrap <$> decodeAeson _mintingPolicy

  foreign import _mintingPolicy :: Aeson
  ```

  The corresponding FFI module would contain:

  ```js
  exports._mintingPolicy = "deadbeef";
  ```

  As shown above, such embedded scripts can be decoded as JSON for greater type safety, rather than attempting to pass types directly across the FFI boundary (which performs no validity checks)

### Applying arguments to parameterized scripts

CTL is currently unable to build full UPLC ASTs on the frontend (although support for this may be added in the future). This means that Plutus' `applyCode`, which is the default method for applying arguments to parameterized scripts, has no direct equivalent in CTL. We do, however, support a workaround for applying arguments to parameterized scripts. `Contract.Scripts.applyArgs` allows you to apply a list of `PlutusData` arguments to any type isomorphic to a `PlutusScript`. Using this allows you to dynamically apply arguments during contract execution, but also implies the following:

- `applyArgs` must be effectful, as we use our Haskell server to do the actual script application
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
