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

## Constraints and lookups

CTL has adapted Plutus' constraints/lookups interface fairly closely.
