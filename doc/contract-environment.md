<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [`Contract` environment](#contract-environment)
  - [`ContractEnv` management](#contractenv-management)
    - [Following the bracket pattern](#following-the-bracket-pattern)
    - [Manually passing the environment](#manually-passing-the-environment)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# `Contract` environment

CTL environment (`ContractEnv` type) consists of:

- various settings
- network protocol parameters (fetched once on initialization)
- active Ogmios `WebSocket` (optional)
- CIP-30 wallet connection (optional)
- internal state used by [synchronization primitives](./query-layers.md)

## `ContractEnv` management

Initialization is a costly operation, and having multiple distinct runtimes may lead to problems with state synchronization and excessive `WebSocket` use, so it is recommended to use only one runtime at any point in time.

If only one `Contract` is ever executed, just using `runContract` is perfectly fine. Otherwise, there are two better approaches:

### Following the bracket pattern

[Bracket pattern](https://wiki.haskell.org/Bracket_pattern) in functional programming world is commonly used to safely manage resources. In our case, `withContractEnv` should be used to initialize and finalize a `ContractEnv`. `runContractEnv` should be used to run a `Contract` within the environment:

```purescript
withContractEnv :: forall (a :: Type). ContractParams -> (ContractEnv -> Aff a) -> Aff a
runContractInEnv :: forall (a :: Type). ContractEnv -> Contract a -> Aff a

myContract1 :: Contract Uni
myContract2 :: Contract Uni

myRunner :: ContractParams -> Aff Unit
myRunner params = withContractEnv params \env -> do
  runContractInEnv env myContract1
  runContractInEnv env myContract2
```

Using bracket functions is safe, but is not always convenient, because `withContractEnv` callback must hold the `Aff` context until all `Contract`s exit.

### Manually passing the environment

Another approach is using `mkContractEnv` coupled with `runContractInEnv` and `stopContractEnv`.

Here's an example:

```purescript
mkContractEnv :: ContractParams -> Aff ContractEnv
stopContractEnv :: ContractEnv -> Aff Unit

myContract1 :: Contract Uni
myContract2 :: Contract Uni

myRunner :: ContractParams -> Aff Unit
myRunner params = do
  env <- mkContractEnv
  void $ try do
    runContractInEnv env myContract1
    runContractInEnv env myContract2
  stopContractEnv env
```

This approach is less safe in general, and it's fairly easy to hit the max WebSocket connections limit (which is 200 for Firefox) due to a forgotten `stopContractEnv` call (e.g. due to an exception), not to mention that any websocket that is not closed will force the server to also keep the connection.

This approach, however, is better suited for use when creating [custom JavaScript SDKs](./using-from-js.md).

## See also

- [Using CTL from JS](./using-from-js.md)
- [CTL runtime](./runtime.md)
