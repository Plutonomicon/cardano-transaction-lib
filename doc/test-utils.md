<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [CTL utilities for testing](#ctl-utilities-for-testing)
  - [Assertions](#assertions)
  - [Checks](#checks)
  - [Examples](#examples)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## CTL utilities for testing

`Contract.Test.Assert` module provides a DSL for assertions that accumulate error messages, instead of exiting early after the first failure.

There are two kinds of tests:

- `ContractAssertion` type: assertions that can raise recoverable errors with `tellFailure`

- `ContractCheck` type: Checks can inspect the state both before and after `Contract` execution, allowing to monitor for effects, e.g. monetary gains/losses at address

`runChecks` function accepts an array of checks and a `Contract` action to run, lifted to `ContractAssertion` to allow for assertion accumulation (lifting can be done with `Control.Monad.Trans.Class.lift`):

```purescript
runChecks
  :: forall (a :: Type)
   . Array (ContractCheck a)
  -> ContractAssertion a
  -> Contract a
```

### Assertions

To convert an assertion into a `ContractCheck`, `Contract.Test.Assert.assertionToCheck` can be used:

```purescript
assertionToCheck
  :: forall (a :: Type)
   . String
  -> (a -> ContractAssertion Unit)
  -> ContractCheck a
```

The first argument is a descriptive message that will be printed in case there is an exception thrown inside the `Contract` that is being tested. This is done because in case of an exception there's no way to get the result value of type `a`. For better developer experience, all the tests skipped due to the exception will still be mentioned in the report, e.g. [for this example](../examples/ContractTestUtils.purs):

```
  ✗ Examples.ContractTestUtils:

  Error: An exception has been thrown:

Error: (some error message)
    at Object.exports.error (/home/me/c/cardano-transaction-lib/output/Effect.Exception/foreign.js:8:10)
    at Object.$$throw [as throw] (/home/me/c/cardano-transaction-lib/output/Effect.Exception/index.js:18:45)
    at /home/me/c/cardano-transaction-lib/output/Ctl.Examples.ContractTestUtils/index.js:131:506
    at /home/me/c/cardano-transaction-lib/output/Control.Monad.Reader.Trans/index.js:108:34
    at run (/home/me/c/cardano-transaction-lib/output/Effect.Aff/foreign.js:278:22)
    at /home/me/c/cardano-transaction-lib/output/Effect.Aff/foreign.js:348:19
    at drain (/home/me/c/cardano-transaction-lib/output/Effect.Aff/foreign.js:120:9)
    at Object.enqueue (/home/me/c/cardano-transaction-lib/output/Effect.Aff/foreign.js:141:11)
    at /home/me/c/cardano-transaction-lib/output/Effect.Aff/foreign.js:339:27
    at /home/me/c/cardano-transaction-lib/output/Effect.Aff.Compat/index.js:18:55

The following `Contract` checks have been skipped due to an exception:

    1. Sender's output has a datum

    2. Output has a reference script

    3. Contains CIP-25 metadata
```

### Checks

`ContractCheck` is defined as follows:

```
type ContractCheck a =
  ContractAssertion a
    -> ContractAssertion (ContractAssertion a /\ ContractAssertion Unit)
```

- The argument is the `Contract` to be tested itself
- The outer `ContractAssertion` can perform effects to initialize internal assertion state (e.g. creation of `Ref`s that can be used by actions in the tuple)
- The first component of the tuple is the `Contract` that is supposed to be executed. It can be modified in the outer `ContractAssertion` action, but most checks would simply return it unchanged.
- The second component of the tuple is a finalization action, that is executed after the first component is run, regardless of whether the result of execution was success or an exception. This allows to implement checks that inspect the state change before and after, as well as the mechanism of skipped assertions reporting.

### Examples

Particular values can be constructed with utility functions, as demonstrated in the [ContractTestUtils example](../examples/ContractTestUtils.purs) (see `mkAssertions`).

All the functions require `Labeled` arguments, that can be constructed with `label` function; or `noLabel`, if descriptive names in error messages are not needed.
