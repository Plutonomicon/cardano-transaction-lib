# CTL integration with Cardano Testnet

**Table of Contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Overview](#overview)
- [Architecture](#architecture)
- [Testing contracts](#testing-contracts)
  - [Testing with Mote](#testing-with-mote)
    - [Using Mote testing interface](#using-mote-testing-interface)
    - [Internal implementation overview](#internal-implementation-overview)
  - [Testing in Aff context](#testing-in-aff-context)
  - [Sharing wallet state between tests](#sharing-wallet-state-between-tests)
  - [Writing checks in tests](#writing-checks-in-tests)
  - [Note on SIGINT](#note-on-sigint)
  - [Testing with Nix](#testing-with-nix)
- [Cluster configuration options](#cluster-configuration-options)
  - [Current limitations](#current-limitations)
- [Using addresses with staking key components](#using-addresses-with-staking-key-components)
- [Limitations](#limitations)
- [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Overview

[`cardano-testnet`](https://github.com/IntersectMBO/cardano-node/tree/master/cardano-testnet) is a tool for spinning up temporary local testnets. CTL integrates with it to provide an environment for `Contract` testing, that is very close to production.

## Architecture

CTL depends on a number of binaries in the `$PATH` to execute tests on the
Cardano Testnet:

- [`cardano-node`](https://github.com/IntersectMBO/cardano-node) to connect to the Cardano Testnet
- [`ogmios`](https://ogmios.dev/)
- [`kupo`](https://cardanosolutions.github.io/kupo/)

All of these are provided by CTL's `overlays.runtime` (and are provided in
CTL's own `devShell`). You **must** use the `runtime` overlay or otherwise make
the services available in your package set (e.g. by defining them within your
own `overlays` when instantiating `nixpkgs`) as `purescriptProject.runTest`
expects all of them; an example of using CTL's overlays is in the
[`ctl-scaffold` template](../templates/ctl-scaffold/flake.nix#L35).

The services are NOT run by `docker-compose` (via `arion`) as is the case with
`launchCtlRuntime`: instead, they are started and stopped on each CTL
`ContractTest` execution by CTL itself.

If you have based your project on the [`ctl-scaffold`
template](../templates/ctl-scaffold) then you have two options to run tests on
the Cardano Testnet:
1. `nix develop` followed by `npm run test` (recommended for development)
2. `nix run .#checks.x86_64-linux.ctl-scaffold-local-testnet-test`
   * where you'd usually replace `x86_64-linux` with the system you run tests on
   * and `ctl-scaffold-local-testnet-test` with the name of the test derivation for your project;

## Testing contracts

CTL provides integrated testing environment for Mote (a test framework in PureScript). Alternatively, testing in the `Aff` context is also available, which makes it possible to integrate with any testing framework (or none at all).

### Testing with Mote

[Mote](https://github.com/garyb/purescript-mote) is a DSL for defining and
grouping tests (plus other quality of life features, e.g. skipping marked
tests).

First (and more widely used) approach is to first build a tree of tests (in
CTL's case a tree of `ContractTest` types -- basically a function from some
distribution of funds to a `Contract a`) via Mote and then use the
`Contract.Test.Testnet.testTestnetContracts` function to execute them. This
allows setting up the Cardano Testnet only once per top-level groups and tests
passed to the `testTestnetContracts` and then use it in many independent tests.
The function will interpret a `MoteT` (effectful test tree) into `Aff`, which
you can then actually run.

The [`ctl-scaffold` template](../templates/ctl-scaffold) provides a simple
`Mote`-based example.

`Contract.Test.Testnet.testTestnetContracts` type is defined as follows:
```purescript
testTestnetContracts
  :: TestnetConfig
  -> TestPlanM ContractTest Unit
  -> TestPlanM (Aff Unit) Unit
```
It takes a configuration and a tree of Mote tests, where tests are of type
`ContractTest`.

To create tests of type `ContractTest`, you should either use
`Contract.Test.Testnet.withWallets` or `Contract.Test.Testnet.noWallet`:

```purescript
withWallets
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> (wallets -> Contract Unit)
  -> ContractTest

noWallet :: Contract Unit -> ContractTest
noWallet test = withWallets unit (const test)
```

Usage of `testTestnetContracts` is similar to that of `runTestnetContract`,
and distributions are handled in the same way. Here's an example:

```purescript
suite :: MoteT Aff (Aff Unit) Aff
suite = testTestnetContracts config do
  test "Test 1" do
    let
      distribution :: Array BigInt /\ Array BigInt
      distribution = ...
    withWallets distribution \(alice /\ bob) -> do
      ...

  test "Test 2" do
    let
      distribution :: Array BigInt
      distribution = ...
    withWallets distribution \alice -> do
      ...

  test "Test 3" do
    noWallet do
      ...
```

#### Using Mote testing interface

To define tests suites you can use `test`, group them with `group` and also
wrap tests or groups with `bracket` to execute custom actions before and
after tests/groups that are inside the bracket. Note that in Mote you can
define several tests and several groups in a single block, and bracket that
wraps them will be run for each such test or group.

Internally `testTestnetContracts` places a bracket that sets up the CTL
environment and connects to the Cardano Testnet on the top level, so if you
want to connect only once wrap your tests or groups in a single group. In
the example above the environment and Testnet setup will happen 3 times.

#### Internal implementation overview

`Contract.Test.Testnet.testTestnetContracts` type is defined as follows:

```purescript
type TestPlanM :: Type -> Type -> Type
type TestPlanM test a = MoteT Aff test Aff a

testTestnetContracts
  :: TestnetConfig
  -> TestPlanM ContractTest Unit
  -> TestPlanM (Aff Unit) Unit

```
where
* `test :: Type` is a type of tests themselves,
  * in our case it's [`ContractTest`](../src/Internal/Test/ContractTest.purs),
    which in a nutshell describes a function from some wallet UTxO distribution to
    a `Contract r`
  * wallet UTxO distribution is the one that you need to pattern-match on when
    writing tests
* `m :: Type -> Type` is a monad where effects during the construction of the
  test suite can be performed,
  * here we use `Aff` again
* `a :: Type` is a result of the test suite, we use `Unit` here.

`testTestnetContracts` also combines ADA distribution requirements of individual tests in a single ADA distribution requirement. This allows to create multiple wallets and
fund them in one step, during the Testnet setup. See the comments in the
[`Ctl.Internal.Testnet.Server` module](../src/Internal/Testnet/Contract.purs) for
more info.

### Testing in Aff context

If using Mote is not desired, it's possible to use the `Contract.Test.Testnet.runTestnetContract` function,
which takes a single `Contract`, connects to the Testnet and executes the passed
contract. This function runs in `Aff`; it will also throw an exception should
contract fail for any reason. The testnet is terminated after `Contract` execution.

You can either call this function directly from your test's `main` or use any
library for grouping and describing tests which support `Aff` effects in test bodies.

`Contract.Test.Testnet.runTestnetContract`'s type is defined as follows:

```purescript
runTestnetContract
  :: forall (distr :: Type) (wallets :: Type) (a :: Type)
   . UtxoDistribution distr wallets
  => TestnetConfig
  -> distr
  -> (wallets -> Contract a)
  -> Aff a
```

`distr` is a specification of how many wallets and with how much funds should be
created. It should either be a `Unit` (for no wallets), nested tuples containing
`Array BigInt` or an `Array (Array BigInt)`, where each element of the inner array
specifies an UTxO amount in Lovelaces (0.000001 Ada).

The `wallets` argument of the callback is either a `Unit`, a tuple of `KeyWallet`s
(with the same nesting level as in `distr`, which is guaranteed by
`UtxoDistribution`) or an `Array KeyWallet`.

`wallets` should be pattern-matched on, and its components should be passed to
`withKeyWallet`:

An example `Contract` with two actors using nested tuples:

```purescript
let
  distribution :: Array BigInt /\ Array BigInt
  distribution =
    [ BigNum.fromInt 1_000_000_000
    , BigNum.fromInt 2_000_000_000
    ] /\
      [ BigNum.fromInt 2_000_000_000 ]
runTestnetContract config distribution \(alice /\ bob) -> do
  withKeyWallet alice do
    pure unit -- sign, balance, submit, etc.
  withKeyWallet bob do
    pure unit -- sign, balance, submit, etc.
```

An example `Contract` with two actors using `Array`:

```purescript
let
  distribution :: Array (Array BigInt)
  distribution =
    -- wallet one: two UTxOs
    [ [ BigNum.fromInt 1_000_000_000, BigNum.fromInt 2_000_000_000]
    -- wallet two: one UTxO
    , [ BigNum.fromInt 2_000_000_000 ]
    ]
runTestnetContract config distribution \wallets -> do
  traverse_ ( \wallet -> do
                withKeyWallet wallet do
                  pure unit -- sign, balance, submit, etc.
            )
            wallets
```

In most cases at least two UTxOs per wallet are needed (one of which will be used
as collateral, so it should exceed `5_000_000` Lovelace).

Internally `runTestnetContract` runs a contract in an `Aff.bracket`, which creates
a Testnet setup on setup and terminates it during the shutdown or in case of an
exception. Logs will be printed in case of an error.

### Sharing wallet state between tests

To execute tests that share the same wallet state, the use of
`Contract.Test.Testnet.runTestnetTestPlan` is suggested, which has a type of:

```purescript
runTestnetTestPlan
  :: TestnetConfig
  -> ContractTestPlan
  -> TestPlanM (Aff Unit) Unit
```

`runTestnetTestPlan` uses the exact same logic as `testTestnetContracts`, except it
requires that wallets are pre-allocated inside of the second parameter, which has a
type of `ContractTestPlan`. `Contract.Test.Testnet.sameWallets` is a helper
function that can be used to create a `ContractTestPlan` where all of the tests use
the same wallets that are defined in the `UtxoDistribution`, this function has a
type of:

```purescript
sameWallets
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> TestPlanM (wallets -> Contract Unit) Unit
  -> ContractTestPlan
```

Usage of `runTestnetTestPlan` is similar to that of `testTestnetContracts`, except
that the distributions are handled slightly differently. Here's an example of
using `sameWallets`:

```purescript
suite :: TestPlanM (Aff Unit) Unit
suite = runTestnetTestPlan config do
  let
    distribution :: Array BigInt /\ Array BigInt
    distribution = ...

  sameWallets distribution $
    group "Test Plan" do
      test "Test 1"  \(alice /\ bob /\ charlie) -> do
          ...

      test "Test 2"  \(alice /\ bob /\ charlie) -> do
          ...

      test "Test 3" \(alice /\ bob /\ charlie) ->  do
          ...
```

Another example for using `sameWallets` can be found [here](../test/Testnet/SameWallets.purs).

### Writing checks in tests

CTL will run contracts in your test bodies and will print errors for any failed
tests. For more complex checks you can use the [assertions library](./test-utils.md).

### Note on SIGINT

Due to `testTestnetContracts`/`runTestnetContract` adding listeners to the SIGINT
IPC signal, Node.js's default behavior of exiting on CTRL+C no longer occurs. This
was done to let cluster cleanup handlers run asynchronously. To restore the usual
exit-by-CTRL+C, we provide helpers to cancel an `Aff` fiber and set the exit code,
to let Node.js shut down gracefully when no more events are to be processed.

```purescript
...
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Data.Posix.Signal (Signal(SIGINT))
import Effect.Aff (cancelWith, effectCanceler, launchAff)

main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    ... test suite in Aff ...
```

### Testing with Nix

You can run Testnet tests via CTL's `purescriptProject` as well. After creating
your project, you can use the `runLocalTestnetTest` attribute to create a Testnet testing
environment that is suitable for use with your flake's `checks`. An example:

```nix
{
  some-testnet-test = project.runLocalTestnetTest {
    name = "some-testnet-test";
    testMain = "Test.MyProject.Testnet";
    # The rest of the arguments are passed through to `runPursTest`:
    env = { SOME_ENV_VAR = "${some-value}"; };
  };
}
```

The usual approach is to put `projectname-testnet-test` in the `checks` attribute of
your project's `flake.nix`. This is done by default in the
[`ctl-scaffold` template](../templates/ctl-scaffold/flake.nix).

## Cluster configuration options

`TestnetConfig` type contains `clusterConfig` record with the following options:

```purescript
  { testnetMagic :: Int
  , era :: Era
  , slotLength :: Seconds
  , epochSize :: Maybe UInt
  }
```

- `slotLength` and `epochSize` define time-related protocol parameters. Epoch size
  is specified in slots.

### Current limitations

* Non-default values of `epochSize` (current default is 80) break staking rewards -
  see [this issue](https://github.com/mlabs-haskell/plutip/issues/149) for more info.
  `slotLength` can be changed without any problems.

## Using addresses with staking key components

It's possible to use stake keys with the Testnet. `Contract.Test.Testnet.withStakeKey`
function can be used to modify the distribution spec:

```purescript
let
  privateStakeKey :: PrivateStakeKey
  privateStakeKey = wrap $ unsafePartial $ fromJust
    $ privateKeyFromBytes =<< hexToRawBytes
      "633b1c4c4a075a538d37e062c1ed0706d3f0a94b013708e8f5ab0a0ca1df163d"
  aliceUtxos =
    [ BigNum.fromInt 2_000_000_000
    , BigNum.fromInt 2_000_000_000
    ]
  distribution = withStakeKey privateStakeKey aliceUtxos
```

Although stake keys serve no real purpose in the Testnet context, they allow the
use of base addresses, and thus allow the same code for Testnet testing, in-browser
tests, and production.

Note that CTL re-distributes tADA from payment key-only ("enterprise") addresses to
base addresses, which requires a few transactions before the test can be run. These
transactions happen on the CTL side, because the Testnet can currently handle only
enterprise addresses (see [this issue](https://github.com/mlabs-haskell/plutip/issues/103)).

## Limitations
* See the `epochSize` configuration option problem [here](#current-limitations).
* Currently there's no way to share wallets between separate tests (which is useful
  for complex protocols). You can adapt [this PR](https://github.com/IndigoProtocol/cardano-transaction-lib/pull/1) (needs to be updated for the newer versions of CTL, likely won't need too many changes) if you need it now (and even better -- make a PR to CTL).
* If you've used the [`plutus-simple-model`](https://github.com/mlabs-haskell/plutus-simple-model) library then you might know that it allows to time travel in tests, which can be very useful for testing vesting schedules, etc. Testing with the Testnet doesn't allow this, as it's running a real network. A way around this problem can be to parametrize onchain logic by a time multiplier (and use a small one for tests).

## See also

- To actually write the test bodies, [assertions library](./test-utils.md) can be
  useful [(usage example)](../examples/ContractTestUtils.purs).
- Take a look at CTL's Testnet tests for the usage examples:
  - the entry point with `main` that runs Testnet tests is [here](../test/Testnet.purs),
  - folder with various test suites is [here](../test/Testnet/).
