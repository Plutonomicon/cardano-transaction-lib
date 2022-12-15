# CTL integration with Plutip

[Plutip](https://github.com/mlabs-haskell/plutip) is a tool to run private Cardano testnets. CTL provides integration with Plutip via a [`plutip-server` binary](https://github.com/mlabs-haskell/plutip/pull/79) that exposes an HTTP interface to control local Cardano clusters.

**Table of Contents**

- [Architecture](#architecture)
- [Testing contracts](#testing-contracts)
  + [Testing in Aff context](#testing-in-aff-context)
  + [Testing with Mote](#testing-with-mote)
  + [Testing with Nix](#testing-with-nix)
- [Using addresses with staking key components](#using-addresses-with-staking-key-components)

## Architecture

CTL depends on a number of binaries in the `$PATH` to execute Plutip tests:

- `plutip-server` to launch a local `cardano-node` cluster
- [`ogmios`](https://ogmios.dev/)
- [`kupo`](https://cardanosolutions.github.io/kupo/)
- [`ogmios-datum-cache`](https://github.com/mlabs-haskell/ogmios-datum-cache)
- PostgreSQL: `initdb`, `createdb` and `psql` for `ogmios-datum-cache` storage

All of these are provided by CTL's `overlays.runtime` (and are provided in CTL's own `devShell`). You **must** use the `runtime` overlay or otherwise make the services available in your package set (e.g. by defining them within your own `overlays` when instantiating `nixpkgs`) as `purescriptProject.runPlutipTest` expects all of them.

The services are NOT run by `docker-compose` as is the case with `launchCtlRuntime`: they are started and stopped on each CTL `Contract` execution by CTL.

## Testing contracts

There are two entry points to the testing interface: `Contract.Test.Plutip.runPlutipContract` and `Contract.Test.Plutip.testPlutipContracts`. They work similarly, the difference being that `runPlutipContract` accepts a single `Contract` and runs in `Aff`, whereas `testPlutipContracts` transforms a `MoteT` test tree of `PlutipTest` into `Aff`. [Mote](https://github.com/garyb/purescript-mote) is a DSL for defining tests, and combined with `testPlutipContracts` you can use a single plutip instance to run multiple indepedent tests.

### Testing in Aff context

`Contract.Test.Plutip.runPlutipContract`'s function type is as follows:

```purescript
runPlutipContract
  :: forall (distr :: Type) (wallets :: Type) (a :: Type)
   . UtxoDistribution distr wallets
  => PlutipConfig
  -> distr
  -> (wallets -> Contract () a)
  -> Aff a
```

`distr` is a specification of how many wallets and with how much funds should be created. It should either be a `unit` (for no wallets), nested tuples containing `Array BigInt` or an `Array` of `Array BigInt`, where each element of the `Array BigInt` specifies an UTxO amount in Lovelaces (0.000001 Ada).

The `wallets` argument is either a `Unit`, a tuple of `KeyWallet`s (with the same nesting level as in `distr`, which is guaranteed by `UtxoDistribution`) or an `Array KeyWallet`.

`wallets` should be pattern-matched on, and its components should be passed to `withKeyWallet`:

An example `Contract` with two actors using nested tuples:

```purescript
let
  distribution :: Array BigInt /\ Array BigInt
  distribution =
    [ BigInt.fromInt 1_000_000_000
    , BigInt.fromInt 2_000_000_000
    ] /\
      [ BigInt.fromInt 2_000_000_000 ]
runPlutipContract config distribution \(alice /\ bob) -> do
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
    [ [ BigInt.fromInt 1_000_000_000, BigInt.fromInt 2_000_000_000]
    , [ BigInt.fromInt 2_000_000_000 ]
    ]
runPlutipContract config distribution \wallets -> do
  traverse_ ( \wallet -> do
                withKeyWallet wallet do
                  pure unit -- sign, balance, submit, etc.
            )
            wallets
```

In most cases at least two UTxOs per wallet are needed (one of which will be used as collateral, so it should exceed `5_000_000` Lovelace).

Note that during execution WebSocket connection errors may occur. However, payloads are re-sent after these errors, so you can ignore them. [These errors will be suppressed in the future.](https://github.com/Plutonomicon/cardano-transaction-lib/issues/670).

### Testing with Mote

`Contract.Test.Plutip.testPlutipContracts` type is as follows:

```purescript
testPlutipContracts
  :: PlutipConfig
  -> MoteT Aff PlutipTest Aff Unit
  -> MoteT Aff (Aff Unit) Aff Unit
```

The final `MoteT` type requires the bracket, test and test building type to all be in `Aff`. The brackets cannot be ignored in the `MoteT` test runner, as it is what allows a single plutip instance to persist over multiple tests.

To create tests of type `PlutipTest`, you must either use `Contract.Test.Plutip.withWallets` or `Contract.Test.Plutip.noWallet`, the latter being a helper alias of the first:

```purescript
withWallets
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> (wallets -> Contract () Unit)
  -> PlutipTest

noWallet :: Contract () Unit -> PlutipTest
noWallet test = withWallets unit (const test)
```

The type is very similar to that of `runPlutipContract`, and distributions are handled in the same way. The following is an example of running multiple tests under the same plutip instance:

```purescript
suite :: MoteT Aff (Aff Unit) Aff
suite = testPlutipContracts config do
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

### Testing with Nix

You can run Plutip tests via CTL's `purescriptProject` as well. After creating your project, you can use the `runPlutipTest` attribute to create a Plutip testing environment that is suitable for use with your flake's `checks`. An example:

```nix
{
  some-plutip-test = project.runPlutipTest {
    name = "some-plutip-test";
    testMain = "Test.MyProject.Plutip";
    # The rest of the arguments are passed through to `runPursTest`:
    env = { SOME_ENV_VAR = "${some-value}"; };
  };
}
```

## Using addresses with staking key components

It's possible to use stake keys with Plutip. `Contract.Test.Plutip.withStakeKey` function can be used to modify the distribution spec:

```purescript
let
  privateStakeKey :: PrivateStakeKey
  privateStakeKey = wrap $ unsafePartial $ fromJust
    $ privateKeyFromBytes =<< hexToRawBytes
      "633b1c4c4a075a538d37e062c1ed0706d3f0a94b013708e8f5ab0a0ca1df163d"
  aliceUtxos =
    [ BigInt.fromInt 2_000_000_000
    , BigInt.fromInt 2_000_000_000
    ]
  distribution = withStakeKey privateStakeKey aliceUtxos
```

Although stake keys serve no real purpose in plutip context, they allow to use base addresses, and thus allow to have the same code for plutip testing, in-browser tests and production.

Note that CTL re-distributes tADA from payment key-only ("enterprise") addresses to base addresses, which requires a few transactions before the test can be run. Plutip can currently handle only enterprise addreses (see [this issue](https://github.com/mlabs-haskell/plutip/issues/103)).
