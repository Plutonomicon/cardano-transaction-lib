# CTL integration with Plutip

[Plutip](https://github.com/mlabs-haskell/plutip) is a tool to run private Cardano testnets. CTL provides integration with Plutip via a [`plutip-server` binary](https://github.com/mlabs-haskell/plutip/pull/79) that exposes an HTTP interface to control local Cardano clusters.

**Table of Contents**

- [Architecture](#architecture)
- [Testing contracts](#testing-contracts)
  - [Testing with Nix](#testing-with-nix)
- [Limitations](#limitations)

## Architecture

CTL depends on a number of binaries in the `$PATH` to execute Plutip tests:

- `plutip-server` to launch a local `cardano-node` cluster
- [`ogmios`](https://ogmios.dev/)
- [`ogmios-datum-cache`](https://github.com/mlabs-haskell/ogmios-datum-cache)
- PostgreSQL: `initdb`, `createdb` and `psql` for `ogmios-datum-cache` storage

If you plan on using CTL's `applyArgs` effect, you must also ensure the following is on your `$PATH`:

- `ctl-server`: a server-side part of CTL itself

All of these are provided by CTL's `overlays.runtime` (and are provided in CTL's own `devShell`). You **must** use the `runtime` overlay or otherwise make the services available in your package set (e.g. by defining them within your own `overlays` when instantiating `nixpkgs`) as `purescriptProject.runPlutipTest` expects all of them.

The services are NOT run by `docker-compose` as is the case with `launchCtlRuntime`: they are started and stopped on each CTL `Contract` execution by CTL.

## Testing contracts

The main entry point to the testing interface is `Contract.Test.Plutip.runPlutipContract` function:

```purescript
runPlutipContract
  :: forall (distr :: Type) (wallets :: Type) (a :: Type)
   . UtxoDistribution distr wallets
  => PlutipConfig
  -> distr
  -> (wallets -> Contract () a)
  -> Aff a
```

`distr` is a specification of how many wallets and with how much funds should be created. It should either be a `unit` (for no wallets) or nested tuples containing `Array BigInt` - each element of the array specifies an UTxO amount in Lovelaces (0.000001 Ada).

The `wallets` argument is either a `Unit` or a tuple of `KeyWallet`s (with the same nesting level as in `distr`, which is guaranteed by `UtxoDistribution`).

`wallets` should be pattern-matched on, and its components should be passed to `withKeyWallet`:

An example `Contract` with two actors:

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

In most cases at least two UTxOs per wallet are needed (one of which will be used as collateral, so it should exceed `5_000_000` Lovelace).

Note that during execution WebSocket connection errors may occur. However, payloads are re-sent after these errors, so you can ignore them. [These errors will be suppressed in the future.](https://github.com/Plutonomicon/cardano-transaction-lib/issues/670).

### Testing with Nix

You can run Plutip tests via CTL's `purescriptProject` as well. After creating your project, you can use the `runPlutipTest` attribute to create a Plutip testing environment that is suitable for use with your flake's `checks`. An example:

```nix
{
  some-plutip-test = project.runPlutipTest {
    name = "some-plutip-test";
    testMain = "MyProject.Test.Plutip";
    # If you don't need `ctl-server`, you can set the following
    # to `false`. Make sure to leave it as `true` (the default)
    # if you are calling `applyArgs` in your contracts. This
    # must match your `PlutipConfig` -- if `ctlServerConfig` is
    # `Nothing`, `ctl-server` will not be spawned
    withCtlServer = false;
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
