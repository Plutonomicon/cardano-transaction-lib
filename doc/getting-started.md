# Getting Started with CTL

This guide will help you get started writing contracts with CTL. Please also see our guide [comparing CTL with Plutus/PAB](./plutus-comparison.md) which offers a more abstract overview of the project.

**Table of Contents**

- [Prerequisites](#prerequisites)
- [Importing CTL modules](#importing-ctl-modules)
  - [The `Contract` interface](#the-contract-interface)
  - [Our `Prelude`](#our-prelude)
- [Executing contracts and the `ContractEnv`](#executing-contracts-and-the-contractenv)
  - [Making the `ContractEnv`](#making-the-contractenv)
- [Building and submitting transactions](#building-and-submitting-transactions)
  - [Awaiting tx confirmation](#awaiting-tx-confirmation)
- [Testing](#testing)
  - [With a light wallet](#with-a-light-wallet)
  - [Without a light wallet](#without-a-light-wallet)

## Prerequisites

You need to have set up a Purescript project using CTL as a dependency ([more details here](./ctl-as-dependency.md)). You will also need to become familiar with [CTL's runtime](./runtime.md) as its runtime services are required for executing virtually all contracts.

## Importing CTL modules

### The `Contract` interface

CTL's public interface is contained in the `Contract.*` namespace. We recommend to always prefer imports from the `Contract` namespace. That is, **avoid importing any CTL modules not contained in `Contract`**, which should be considered internal. Importing non-`Contract` modules will make your code more brittle and susceptible to breakages when upgrading CTL versions.

For example, avoid the following:

```purescript

-- Anything not in in the `Contract` namespace should be considered an
-- **internal** CTL module

import Types.TokenName (TokenName)
import Types.Scripts (MintingPolicy)

```

Instead, prefer:

```purescript

import Contract.Value (TokenName)
import Contract.Scripts (MintingPolicy)

```

### Our `Prelude`

Unlike Haskell, Purescript's `Prelude` is not imported implicitly in every module and is much smaller in scope (for example, common non-primitive types like `Maybe` are contained in their own packages, rather than in the `Prelude`). Rather than require users to import Purescript's `Prelude` and other common modules directly, we offer a `Contract.Prelude` that re-exports Purescript's `Prelude`, several common modules (e.g. `Data.Maybe`, `Data.Either`, etc...), and CTL-specific functionality. **We recommend using `Contract.Prelude` as a replacement for `Prelude` in projects using CTL**, particularly for developers who are less familiar with Purescript and its divergences from Haskell.

## Executing contracts and the `ContractEnv`

Unlike [Plutus/PAB](plutus-comparison.md#the-contract-type), CTL is structued internally around a familiar `mtl`-style monad transformer stack. As such, contracts written in CTL are called from other Purescript code (i.e. CTL has no concept of "endpoints" or "activation"). The top-level of your program written in CTL will look like the following:

```purescript
main :: Effect Unit
main = ...
```

(`Effect` is Purescript's synchronous effect monad.)

Internally, CTL uses Purescript's `Aff` monad, which represents asynchronous effects. Thus, you must first call the eliminator for `Aff` to run your `Contract` code:

```purescript
main :: Effect Unit
main = Contract.Monad.launchAff_ do -- we re-export this for you
 ...
```

Then use the eliminator `Contract.Monad.runContract` with a config specifying network and wallet:

```purescript
main :: Effect Unit
main = Contract.Monad.launchAff_ do
  runContract Contract.Config.testnetNamiConfig do
    ...
```

### Making the `ContractEnv`

The `ContractEnv` type contains configuration values and websocket connections that are required to execute contracts written in CTL. The users should not construct it directly - `Contract.Config.ConfigParams` should be used instead.

For local development and testing, we provide `Contract.Config.testnetConfig` where all service hosts are set to `localhost` and the `logLevel` is set to `Trace`.

It is **not recommended to directly construct or manipulate a `ContractEnv` yourself** as the process of making a new config initializes websockets. Instead, use `Contract.Monad.ConfigParams` with `runContract`.

As explained in the [Plutus/PAB comparison](plutus-comparison.md#the-contract-type), the `ContractEnv` environment using Purescript's extensible records. This can also be done via `ConfigParams`, which holds an `extraConfig` field corresponding to the `Row Type` argument to `ContractEnv` (and by extension, `Contract`).

A special `Contract.Config.WalletSpec` type is used to specify which wallet to use during the `Contract` lifetime.

An example of building a `Contract` via `ConfigParams` is as follows:

```purescript
main :: Effect Unit
main = Contract.Monad.launchAff_ do -- we re-export this for you
  let
    (config :: ConfigParams (apiKey :: String)) =
      { ogmiosConfig: defaultOgmiosWsConfig
      , datumCacheConfig: defaultDatumCacheWsConfig
      , ctlServerConfig: defaultServerConfig
      , networkId: TestnetId
      , logLevel: Trace
      , extraConfig: { apiKey: "foo" }
      , walletSpec: Just ConnectToNami
      , customLogger: Nothing
      }
  runContract config someContractWithApiKeyInEnv

-- As we provided `(apiKey :: String)` to the `extraConfig` above, we can now
-- access it in the reader environment of any `Contract` actions call using
-- `askConfig`.
someContractWithApiKeyInEnv
  :: forall. Contract (apiKey :: String) Unit
  -- We can also retain polymorphism by adding `| r` to the row type:
  --   :: forall (r :: Row Type). Contract (apiKey :: String | r) Unit
someContractWithApiKeyInEnv = do
  { apiKey } <- askConfig
  ...
```

## Building and submitting transactions

Unlike PAB, CTL obscures less of the build-balance-sign-submit pipeline for transactions and most of the steps are called individually. The general workflow in CTL is similar to the following:

- Build a transaction using `Contract.ScriptLookups` and `Contract.TxConstraints` (it is also possible to directly build a `Transaction` if you require even greater low-level control over the process, although we recommend the constraints/lookups approach for most users):

  ```purescript
  contract = do
    let
      constraints :: TxConstraints Unit Unit
      constraints = TxConstraints.mustPayToScript vhash unitDatum
          $ Value.lovelaceValueOf
          $ BigInt.fromInt 2_000_000

      lookups :: ScriptLookups PlutusData
      lookups = ScriptLookups.validator validator

    -- `liftedE` will throw a runtime exception on `Left`s
    ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    ...
  ```

- Sign it and balance it using `Contract.Transaction.balanceAndSignTx`:

  ```purescript
  contract = do
    ...
    -- `liftedM` will throw on `Nothing`s
    bsTx <-
      liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
    ...
  ```

- Submit using `Contract.Transaction.submit`:

  ```purescript
  contract = do
    ...
    txId <- submit bsTx
    logInfo' $ "Tx ID: " <> show txId
  ```

### Awaiting tx confirmation

One major caveat to using CTL in its current state is that we have no equivalent of Plutus' `awaitTxConfirmed`. We cannot guarantee that a transaction that has been accepted into a mempool has actually been added to a block. When `Contract.Transaction.submit` returns, this is **not** a guarantee that your transaction has been accepted into a block. If transaction confirmation is critical for you, you may wish to adopt a different strategy: sleeping for a pre-determined amount of time, looping until an address contains UTxOs from the recently submitted transaction, etc.... We plan to add functionality similar to `awaitTxConfirmed` in upcoming versions of CTL.

## Testing

### Without a light wallet

We provide `KeyWallet` to enable testing outside of the browser, or in-browser without a light wallet installed. To generate a key, you can use `cardano-cli` as follows:

```bash
$ cardano-cli address key-gen --normal-key --signing-key-file payment.skey --verification-key-file payment.vkey
```

The signing key can be loaded to CTL using `WalletSpec`'s `UseKeys` constructor. See `examples/Pkh2PkhKeyWallet.purs`.

From here you can submit transactions that will be signed with your private key, or perhaps export transactions to be tested with external tools such as [`plutip` testing tool](https://github.com/mlabs-haskell/plutip). We are currently working on integration with the plutip. These will be included in an upcoming release of CTL.

### With a light wallet

For full testing with browser-based light wallets, tools such as [`puppeteer`](https://github.com/puppeteer/puppeteer) or its [Purescript bindings](https://pursuit.purescript.org/packages/purescript-toppokki) might be useful.
