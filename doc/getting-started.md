# Getting Started with CTL

This guide will help you get started for writing contracts with CTL. Please also see our guide [comparing CTL with Plutus/PAB](./plutus-comparison.md) which offers a more abstract overview of the project.

## Prerequisites

Please see the guide on [adding CTL as a dependency](./ctl-as-dependency.md) to a Purescript project. You will also need to become familiar with [CTL's runtime](./runtime.md) as its runtime services are required for executing most contracts.

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

## Executing contracts and the `ContractConfig`

Unlike [Plutus/PAB](plutus-comparison.md#the-contract-type), CTL is structued internally around a familiar `mtl`-style monad transformer stack. As such, contracts written in CTL are called from other Purescript code (i.e. CTL has no concept of "endpoints" or "activation"). The top-level of your program written in CTL will look like the following:

```purescript
main :: Effect Unit
main = ...
```

(`Effect` is Purescript's synchronous effect monad.)

Internally, CTL uses Purescript's `Aff` monad, which represents asynchronous effects. Thus, you must first call the eliminator for `Aff` to run your `Contract` code:

```purescript
main :: Effect Unit
main = Contract.Monad.launchAff_ $ do -- we re-export this for you
 ...
```

Within this `Aff` action, you should create a wallet type and initialize your `ContractConfig`:

```purescript
main :: Effect Unit
main = Contract.Monad.launchAff_ $ do -- we re-export this for you
  wallet <- Contract.Wallet.mkNamiWalletAff -- for the Nami backend
  cfg <- undefined -- see the next section for more details on this part
 ...
```

Then use the eliminator `Contract.Monad.runContract` (or `runContract_` to discard the return value):

```purescript
main :: Effect Unit
main = Contract.Monad.launchAff_ $ do -- we re-export this for you
  wallet <- Contract.Wallet.mkNamiWalletAff -- for the Nami backend
  cfg <- undefined -- see the next section for more details on this part
  runContract_ cfg $ do 
    ...
```

### Making the `ContractConfig`

The `ContractConfig` contains the configuration values and websocket connections that are required to execute contracts written in CTL. For local development and testing, we provide `Contract.Monad.traceContractConfig` where all service hosts are set to `localhost` and the `logLevel` is set to `Trace`. Needless to say, this is not viable for production or staging environments.

To contstruct a `ContractConfig`, it is **not recommended to directly construct or manipulate a `ContractConfig` yourself** as the process of making a new config initializes websockets. Instead, use `Contract.Monad.ConfigParams` with `Contract.Monad.mkContractConfig`. 

As explained in the [Plutus/PAB comparison](plutus-comparison.md#the-contract-type), the `ContractConfig` environment using Purescript's extensible records. This can also be done via `ConfigParams`, which holds an `extraConfig` field corresponding to the `Row Type` argument to `ContractConfig` (and by extension, `Contract`). 

An example of building a `ContractConfig` via `ConfigParams` is as follows:

```purescript
main :: Effect Unit
main = Contract.Monad.launchAff_ $ do -- we re-export this for you
  wallet <- Contract.Wallet.mkNamiWalletAff -- for the Nami backend
  cfg <- mkContractConfig $ ConfigParams
    -- The server defaults below are also exported from 
    -- `Contract.Monad`
    { ogmiosConfig: defaultOgmiosWsConfig
    , datumCacheConfig: defaultDatumCacheWsConfig
    , ctlServerConfig: defaultServerConfig
    , networkId: TestnetId
    , logLevel: Trace
    , extraConfig: { apiKey: "foo" }
    , wallet
    }
  runContract_ cfg someContractWithApiKeyInEnv

-- As we provided `(apiKey :: String)` to the `extraConfig` above, we can now
-- access it in the reader environment of any `Contract` actions call using 
-- the `ContractConfig` we created above. We can also retain polymorphism
-- by adding `| r` to the row type
someContractWithApiKeyInEnv 
  :: forall (r :: Row Type). Contract (apiKey :: String | r) Unit
someContractWithApiKeyInEnv = ...
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
    BalancedSignedTransaction bsTx <-
      liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
    ...
  ```

- Submit using `Contract.Transaction.submit` (note that due to current infelicities in CTL's internal transaction builder, we must currently use the CBOR of the balanced and signed transaction rather than the transaction itself; this will be resolved in an upcoming CTL version):
  ```purescript
  contract = do
    ...
    txId <- submit bsTx.signedTxCbor
    logInfo' $ "Tx ID: " <> show txId
  ```

### Awaiting tx confirmation

One major caveat to using CTL in its current state is that we have no equivalent of Plutus' `awaitTxConfirmed`. We cannot guarantee that a transaction that has been accepted into a mempool has actually been added to a block. When `Contract.Transaction.submit` returns, this is **not** a guarantee that your transaction has been accepted into a block. If transaction confirmation is critical for you, you may wish to adopt a different strategy: sleeping for a pre-determined amount of time, looping until an address contains UTxOs from the recently submitted transaction, etc.... We plan to add functionality similar to `awaitTxConfirmed` in upcoming versions of CTL.

## Testing

Unfortunately, CTL does not currently offer robust testing strategies out-of-the-box. We are currently working on a keypair-based wallet that will work outside of a browser environment as well as integration with the [`plutip` testing tool](https://github.com/mlabs-haskell/plutip). These will be included in an upcoming release of CTL.

For full testing with browser-based light wallets, tools such as [`puppeteer`](https://github.com/puppeteer/puppeteer) or its [Purescript bindings](https://pursuit.purescript.org/packages/purescript-toppokki) might be useful.
