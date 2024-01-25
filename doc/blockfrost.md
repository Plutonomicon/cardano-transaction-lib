# Blockfrost backend

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Setting up a Blockfrost-powered test suite](#setting-up-a-blockfrost-powered-test-suite)
  - [Getting an API key](#getting-an-api-key)
  - [Generating private keys](#generating-private-keys)
  - [Funding your address](#funding-your-address)
  - [Setting up a directory for temporary keys](#setting-up-a-directory-for-temporary-keys)
  - [Providing an API endpoint URL](#providing-an-api-endpoint-url)
  - [Setting Tx confirmation delay](#setting-tx-confirmation-delay)
  - [Test suite setup on PureScript side](#test-suite-setup-on-purescript-side)
- [Running `Contract`s with Blockfrost](#running-contracts-with-blockfrost)
- [Running Blockfrost locally](#running-blockfrost-locally)
  - [Configuration](#configuration)
    - [Contract parameters](#contract-parameters)
    - [Test suite](#test-suite)
  - [Synchronization](#synchronization)
- [Limitations](#limitations)
  - [Performance](#performance)
  - [Transaction chaining](#transaction-chaining)
  - [Getting pool parameters](#getting-pool-parameters)
- [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

Thanks to [Catalyst Fund9](https://cardano.ideascale.com/c/idea/420791), CTL has been extended with support for [Blockfrost](https://blockfrost.io/) as an alternative query layer.

The users [can now run](#running-contracts-with-blockfrost) CTL contracts just by providing a Blockfrost API key and some ADA for the Contract to consume.

For testing, we offer an automated test engine that allows to run any `ContractTest` test suite with Blockfrost.

## Setting up a Blockfrost-powered test suite

In this section, we show how to set up a test suite that uses public Blockfrost deployment to run `Contract`s and prints the report nicely using [Mote](https://github.com/garyb/purescript-mote).

Public Blockfrost instances have endpoints for different networks. By default, the test suite is configured to run on `preview`.

The configuration is stored in environment variables defined in [`test/blockfrost.env` file](../test/blockfrost.env), or a similar one in your project if it is initialized from the template.

Here's how to populate this configuration file to be ready for use:

### Getting an API key

Go to https://blockfrost.io to generate a new API key and specify it as `BLOCKFROST_API_KEY` in the config.

### Generating private keys

Follow https://developers.cardano.org/docs/stake-pool-course/handbook/keys-addresses/ to generate a private payment key (and, optionally, a stake key). You can use [this script](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/scripts/generate-keys.sh) for convenience instead of following instructions in this section manually.

The generated keys should look like this:

```json
{
    "type": "PaymentSigningKeyShelley_ed25519",
    "description": "Payment Signing Key",
    "cborHex": "..."
}
```

Get the address for this payment key (and, optionally, a stake key), following the guide above.

If you are using a testnet, replace `--mainnet` flag in the shell command with
`--testnet-magic YOUR_NETWORK_MAGIC`, where `YOUR_NETWORK_MAGIC` is a genesis
parameter of the network.

For public testnets, get it from [cardano-configurations repo](https://github.com/input-output-hk/cardano-configurations). The location is `network/YOUR_NETWORK_NAME/genesis/shelley.json`, look for `networkMagic` key.

The common values are 1 for `preprod` and 2 for `preview`.

### Funding your address

Fund your address using the [testnet faucet](https://docs.cardano.org/cardano-testnet/tools/faucet). Make sure you are sending the funds in the correct network.

Point the test suite to your keys by setting `PRIVATE_PAYMENT_KEY_FILE` and `PRIVATE_STAKE_KEY_FILE` to the paths of your `.skey` files.

If you are going to use an enterprise address (without a staking credential component), then do not provide the staking key file. The choice of using either type of addresses does not affect anything, because the test suite will be using the address only to distribute funds to other, temporary addresses.

### Setting up a directory for temporary keys

During testing, the test engine will move funds around according to the UTxO distribution specifications provided via `Contract.Test.withWallets` calls in the test bodies. It will generate private keys as needed on the fly. The private keys will be stored in a special directory, to prevent loss of funds in case the test suite suddently exits. Set `BACKUP_KEYS_DIR` to an existing directory where you would like the keys to be stored.

In this directory, keys will be stored in subdirs named as addresses that are derived from these keys. Most of these directories will contain a file named `inactive`. It indicates that the test suite assumes that there are no funds left (because they have been withdrawn successfully).

Each test run generates fresh keys that will be stored indefinitely, and it's up to the user to decide when to delete the corresponding directories. The reason why the keys are not being disposed of automatically is because there may be some on-chain state uniquely tied to them that the user may not want to lose access to.

### Providing an API endpoint URL

Blockfrost dashboard provides endpoint URLs for your projects.

In the test suite configuration, parts of the endpoint URLs are specified separately, e.g. `https://cardano-preview.blockfrost.io/api/v0/` becomes:

```bash
export BLOCKFROST_PORT=443 # https -> 443, http -> 80
export BLOCKFROST_HOST=cardano-preview.blockfrost.io
export BLOCKFROST_SECURE=true # Use HTTPS
export BLOCKFROST_PATH="/api/v0"
```

### Setting Tx confirmation delay

We introduce an artificial delay after Tx confirmation to ensure that the changes propagate to Blockfrost's query layer.
Blockfrost does not update the query layer state atomically (proxied Ogmios eval-tx endpoint [seems to lag behind the DB](https://github.com/blockfrost/blockfrost-backend-ryo/issues/86)), and we have no way to query it, so this is the best workaround we can have.
If the tests are failing because the effects of the transaction do not seem to propagate (the symptom is unexpected errors from Ogmios), it is possible to increase the delay by setting the environment variable for the test suite:

```bash
export TX_CONFIRMATION_DELAY_SECONDS=30
```

The "safe" value in practice is 30 seconds.

If there's a problem with UTxO set syncrhonization, most commonly Blockfrost returns error code 400 on transaction submission:

```
[TRACE] 2023-02-16T12:26:13.019Z { body: "{\"error\":\"Bad Request\",\"message\":\"\\\"transaction submit error ShelleyTxValidationError ShelleyBasedEraBabbage (ApplyTxError [UtxowFailure (UtxoFailure (FromAlonzoUtxoFail (ValueNotConservedUTxO ...
```

### Test suite setup on PureScript side

`executeContractTestsWithBlockfrost` is a helper function that reads all the variables above and takes care of contract environment setup.

It accepts a number of arguments:

1. A test spec config, e.g. `Test.Spec.Runner.defaultConfig` - it's probably better to increase the timeout to 300 seconds or more.
2. A `Contract` config, e.g. `Contract.Config.testnetConfig`
3. A `ContractTest` suite

See [this example](../test/Blockfrost/Contract.purs), which can be executed with `npm run blockfrost-test` command in CTL repo. It will automatically load the exported variables from [`test/blockfrost.env`](../test/blockfrost.env).

## Running `Contract`s with Blockfrost

On production, the environment should be configured on PureScript side (environment variables are not respected).

`mkBlockfrostBackendParams` can be called on a populated `BlockfrostBackendParams` record to create a `QueryBackendParams` value. `backendParams` field of `ContractParams` uses a value of this type. And `ContractParams` can in turn be used with `runContract`.

```purescript
type BlockfrostBackendParams =
  { blockfrostConfig :: ServerConfig
  , blockfrostApiKey :: Maybe String
  , confirmTxDelay :: Maybe Seconds
  }
```

For convenience, use `blockfrostPublicMainnetServerConfig`, `blockfrostPublicPreviewServerConfig` or `blockfrostPublicPreprodServerConfig` for pre-configured `ServerConfig` setups.


## Running Blockfrost locally

Blockfrost can be started locally using [blockfrost-backend-ryo](https://github.com/blockfrost/blockfrost-backend-ryo) distribution. It is included in CTL runtime as optional service, use `npm run start-blockfrost-runtime` instead of `npm run start-runtime`.

The differences between public and run-your-own flavours of Blockfrost are:

- There's no need to provide an API key
- `/tx/submit` and `/utils/txs/evaluate` endpoints, that are simply proxied to Ogmios on the public instance, [are not provided](https://github.com/blockfrost/blockfrost-backend-ryo/issues/38) within the distribution, so Ogmios should be used for these.

### Configuration

#### Contract parameters

Configuration on PureScript side is the same as above, but `mkSelfHostedBlockfrostBackendParams` should be used, to specify Ogmios and Kupo parameters (only Ogmios will in fact be used).

`Contract.Config.blockfrostSelfHostedServerConfig` contains default server parameters usable with a locally-deployed Blockfrost instance, together with `defaultOgmiosWsConfig`.

CTL handles the absence of `submit` and `evaluate` endpoints in self-hosted Blockfrost nicely by setting up own runtime to query Ogmios for these two endpoints when `mkSelfHostedBlockfrostBackendParams` is used.

#### Test suite

The Blockfrost test suite can also be configured for local runtime. To let it connect to local Ogmios, these four parameters must be provided:

```bash
export OGMIOS_PORT=1337
export OGMIOS_HOST=127.0.0.1
export OGMIOS_PATH=""
export OGMIOS_SECURE=false
```

See [`blockfrost-local.env`](../test/blockfrost-local.env) for a complete example.

### Synchronization

`cardano-db-sync` takes 30 minutes or more to synchronize with `preview` network (and many hours with `mainnet`).

It will print messages like these, so you can estimate the sync progress knowing the latest epoch number:

```
db-sync-preview_1   | [db-sync-node:Info:70] [2023-03-06 15:09:27.39 UTC] Starting epoch 38
db-sync-preview_1   | [db-sync-node:Info:70] [2023-03-06 15:09:27.40 UTC] epochPluginInsertBlockDetails: epoch 37
```

## Limitations

### Performance

The main disadvantage of using Blockfrost in comparison with CTL backend is speed of Tx confirmation (see [here](#6-setting-tx-confirmation-delay) for explanation).

Contracts running with local Blockfrost instance are expected to be slightly slower, because transaction propagation takes a bit longer.

### Transaction chaining

Public Blockfrost instance is proxying [Ogmios](https://ogmios.dev) to provide an endpoint for execution units evaluation. This Ogmios endpoint normally [accepts a parameter](https://ogmios.dev/mini-protocols/local-tx-submission/#additional-utxo-set) that allows to specify additional UTxOs that should be considered. Transaction chaining is relying on this feature to allow Ogmios to "see" the newly created UTxOs. But Blockfrost seems to not pass this parameter to Ogmios ([issue](https://github.com/blockfrost/blockfrost-backend-ryo/issues/85)).

When using self-hosted blockfrost, chaining works normally, because local Ogmios is used without proxying.

### Getting pool parameters

`getPoolParameters` function only runs with [CTL backend](./runtime.md#ctl-backend) (Ogmios), see [here](https://github.com/blockfrost/blockfrost-backend-ryo/issues/82) for more context.

This function is never used for constraints resolution, the only way to make it run is to call it manually.

## See also

- [Testing utilities for CTL](./test-utils.md).
