# Blockfrost backend

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Setting up a Blockfrost-powered test suite](#setting-up-a-blockfrost-powered-test-suite)
  - [1. Getting an API key](#1-getting-an-api-key)
  - [2. Generating private keys](#2-generating-private-keys)
  - [3. Funding your address](#3-funding-your-address)
  - [4. Setting up a directory for temporary keys](#4-setting-up-a-directory-for-temporary-keys)
  - [5. Providing an API endpoint URL](#5-providing-an-api-endpoint-url)
  - [6. Extra configuration options](#6-extra-configuration-options)
  - [7. Test suite setup on PureScript side](#7-test-suite-setup-on-purescript-side)
- [Running `Contract`s with Blockfrost](#running-contracts-with-blockfrost)
- [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

Thanks to [Catalyst Fund9](https://cardano.ideascale.com/c/idea/420791), CTL has been extended with support for [Blockfrost](https://blockfrost.io/) as an alternative query layer.

The users [can now run]((#running-contracts-with-blockfrost)) CTL contracts just by providing a Blockfrost API key and some ADA for the Contract to consume.

For testing, we offer an automated test engine that allows to run any `ContractTest` test suite with Blockfrost.

## Setting up a Blockfrost-powered test suite

Public Blockfrost instances have endpoints for different networks. By default, the test suite is configured to run on `preview`.

The configuration is stored in environment variables defined in [`test/blockfrost.env` file](../test/blockfrost.env), or a similar one in your project if it is initialized from the template.

Here's how to populate this configuration file to be ready for use:

### 1. Getting an API key

Go to https://blockfrost.io to generate a new API key and specify it as `BLOCKFROST_API_KEY` in the config.

### 2. Generating private keys

Follow https://developers.cardano.org/docs/stake-pool-course/handbook/keys-addresses/ to generate a private payment key (and, optionally, a stake key).

It should look like this:

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

### 3. Funding your address

Fund your address using the [testnet faucet](https://docs.cardano.org/cardano-testnet/tools/faucet). Make sure you are sending the funds in the correct network.

Point the test suite to your keys by setting `PRIVATE_PAYMENT_KEY_FILE` and `PRIVATE_STAKE_KEY_FILE` to the paths of your `.skey` files.

If you are going to use an enterprise address (without a staking credential component), then do not provide the staking key file. The choice of using either type of addresses does not affect anything, because the test suite will be using the address only to distribute funds to other, temporary addresses.

### 4. Setting up a directory for temporary keys

During testing, the test engine will move funds around according to the UTxO distribution specifications provided via `Contract.Test.withWallets` calls in the test bodies. It will generate private keys as needed on the fly. The private keys will be stored in a special directory, to prevent loss of funds in case the test suite exits. Set `BACKUP_KEYS_DIR` to an existing directory where you would like the keys to be stored.

### 5. Providing an API endpoint URL

Parts of the endpoint URLs are specified separately, e.g. `https://cardano-preview.blockfrost.io/api/v0/` becomes:

```bash
export BLOCKFROST_PORT=443 # https -> 443, http -> 80
export BLOCKFROST_HOST=cardano-preview.blockfrost.io
export BLOCKFROST_SECURE=true # Use HTTPS
export BLOCKFROST_PATH="/api/v0"
```

### 6. Extra configuration options

If your tests are failing because the effects of the transaction do not seem
to propagate, try increasing the delay after transaction submission. Blockfrost does not update the query layer state consistently, so this is the best workaround we can have.

```bash
export TX_CONFIRMATION_DELAY_SECONDS=30
```

### 7. Test suite setup on PureScript side

`executeContractTestsWithBlockfrost` is a helper function that reads all the variables above and takes care of contract environment setup.

It accepts a number of arguments:

1. A test spec config, e.g. `Test.Spec.Runner.defaultConfig` - it's probably better to increase the timeout.
2. A `Contract` config, e.g. `Contract.Config.testnetConfig`
3. An optional CTL runtime config
4. A `ContractTest` suite

See [this example](../test/Blockfrost/Contract.purs), which can be executed with `npm run blockfrost-test` command. It will automatically load the exported variables from [`test/blockfrost.env`](../test/blockfrost.env).

## Running `Contract`s with Blockfrost

`mkBlockfrostBackendParams` can be called on a populated `BlockfrostBackendParams` record to create a `QueryBackendParams` value. `backendParams` field of `ContractParams` uses a value of this type. And `ContractParams` can in turn be used with `runContract`.

```
type BlockfrostBackendParams =
  { blockfrostConfig :: ServerConfig
  , blockfrostApiKey :: Maybe String
  , confirmTxDelay :: Maybe Seconds
  }
```

For convenience, use `blockfrostPublicMainnetServerConfig`, `blockfrostPublicPreviewServerConfig` or `blockfrostPublicPreprodServerConfig` for pre-configured `ServerConfig` setups.

## See also

- [Testing utilities for CTL](./test-utils.md).
