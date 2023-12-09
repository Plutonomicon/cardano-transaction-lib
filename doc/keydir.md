# Keydir backend

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Setting up a keydir test suite](#setting-up-a-keydir-test-suite)
  - [Generating private keys](#generating-private-keys)
  - [Funding your address](#funding-your-address)
  - [Setting up a directory for temporary keys](#setting-up-a-directory-for-temporary-keys)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Setting up a keydir test suite

In this section, we show how to set up a test suite that uses the keydir backend
to run `Contract`s and print reports nicely using [Mote][mote].

**Important note:** Blockfrost-based tests also use the keydir test engine.
Thus, if you have already done basic setup for Blockfrost-based tests, you don't
have to repeat this setup for keydir-based tests.

### Generating private keys

Follow [the Cardano handbook][cardano-handbook-keys-addresses] to generate a
private payment key, and optionally, a stake key. You can use [this
script][ctl-generate-keys] for convenience instead of following the instructions
given in this section.

The generated keys should look like this:

```json
{
    "type": "PaymentSigningKeyShelley_ed25519",
    "description": "Payment signing key",
    "cborHex": "..."
}
```

Get the address for this payment key (and, optionally, stake key), following the
guide above.

If you are using a testnet, replace the `--mainnet` flag in the shell command
with `--testnet-magic YOUR_NETWORK_MAGIC`, where `YOUR_NETWORK_MAGIC` is a
genesis parameter of the network. For public testnets, you can get this value
from the [`cardano-configurations` repo][cardano-configurations]. You can find
it in `network/YOUR_NETWORK_NAME/genesis/shelley.json`, under the `networkMagic`
key. Common values are 1 for `preprod`, and 2 for `preview`.

### Funding your address

Fund your address using the [testnet faucet][testnet-faucet]. Make sure you're
sending funds in the correct network.

Point the test suite to your keys by setting `PRIVATE_PAYMENT_KEY_FILE` and
`PRIVATE_STAKE_KEY_FILE` to the paths of your `.skey` files. If you are using an
enterprise address (without a staking credential component), then do not provide
the staking key file. Whether you use a staking credential component or not does
not affect anything, because the test suite will only use this address to
distribute funds to other, temporary addresses.

### Setting up a directory for temporary keys

During testing, the test engine will move funds around according to the UTxO
distribution specifications provided via `Contract.Test.withWallets' calls in
the test bodies. This will generate private keys as-needed on the fly: these
will be stored in a special directory, to prevent loss of funds in case the test
suite suddenly exits. This directory will become the `FilePath` argument to
`runContractTestsWithKeyDir`.

Each test will generate fresh keys that will be stored in this directory
indefinitely. It is up to the user to decide when to delete the corresponding
directories. We don't dispose of the keys automatically, as there may be some
on-chain state that's uniquely tied to them, which the user might not want to
lose access to.

[mote]: https://github.com/garyb/purescript-mote
[cardano-handbook-keys-addresses]: https://developers.cardano.org/docs/stake-pool-course/handbook/keys-addresses
[ctl-generate-keys]: https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/scripts/generate-keys.sh
[cardano-configurations]: https://github.com/input-output-hk/cardano-configurations
[testnet-faucet]: https://docs.cardano.org/cardano-testnet/tools/faucet
