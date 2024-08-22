<!-- DOCTOC SKIP -->

# Testing CTL `Contract`s

This page summarizes various approaches to testing with CTL.

## Testing with Cardano Testnet

[See here for more info](./cardano-testnet-testing.md).

## Testing with a headless browser (E2E testing)

"E2E test engine", in our terminology, is a set of APIs we provide to faciliate running tests in real browsers in headless mode. The test engine handles browser startup, interaction with the page, and, most importantly, it clicks through the wallet UI when there is a need to confirm transactions.

[See here for more info](./e2e-testing.md)

## Key wallets

Key wallet is a special kind of wallet that simply wraps a private key.

See `Contract.Wallet.Key` and `Contract.Wallet.KeyFile` for its interface.

## CIP-30 mocking

It is possible to test `Contract`s that explicitly use wallet connections in NodeJS environment. But actual wallet calls will be replaced by mock methods.

See `Contract.Test.Cip30Mock` module.

## Cardano Testnet and CIP-30 mocking in headless browsers

We also provide abilities to test `Contract`s in a headless browser using query layer of a Cardano Testnet cluster as backend and CIP-30 mock instead of a wallet. This method provides stronger guarantees than just CIP-30 mocking in NodeJS, while retaining the ability to be used in Nix builds (by not depending on a real light wallet extension).

[See here for more info](./e2e-testing.md#using-cip-30-mock-with-cardano-testnet).

## Assertion helpers in PureScript

Assertion utilities in PureScript can be used in any testing environment, because they work in `Contract` monad. It is possible to define dynamic properties, like asset gains or script execution budgets.

[See here for more info](./test-utils.md)
