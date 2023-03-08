# ctl-scaffold

Welcome to your new CTL project!

## Development

To start working on this project, enter the Nix environment by running `nix develop`. Please make sure to use Nix v2.8 or later.

Please also see our

- [Documentation](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/doc)

- [PureScript API documentation for CTL](https://plutonomicon.github.io/cardano-transaction-lib/)

- [Discord server](https://discord.gg/JhbexnV9Pc)

If you encounter problems and/or want to report a bug, you can open an issue [here](https://github.com/Plutonomicon/cardano-transaction-lib/issues).

Please search for existing issues beforehand!

## Testing

Here are a few tips on how to get started with testing your code.

### Testing with Plutip

[Plutip](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md) is a tool that manages local disposable cardano-node clusters that we use to test contracts locally.

- run `npm run test` from Nix shell

### Testing with Blockfrost

[Blockfrost.io](https://blockfrost.io) is an alternative CTL backend that can be used as a subscription service.

- populate the variables in [`test/blockfrost.env`](./test/blockfrost.env) following [this guide](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/blockfrost.md)
- run `npm run blockfrost-test` (no Nix shell required, but the correct versions of [Spago](https://github.com/purescript/spago/) and [PureScript compiler](https://github.com/purescript/purescript/releases/tag/v0.14.9) must be used)

### Testing with a headless browser

[Headless browser test suite](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/e2e-testing.md) allows to run user contracts with real wallet browser extensions automatically.

- Start the CTL runtime services: `npm run start-runtime`
- run `npm run e2e-serve` from Nix shell to start a web server
- run `npm run e2e-test` from Nix shell to run the test suite
