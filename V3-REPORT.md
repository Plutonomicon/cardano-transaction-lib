# v3.0.0 feature report

# Milestone 1 - Specification

Refer to this document:

https://docs.google.com/document/d/1QRFB4bfWFXZGnTA7i6gccgD6o5CEW660/view

# Milestone 2 - Stake validators

Tests that demonstrate stake validators support are located in [test/Plutip/Staking.purs](./test/Plutip/Staking.purs) and can be run using `npm run staking-test` from the development shell.

It covers:

- Registration/Deregistration of stake credentials (pubkeys, plutus and native scripts)
- Registration/Retirement of a stake pool
- Delegation of ADA to a stake pool
- Receiving rewards
- Withdrawing rewards

# Milestone 3 - Babbage features

For feature overview see our [docs](./doc/babbage-features.md).

The following Plutip tests show support for each of the features:

## CIP-31 - Reference inputs

`ReferenceInputs`
`ReferenceInputsAndScripts`

## CIP-32 - Inline datums

`InlineDatum` - paying & withdrawing ADA to/from a script that uses inline datums

## CIP-33 - Reference scripts

`ReferenceScripts`
`ReferenceScripts (with StakeKey, testing mustPayToScriptAddressWithScriptRef)`
`ReferenceInputsAndScripts`

## CIP-40 - Explicit Collateral Output (Collateral Return)

`AlwaysFails Ada Collateral Return`

# Milestone 4 - Add a chain indexer and update the runtime, internal queries, and docs

Chain indexer of choice is Kupo. It's can be demonstrated by its presence [in the runtime](./nix/runtime.nix) and in the [PureScript codebase](./src/Internal/QueryM/Kupo.purs).

No regressions have been observed in the test suite.

# Milestone 5 - Add out-of-the-box support for e2e tests (for users, PS interface)

New E2E test suite can be used out-of-the-box after performing the following steps:

1. Initializing the template as described in [getting-started.md](./doc/getting-started.md)
2. Starting `ctl-runtime` (`nix run -L .#ctl-scaffold-runtime` in the template directory)
3. Entering the nix shell and running `npm run e2e-serve` to serve the test contract
4. Entering another nix shell and running `npm run e2e-test-debug` to see how the contracts are being executed in the browser (use `npm run e2e-test` to run in headless mode with test reporting only). The E2E assets will be downloaded on the first run and put to user data directory.

# Milestone 6 - Integrate CIP-30 mock wallet with Plutip and run e2e tests on CI

The E2E test suite that uses Plutip and CIP-30 mock can be executed in CI or locally using `nix build -L .#hydraJobs.x86_64-linux.ctl-e2e-test` command.

# Milestone 7 - Finish writing tests for all of the features mentioned in `doc/test-plan.md`

See [doc/test-plan.md](./doc/test-plan.md).

# Milestone 8 - Documentation

- Release notes - can be found in [CHANGELOG](./CHANGELOG.md)
- Tutorials
  - [getting started](./doc/getting-started.md)
  - [Using CTL as dependency](./doc/ctl-as-dependency.md)
  - [importing scripts](./doc/importing-scripts.md)
  - [Plutip testing](./doc/plutip-testing.md)
  - [E2E testing](./doc/e2e-testing.md)
- Examples - can be located in the [examples/](./examples/) directory.
- Explainers/How-to articles for new features and API’s:
  - [Babbage features](./doc/babbage-features.md)
  - [Runtime](./doc/runtime.md)
  - [Comparison with Plutus](./doc/side-by-side-ctl-plutus-comparison.md)
