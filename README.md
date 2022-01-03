# Cardano Browser Tx

## Goals:

1. build a transaction in the browser that works with at least 1 light wallet (Nami).
2. once we can construct a simple user-to-user transaction, we will try to use the library to submit the Tx with nami. 
3. Once we have a simple working transaction, we will seek to build a Plutus Contract transaction With datum from scratch. 
4. Once we can construct Plutus Contract transactions, we will seek to build a library/dsl/interface such that transactions can be built using constraints and lookups - as close as possible to a cut-and-paste solution from `Contract` Monad code in haskell (but with no guarantee that code changes are not necessary)

## resources/tools:
  - Cardano-serialization-lib (Sundae fork):https://github.com/SundaeSwap-finance/cardano-serialization-lib)
  - ogmios - for querying the chain - https://ogmios.dev 
  - example testbed - https://github.com/Benjmhart/nami-integration 
  - CIP-30 (Wallet interface - nami partially implements this) -https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030
  - Nami docs - https://github.com/Berry-Pool/nami-wallet 
  - cddl spec for alonzo - https://github.com/input-output-hk/cardano-ledger/blob/0738804155245062f05e2f355fadd1d16f04cd56/alonzo/impl/cddl-files/alonzo.cddl 

## Environment setup
this project is currently not nix-ified.

in the meantime, here are the setup instructions to ensure a consistent environment:
1. install node 14.17.3  - recommend using [Node Version Manager](https://github.com/nvm-sh/nvm)
2. install Docker, using the recommended method for your Operating System
3. `npm i -g purescript@0.13.8 spago@0.20.1`
4. steps 3-16 are to set up the Cardano environment for the public testnet. Private testnets will require additional documentation. For these steps, work in a sibling directory:
5. `git clone git@github.com:mlabs-haskell/cardano-infrastructure.git`
6. `cd cardano-infrastructure`
7. `export CARDANO_NODE_SOCKET_PATH=$PWD/node/socket/node.socket`
8. `./node/start.sh`
9. Query the tip to see if the node is fully synced (in a new shell from `cardano-infrastructure`) `cardano-cli query tip --testnet-magic 1097911063 --socket node/socket/node.socket`

if the node is fully synced, you will see:
```
{   "epoch": 1005,
    "hash": "162d6541cc5aa6b0e098add8fa08a94660a08b9463c0a86fcf84661b5f63375f",
    "slot": 7232440,
    "block": 322985,
    "era": "Alonzo",
    "syncProgress": "100.00"
}
```

In particular, `syncProgress` is the important part here.

10. continuing in your new shell, repeat step 7 to define `CARDANO_NODE_SOCKET_PATH` in your new shell, if it is not currently defined in this environment

11. in `cardano-infrastructure` - copy the following files
- https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/test-node/testnet/testnet-config.json
- https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/test-node/testnet/testnet-byron-genesis.json
- https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/test-node/testnet/testnet-shelley-genesis.json

12. `export CARDANO_NODE_CONFIG=$PWD/config.json` (the `ogmios` command will automatically search for the genesis files as siblings to the config file)
13. `git clone git@github.com:mlabs-haskell/ogmios.git` (currently we use an Ogmios Fork for querying Utxo's and Datum from the Cardano network)
14. `cd ogmios` 
15. `nix build --extra-experimental-features nix-command --extra-experimental-features flakes`
16. `./result/bin/ogmios --node-socket $CARDANO_NODE_SOCKET_PATH --node-config $CARDANO_NODE_CONFIG`


## Setup

This project is not currently using nix, setup depends on Node version 14.17.3, purescript version 0.13.8, spago version 0.20.1.
```
spago install
npm install
spago build
```

The build output is a library which can be used in a browser frontend such as the example testbed linked above

To check runtime behaviors currently, you can run `spago repl`, or you can write some javascript in `scratch.js` and run `node scratch.js`

## Testing

`npm run test` will run the test suite.

## Architecture
So if we think of pab as a library instead of as a standalone process there are really just a few problems to consider:

1. How do we get the transaction in the right format - this is handled by cardano-serialization-lib,  a rust library available as wasm
2. How do we query the chain - Ogmios or BlockFrost api integration,   if these services don't have a permissive CORS setting,  the user/developer needs to provide the url for a proxy server.
3. Querying Datum may require chain-index or blockfrost, as ogmios does not support this feature.
note: this may have limitations for private testnets where Ogmios or blockfrost services do not yet exist
4. How do we submit the transaction - through the light wallet integration in the browser based on cip-30
5. The lingering question is around storage solutions if needed - this can be in memory,  in various browser storage solutions,  or a decentralized db like flurry

The main goal of the library is to provide a reasonable interface to build and balance a transaction manually

In the first iteration, we just want a library interface to achieve this with Nami so we can start shipping

In the second iteration we will want to support multiple wallets and automatic balancing, rather than manual.

In the third iteration,  we want to support an interface that matches the original pab so people can easily port their code over. This will likely Not be a compiled eDSL in PureScript.   Library code in a Promise Monad is much more likely.

We will support both a PureScript and a JavaScript api.

