# Cardano Browser Tx

## Goals:

1. build a transaction in the browser that works with at least 1 light wallet (Nami).
2. once we can construct a simple user-to-user transaction, we will try to use the library to submit the Tx with nami. 
3. Once we have a simple working transaction, we will seek to build a Plutus Contract transaction With datum from scratch. 
4. Once we can construct Plutus Contract transactions, we will seek to build a library/dsl/interface such that transactions can be built using constraints and lookups - as close as possible to a cut-and-paste solution from `Contract` Monad code in haskell (but with no guaruntee that code changes are not necessary)

## resources/tools:
  - Cardano-serialization-lib (Sundae fork:https://github.com/SundaeSwap-finance/cardano-serialization-lib)
  - ogmios - for querying the chain - https://ogmios.dev 
  - example testbed - https://github.com/Benjmhart/nami-integration 
  - CIP-30 (Wallet interface - nami partially implements this) -https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030
  - Nami docs - https://github.com/Berry-Pool/nami-wallet 

## Setup

This project is not currently using nix, setup depends on Node version 14.17.3, purescript version 0.13.8, spago version 0.20.1.
```
spago install
npm install
spago build
```

The build output is a library which can be used in a browser frontend such as the example testbed linked above
