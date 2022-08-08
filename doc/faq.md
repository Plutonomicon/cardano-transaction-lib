This document lists common problems encountered by CTL users and developers.

# Bundling-related

## lib.Something is not a function

This is probably because npm is used directly. This is something users have reported when using `npm install` instead of having Nix manage the node dependencies (done automatically with `nix develop`, but if you have `node_modules` present in the working directory it will shadow the ones from the Nix store).

# Time-related

## Q: Time-related functions behave strangely, what's the reason?

Local `cardano-node` lags behind the global network time, so when using time conversion functions (`slotToPosixTime`, `posixTimeToSlot`, etc.) users should be aware that the node sees time differently from the OS.
During normal runs, the lag can be somewhere between 0 and 200 seconds.

To do anything time-related, it's best to rely on local node chain tip time, instead of using `Date.now()` as a source of truth. This is often a requirement when using `mustValidateIn`, because the node will reject the transaction if it appears too early.

# Q: I am getting `Error: (AtKey "coinsPerUtxoByte" MissingValue)`

This is because the node hasn't fully synched. The protocol parameter name changed from `coinsPerUtxoWord` to `coinsPerUtxoByte` in Babbage. CTL only supports the latest era, but Ogmios returns different protocol parameters format depending on current era of a local node.

# Q: Time/slot conversion functions return `Nothing`. Why is that?

Time/slot conversion functions depend on `eraSummaries` [Ogmios local state query](https://ogmios.dev/mini-protocols/local-state-query/), that returns era bounds and slotting parameters details, required for proper slot arithmetic. The most common source of the problem is that Ogmios does not return enough epochs into the future.
