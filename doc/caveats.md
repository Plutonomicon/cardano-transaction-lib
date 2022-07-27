This document lists common problems encountered by CTL users and developers.

# Time-related

## cardano-node lags

Local `cardano-node` lags behind the global network time, so when using time conversion functions (`slotToPosixTime`, `posixTimeToSlot`, etc.) users should be aware that the node sees time differently from the OS.
During normal runs, the lag can be somewhere between 0 and 200 seconds.

To do anything time-related, it's best to query current slot with `chainTip` and convert it to posix time, instead of using `Date.now()` as a source of truth. This is almost a requirement when using `mustValidateIn`, because the node will reject the transaction if it appears too early.

