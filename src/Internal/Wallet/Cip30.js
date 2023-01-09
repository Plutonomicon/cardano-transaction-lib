/* global BROWSER_RUNTIME */

exports._getNetworkId = conn => () => conn.getNetworkId();

exports._getUtxos = maybe => conn => amount => paginate => () =>
  conn
    .getUtxos(amount, paginate)
    .then(res => (res === null ? maybe.nothing : maybe.just(res)))
    .catch(_ => maybe.nothing);

exports._getCollateral = maybe => conn => amount => () =>
  conn.experimental
    .getCollateral({ amount: amount })
    .then(utxos =>
      utxos !== null && utxos.length ? maybe.just(utxos) : maybe.nothing
    )
    .catch(_ => maybe.nothing);

exports._getBalance = conn => () => conn.getBalance();

exports._getAddresses = conn => paginate => () =>
  conn.getUsedAddresses(paginate).catch(_ => []);

exports._getUnusedAddresses = conn => () => conn.getUnusedAddresses();

exports._getChangeAddress = conn => () => conn.getChangeAddress();

exports._getRewardAddresses = conn => () => conn.getRewardAddresses();

exports._signTx = txHex => partialSign => conn => () =>
  conn.signTx(txHex, partialSign).catch(e => {
    throw JSON.stringify(e);
  });

exports._signData = address => payload => conn => () =>
  conn.signData(address, payload).catch(e => {
    throw JSON.stringify(e);
  });
