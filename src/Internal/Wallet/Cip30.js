/* global BROWSER_RUNTIME */

exports._getNetworkId = conn => () => conn.getNetworkId();

exports._getUtxos = maybe => conn => () =>
  conn.getUtxos().then(res => (res === null ? maybe.nothing : maybe.just(res)));

exports._getCollateral = maybe => conn => () =>
  // yoroi will throw an error if no argument is provided
  // typhon will throw an error if the argument is not a string
  (conn.getCollateral
    ? conn.getCollateral("3000000")
    : conn.experimental.getCollateral("3000000")
  ).then(utxos =>
    utxos !== null && utxos.length ? maybe.just(utxos) : maybe.nothing
  );

exports._getBalance = conn => () => conn.getBalance();

exports._getAddresses = conn => () => conn.getUsedAddresses();

exports._getUnusedAddresses = conn => () => conn.getUnusedAddresses();

exports._getChangeAddress = conn => () => conn.getChangeAddress();

exports._getRewardAddresses = conn => () => conn.getRewardAddresses();

exports._signTx = txHex => conn => () =>
  conn.signTx(txHex, true).catch(e => {
    throw JSON.stringify(e);
  });

exports._signData = address => payload => conn => () =>
  conn.signData(address, payload).catch(e => {
    throw JSON.stringify(e);
  });
