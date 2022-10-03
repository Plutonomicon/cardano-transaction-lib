/* global BROWSER_RUNTIME */

exports._getNetworkId = conn => () => conn.getNetworkId();

exports._getUtxos = maybe => conn => () =>
  conn.getUtxos().then(res => (res === null ? maybe.nothing : maybe.just(res)));

exports._getCollateral = maybe => conn => () =>
  conn.experimental.getCollateral().then(utxos => {
    return utxos.length ? maybe.just(utxos) : maybe.nothing;
  });

exports._getBalance = conn => () => conn.getBalance();

exports._getAddresses = conn => conn.getUsedAddresses;

exports._getUnusedAddresses = conn => () => conn.getUnusedAddresses();

exports._getChangeAddress = conn => () => conn.getChangeAddress();

exports._getRewardAddresses = conn => () => conn.getRewardAddresses();

exports._signTx = txHex => conn => () => {
  return conn.signTx(txHex, true).catch(e => {
    console.log("Error in signTx: ", e);
    throw JSON.stringify(e);
  });
};

exports._signData = address => payload => conn => () => {
  return conn.signData(address, payload).catch(e => {
    console.log("Error in signData: ", e);
    throw JSON.stringify(e);
  });
};
