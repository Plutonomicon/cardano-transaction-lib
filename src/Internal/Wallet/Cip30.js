/* global BROWSER_RUNTIME */

exports._getNetworkId = conn => () => conn.getNetworkId();

exports._getUtxos = maybe => conn => () =>
  conn.getUtxos().then(res => (res === null ? maybe.nothing : maybe.just(res)));

const getCollateral = conn =>
  conn.getCollateral !== undefined
    ? conn.getCollateral
    : conn.experimental.getCollateral;

exports._getCollateral = maybe => conn => () =>
  getCollateral(conn)().then(utxos =>
    utxos !== null && utxos.length ? maybe.just(utxos) : maybe.nothing
  );

// Yoroi wallet requires an explicit amount as argument to `getCollateral` and
// needs it in the form of `getCollateral(amount)` as opposed to CIP-30's
// `getCollateral(params: { amount: Cbor })`.
exports._getCollateralYoroi = maybe => conn => () =>
  conn
    .getCollateral(3000000)
    .then(utxos =>
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
