/* global BROWSER_RUNTIME */

export function _getNetworkId(conn) {
  return () => conn.getNetworkId();
}

export function _getUtxos(maybe) {
  return conn => () =>
    conn
      .getUtxos()
      .then(res => (res === null ? maybe.nothing : maybe.just(res)));
}

export function _getCollateral(maybe) {
  return conn => requiredValue => () =>
    (typeof conn.getCollateral === "function"
      ? conn.getCollateral(requiredValue)
      : conn.experimental.getCollateral(requiredValue)
    ).then(utxos =>
      utxos !== null && utxos.length ? maybe.just(utxos) : maybe.nothing
    );
}

export function _getBalance(conn) {
  return () => conn.getBalance();
}

export function _getAddresses(conn) {
  return conn.getUsedAddresses;
}

export function _getUnusedAddresses(conn) {
  return () => conn.getUnusedAddresses();
}

export function _getChangeAddress(conn) {
  return () => conn.getChangeAddress();
}

export function _getRewardAddresses(conn) {
  return () => conn.getRewardAddresses();
}

export function _signTx(txHex) {
  return conn => () =>
    conn.signTx(txHex, true).catch(e => {
      throw JSON.stringify(e);
    });
}

export function _signData(address) {
  return payload => conn => () =>
    conn.signData(address, payload).catch(e => {
      throw JSON.stringify(e);
    });
}
