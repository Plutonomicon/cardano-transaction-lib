/* global BROWSER_RUNTIME */

exports._enableNami = () => window.cardano.nami.enable();

exports._enableGero = () => window.cardano.gerowallet.enable();

const isWalletAvailable = walletName => () =>
      typeof window.cardano != 'undefined' &&
      typeof window.cardano[walletName] != 'undefined' &&
      typeof window.cardano[walletName].enable == 'function';

exports._isNamiAvailable = isWalletAvailable('nami');

exports._isGeroAvailable = isWalletAvailable('gerowallet');
