exports.injectCip30Mock = walletName => mock => () => {
  window[walletName] = mock;
};
