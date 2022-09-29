/* eslint-disable no-global-assign */

exports.injectCip30Mock = walletName => mock => () => {
  const hadWindow = typeof window != "undefined";

  if (
    typeof window == "object" &&
    typeof window.cardano == "object" &&
    typeof window.cardano[walletName] != "undefined"
  ) {
    throw (
      "injectCip30Mock: refusing to overwrite existing wallet (" +
      walletName +
      ")"
    );
  }

  if (typeof window == "undefined") {
    window = { cardano: {} };
  } else if (typeof window.cardano == "undefined") {
    window.cardano = {};
  }

  window.cardano[walletName] = {
    enable: () => {
      return new Promise((resolve, _reject) =>
        resolve({
          experimental: {
            getCollateral: mock.getCollateral,
          },
          signTx: mock.signTx,
          getUsedAddresses: mock.getUsedAddresses,
          getBalance: mock.getBalance,
          getUtxos: mock.getUtxos,
        })
      );
    },
  };

  return () => {
    delete window.cardano[walletName];
    if (!hadWindow) {
      window = undefined;
    }
  };
};
