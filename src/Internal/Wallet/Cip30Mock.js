/* eslint-disable no-global-assign */

export function injectCip30Mock(walletName) {
  return mock => () => {
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
            getNetworkId: mock.getNetworkId,
            getUtxos: mock.getUtxos,
            experimental: {
              getCollateral: mock.getCollateral,
            },
            getBalance: mock.getBalance,
            getUsedAddresses: mock.getUsedAddresses,
            getUnusedAddresses: mock.getUnusedAddresses,
            getChangeAddress: mock.getChangeAddress,
            getRewardAddresses: mock.getRewardAddresses,
            signTx: mock.signTx,
            signData: mock.signData,
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
}
