/* eslint-disable no-global-assign */

export function injectCip30Mock(walletName) {
  return mock => () => {
    let window_ = typeof window != "undefined" ? window : (global.window = {});

    if (
      typeof window_ == "object" &&
      typeof window_.cardano == "object" &&
      typeof window_.cardano[walletName] != "undefined"
    ) {
      throw (
        "injectCip30Mock: refusing to overwrite existing wallet (" +
        walletName +
        ")"
      );
    }

    window_.cardano = {};
    window_.cardano[walletName] = {
      enable: () => {
        return new Promise((resolve, _reject) =>
          resolve({
            getNetworkId: mock.getNetworkId,
            getUtxos: mock.getUtxos,
            experimental: {
              getCollateral: mock.getCollateral
            },
            getBalance: mock.getBalance,
            getUsedAddresses: mock.getUsedAddresses,
            getUnusedAddresses: mock.getUnusedAddresses,
            getChangeAddress: mock.getChangeAddress,
            getRewardAddresses: mock.getRewardAddresses,
            signTx: mock.signTx,
            signData: mock.signData
          })
        );
      }
    };

    return () => {
      delete window_.cardano[walletName];
    };
  };
}
