/* eslint-disable no-global-assign */

class PaginateError {
  constructor(maxSize) {
    this.maxSize = maxSize;
  }
}

exports.raisePaginateError = maxSize => () => {
  throw new PaginateError(maxSize);
};

class APIError {}

exports.raiseAPIError = function () {
  throw new APIError();
};

exports._catchPaginateError = maybeffi => action => () => {
  try {
    return maybeffi.just(action());
  } catch (PaginateError) {
    return maybeffi.nothing;
  }
};

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
