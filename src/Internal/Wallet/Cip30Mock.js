/* eslint-disable no-global-assign */

// Classes are our implementation detail - CIP-30 only requires object fields
class PaginateError {
  constructor(maxSize) {
    this.maxSize = maxSize;
  }
}

exports.raisePaginateError = maxSize => () => {
  throw new PaginateError(maxSize);
};

const InvalidRequestCode = -1;

class APIError {
  constructor(code, info) {
    this.code = code;
    this.info = info;
  }
}

exports.raiseInvalidRequestError = info => () => {
  throw new APIError(InvalidRequestCode, info);
};

class TxSignError {
  constructor(code, info) {
    this.code = code;
    this.info = info;
  }
}

const ProofGenerationError = 1;

exports.raiseTxSignProofGenerationError = info => () => {
  throw new TxSignError(ProofGenerationError, info);
};

exports._catchPaginateError = eitherpaginateerrorffi => action => () => {
  try {
    return eitherpaginateerrorffi.right(action());
  } catch (error) {
    if (error instanceof PaginateError) {
      return eitherpaginateerrorffi.left(error.maxSize);
    }
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
