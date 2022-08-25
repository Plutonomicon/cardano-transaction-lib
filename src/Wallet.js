/* global BROWSER_RUNTIME */

const getIsWalletAvailableFunctionName = wallet => {
  const strs = {
    nami: "isNamiWalletAvailable",
    gerowallet: "isGeroWalletAvailable",
    flint: "isFlintWalletAvailable",
    LodeWallet: "isLodeWalletAvailable",
  };

  return strs[wallet] || "is?WalletAvailable";
};

const wallets = {
  nami: "nami",
  flint: "flint",
  gero: "gerowallet",
  lode: "LodeWallet",
};

const nodeEnvError =
  "`window` is not an object. Are you trying to run a Contract with" +
  " connected light wallet in NodeJS environment?";

const checkNotNode = () => {
  if (typeof window != "object") {
    throw nodeEnvError;
  }
};

const delay = ms => new Promise(resolve => setTimeout(resolve, ms));

const enableWallet = wallet => () => {
  return delay(100).then(() => {
    const isAvailable = isWalletAvailable(wallet)();
    if (isAvailable) {
      return window.cardano[wallet].enable().catch(e => {
        throw (
          "enableWallet failed: " +
          (typeof e.info == "string" ? e.info : e.toString())
        );
      });
    } else {
      throw (
        "Wallet is not available. Use `" +
        getIsWalletAvailableFunctionName(wallet) +
        "` before connecting."
      );
    }
  });
};

exports._enableNami = enableWallet(wallets.nami);
exports._enableGero = enableWallet(wallets.gero);
exports._enableFlint = enableWallet(wallets.flint);
exports._enableLode = enableWallet(wallets.lode);

const isWalletAvailable = walletName => () => {
  checkNotNode();
  return (
    typeof window.cardano != "undefined" &&
    typeof window.cardano[walletName] != "undefined" &&
    typeof window.cardano[walletName].enable == "function"
  );
};

exports._isNamiAvailable = isWalletAvailable(wallets.nami);
exports._isGeroAvailable = isWalletAvailable(wallets.gero);
exports._isFlintAvailable = isWalletAvailable(wallets.flint);
exports._isLodeAvailable = isWalletAvailable(wallets.lode);
