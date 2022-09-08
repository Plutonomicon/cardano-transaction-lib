/* global BROWSER_RUNTIME */

const getIsWalletAvailableFunctionName = wallet => {
  const strs = {
    nami: "isNamiWalletAvailable",
    gerowallet: "isGeroWalletAvailable",
  };

  return strs[wallet] || "is?WalletAvailable";
};

const nodeEnvError = new Error(
  "`window` is not an object. Are you trying to run a Contract with" +
    " connected light wallet in NodeJS environment?"
);

const checkNotNode = () => {
  if (typeof window != "object") {
    throw nodeEnvError;
  }
};

const enableWallet = wallet => () => {
  const isAvailable = isWalletAvailable(wallet)();
  if (isAvailable) {
    return window.cardano[wallet].enable().catch(e => {
      throw new Error(
        "enableWallet failed: " +
          (typeof e.info == "string" ? e.info : e.toString())
      );
    });
  } else {
    throw new Error(
      "Wallet is not available. Use `" +
        getIsWalletAvailableFunctionName(wallet) +
        "` before connecting."
    );
  }
};

exports._enableNami = enableWallet("nami");
exports._enableFlint = enableWallet("flint");
exports._enableEternl = enableWallet("eternl");
exports._enableGero = enableWallet("gerowallet");

const isWalletAvailable = walletName => () => {
  checkNotNode();
  return (
    typeof window.cardano != "undefined" &&
    typeof window.cardano[walletName] != "undefined" &&
    typeof window.cardano[walletName].enable == "function"
  );
};

exports._isNamiAvailable = isWalletAvailable("nami");
exports._isGeroAvailable = isWalletAvailable("gerowallet");
exports._isFlintAvailable = isWalletAvailable("flint");
exports._isEternlAvailable = isWalletAvailable("eternl");
