/* global BROWSER_RUNTIME */

const getIsWalletAvailableFunctionName = wallet => {
  const strs = {
    nami: "isNamiWalletAvailable",
    gerowallet: "isGeroWalletAvailable",
  };

  return strs[wallet] || "is?WalletAvailable";
};

const nodeEnvError =
  "`window` is not an object. Are you trying to run a Contract with" +
  " connected light wallet in NodeJS environment?";

const checkNotNode = () => {
  if (typeof window != "object") {
    throw nodeEnvError;
  }
};

const enableWallet = wallet => () => {
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
};

exports._enableNami = enableWallet("nami");

exports._enableGero = enableWallet("gerowallet");

exports._enableFlint = enableWallet("flint");

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
