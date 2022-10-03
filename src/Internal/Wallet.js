/* global BROWSER_RUNTIME */

const getIsWalletAvailableFunctionName = wallet => {
  const strs = {
    nami: "isNamiWalletAvailable",
    gerowallet: "isGeroWalletAvailable",
    flint: "isFlintWalletAvailable",
    LodeWallet: "isLodeWalletAvailable",
    eternl: "isEternlWalletAvailable",
  };

  return strs[wallet] || "is?WalletAvailable";
};

//Keep it in sync with Wallet.supportedWalletToName
const wallets = {
  nami: "nami",
  flint: "flint",
  gero: "gerowallet",
  lode: "LodeWallet",
  eternl: "eternl",
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

exports._enableNami = enableWallet(wallets.nami);
exports._enableGero = enableWallet(wallets.gero);
exports._enableFlint = enableWallet(wallets.flint);
exports._enableLode = enableWallet(wallets.lode);
exports._enableEternl = enableWallet(wallets.eternl);

const isWalletAvailable = walletName => () => {
  checkNotNode();
  return (
    typeof window.cardano != "undefined" &&
    typeof window.cardano[walletName] != "undefined" &&
    typeof window.cardano[walletName].enable == "function"
  );
};

exports._isWalletAvailable = isWalletAvailable;

exports._isNamiAvailable = isWalletAvailable(wallets.nami);
exports._isGeroAvailable = isWalletAvailable(wallets.gero);
exports._isFlintAvailable = isWalletAvailable(wallets.flint);
exports._isLodeAvailable = isWalletAvailable(wallets.lode);
exports._isEternlAvailable = isWalletAvailable(wallets.eternl);

exports._isEnabled = walletName => () => {
  const isAvailable = isWalletAvailable(walletName)();
  if (isAvailable) {
    return window.cardano[walletName].isEnabled();
  } else {
    throw new Error("Wallet `" + walletName + "` is not available");
  }
};

exports._apiVersion = walletName => () => {
  const isAvailable = isWalletAvailable(walletName)();
  if (isAvailable) {
    return window.cardano[walletName].apiVersion;
  } else {
    throw new Error("Wallet `" + walletName + "` is not available");
  }
};

exports._name = walletName => () => {
  const isAvailable = isWalletAvailable(walletName)();
  if (isAvailable) {
    return window.cardano[walletName].name;
  } else {
    throw new Error("Wallet `" + walletName + "` is not available");
  }
};

exports._icon = walletName => () => {
  const isAvailable = isWalletAvailable(walletName)();
  if (isAvailable) {
    return window.cardano[walletName].icon;
  } else {
    throw new Error("Wallet `" + walletName + "` is not available");
  }
};
