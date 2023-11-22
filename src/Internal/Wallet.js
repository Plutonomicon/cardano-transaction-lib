/* global BROWSER_RUNTIME */

function getWindow() {
  return typeof window != "undefined" ? window : global.window_;
}

const getIsWalletAvailableTagName = wallet => {
  const strs = {
    nami: "NamiWallet",
    gerowallet: "GeroWallet",
    flint: "FlintWallet",
    LodeWallet: "LodeWallet",
    eternl: "EternlWallet"
  };

  return strs[wallet] || "is?WalletAvailable";
};

const nodeEnvError = new Error(
  "`window` is not an object. Are you trying to run a Contract with" +
    " connected light wallet in NodeJS environment?"
);

const checkNotNode = () => {
  if (typeof getWindow() != "object") {
    throw nodeEnvError;
  }
};

const enableWallet = wallet => () => {
  if (isWalletAvailable(wallet)()) {
    return getWindow()
      .cardano[wallet].enable()
      .catch(e => {
        throw new Error(
          "enableWallet failed: " +
            (typeof e.info == "string" ? e.info : e.toString())
        );
      });
  } else {
    throw new Error(
      "Wallet is not available. Use `isWalletAvailable " +
        getIsWalletAvailableTagName(wallet) +
        "` before connecting."
    );
  }
};

export { enableWallet as _enableWallet };

const isWalletAvailable = walletName => () => {
  checkNotNode();
  return (
    typeof getWindow().cardano != "undefined" &&
    typeof getWindow().cardano[walletName] != "undefined" &&
    typeof getWindow().cardano[walletName].enable == "function"
  );
};

export { isWalletAvailable as _isWalletAvailable };

export function _isEnabled(walletName) {
  return () => {
    if (isWalletAvailable(walletName)()) {
      return getWindow().cardano[walletName].isEnabled();
    } else {
      throw new Error("Wallet `" + walletName + "` is not available");
    }
  };
}

export function _apiVersion(walletName) {
  return () => {
    if (isWalletAvailable(walletName)()) {
      return getWindow().cardano[walletName].apiVersion;
    } else {
      throw new Error("Wallet `" + walletName + "` is not available");
    }
  };
}

export function _name(walletName) {
  return () => {
    if (isWalletAvailable(walletName)()) {
      return getWindow().cardano[walletName].name;
    } else {
      throw new Error("Wallet `" + walletName + "` is not available");
    }
  };
}

export function _icon(walletName) {
  return () => {
    if (isWalletAvailable(walletName)()) {
      return getWindow().cardano[walletName].icon;
    } else {
      throw new Error("Wallet `" + walletName + "` is not available");
    }
  };
}
