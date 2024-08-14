function getWindow() {
  return typeof window != "undefined" ? window : global.window;
}

const nodeEnvError = new Error(
  "`window` is not an object. Are you trying to run a Contract with" +
    " a connected light wallet in NodeJS environment?"
);

const checkNotNode = () => {
  if (typeof getWindow() != "object") {
    throw nodeEnvError;
  }
};

export const isWalletAvailable = walletName => () => {
  checkNotNode();
  return (
    typeof getWindow().cardano != "undefined" &&
    typeof getWindow().cardano[walletName] != "undefined" &&
    typeof getWindow().cardano[walletName].enable == "function"
  );
};
