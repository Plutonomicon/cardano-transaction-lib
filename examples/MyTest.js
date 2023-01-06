let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

lib = require("csl-runtime-gc")(lib);

window.cslLib = lib;

const loopEffect = () => {
  const arr = [];
  console.log("looping");
  for (let i = 0; i < 20; i++) {
    const x = new Uint8Array(Array(10000000).fill(0));
    const pd = lib.PlutusData.new_bytes(x);
    arr.push(pd);
  }
  console.log(lib.__gcPointerStore.store.size);
  return arr;
};

exports.loopEffect = loopEffect;
