/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-browser");
} else {
  lib = await import("@mlabs-haskell/cardano-serialization-lib-gc-nodejs");
}

import process from "process";

export const testExternalMemLeakImpl = config => async () => {
  let externalMemUpperBound = null;

  for (let i = 0; i < config.numIterations; i++) {
    // console.log(`\nIteration ${i}`);

    for (let j = 0; j < config.numArrays; j++) {
      let bytes = new Uint8Array(Array(config.arrSize).fill(0));
      lib.PlutusData.new_bytes(bytes);
    }

    await new Promise(r => setTimeout(r, config.delay));

    const memUsage = process.memoryUsage();
    // console.log(memUsage);

    if (i == config.refIteration) {
      externalMemUpperBound = memUsage.external * (1 + config.maxError / 100);
    }
    if (i > config.refIteration && memUsage.external > externalMemUpperBound) {
      throw new Error("External memory leak detected.");
    }
  }
};
