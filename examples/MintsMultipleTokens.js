/* global BROWSER_RUNTIME */

let redeemerInt1, redeemerInt2, redeemerInt3;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  redeemerInt1 = require("Scripts/redeemer1.plutus");
  redeemerInt2 = require("Scripts/redeemer2.plutus");
  redeemerInt3 = require("Scripts/redeemer3.plutus");
} else {
  const fs = await import("fs");
  const readScript = name =>
    fs.readFileSync(
      new URL(`../../fixtures/scripts/${name}.plutus`, import.meta.url),
      "utf8"
    );
  redeemerInt1 = readScript("redeemer1");
  redeemerInt2 = readScript("redeemer2");
  redeemerInt3 = readScript("redeemer3");
}

export { redeemerInt1 };
export { redeemerInt2 };
export { redeemerInt3 };
