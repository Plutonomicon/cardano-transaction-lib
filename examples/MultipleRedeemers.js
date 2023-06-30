/* global BROWSER_RUNTIME */

let vredeemerInt1, vredeemerInt2, vredeemerInt3;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  vredeemerInt1 = require("Scripts/redeemer1-validator.plutus");
  vredeemerInt2 = require("Scripts/redeemer2-validator.plutus");
  vredeemerInt3 = require("Scripts/redeemer3-validator.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  const readScript = name =>
    fs.readFileSync(
      path.resolve(__dirname, `../../fixtures/scripts/${name}.plutus`),
      "utf8"
    );
  vredeemerInt1 = readScript("redeemer1-validator");
  vredeemerInt2 = readScript("redeemer2-validator");
  vredeemerInt3 = readScript("redeemer3-validator");
}

export {vredeemerInt1};
export {vredeemerInt2};
export {vredeemerInt3};
