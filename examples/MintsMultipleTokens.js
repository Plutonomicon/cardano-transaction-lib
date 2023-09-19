/* global BROWSER_RUNTIME */

let redeemerInt1, redeemerInt2, redeemerInt3;

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  redeemerInt1 = require("Scripts/redeemer1.plutus");
  redeemerInt2 = require("Scripts/redeemer2.plutus");
  redeemerInt3 = require("Scripts/redeemer3.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  const readScript = name =>
    fs.readFileSync(
      path.resolve(__dirname, `../../fixtures/scripts/${name}.plutus`),
      "utf8"
    );
  redeemerInt1 = readScript("redeemer1");
  redeemerInt2 = readScript("redeemer2");
  redeemerInt3 = readScript("redeemer3");
}

exports.redeemerInt1 = redeemerInt1;
exports.redeemerInt2 = redeemerInt2;
exports.redeemerInt3 = redeemerInt3;
