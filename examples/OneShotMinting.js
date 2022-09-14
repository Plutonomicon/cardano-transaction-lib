/* global BROWSER_RUNTIME */

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/one-shot-minting.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve(__dirname, "../../fixtures/scripts/one-shot-minting.plutus"),
    "utf8"
  );
}

exports.oneShotMinting = script;
