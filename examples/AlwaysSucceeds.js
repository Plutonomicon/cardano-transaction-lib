/* global BROWSER_RUNTIME */

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/always-succeeds.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve(__dirname, "../../fixtures/scripts/always-succeeds.plutus"),
    "utf8"
  );
}

exports.alwaysSucceeds = script;
