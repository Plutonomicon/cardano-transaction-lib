/* global BROWSER_RUNTIME */

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/datum42.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve(__dirname, "../../fixtures/scripts/datum42.plutus"),
    "utf8"
  );
}

exports.datum42 = script;
