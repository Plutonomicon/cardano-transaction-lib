/* global BROWSER_RUNTIME */

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/check-datum-is-inline.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve(
      __dirname,
      "../../fixtures/scripts/check-datum-is-inline.plutus"
    ),
    "utf8"
  );
}

exports.checkDatumIsInline = script;
