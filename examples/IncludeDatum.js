/* global BROWSER_RUNTIME */

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
    script = require("Scripts/include-datum.plutus");
} else {
    const fs = require("fs");
    const path = require("path");
    script = fs.readFileSync(
        path.resolve(__dirname, "../../fixtures/scripts/include-datum.plutus"),
        "utf8"
    );
}

exports.includeDatum = script;
