/* global BROWSER_RUNTIME */

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/check-datum-is-inline.plutus");
} else {
  const fs = await import("fs");
  script = fs.readFileSync(
    new URL(
      "../../fixtures/scripts/check-datum-is-inline.plutus",
      import.meta.url
    ),
    "utf8"
  );
}

export { script as checkDatumIsInline };
