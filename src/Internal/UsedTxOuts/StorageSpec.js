/* global BROWSER_RUNTIME */

let IN_BROWSER;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  IN_BROWSER = true;
} else {
  IN_BROWSER = false;
}

exports._defaultStorageSpec = local => file => IN_BROWSER ? local : file;
