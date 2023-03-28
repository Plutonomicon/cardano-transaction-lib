/* global BROWSER_RUNTIME */
let IN_BROWSER;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  IN_BROWSER = true;
} else {
  IN_BROWSER = false;
}

exports._localStorage = nothing => just => ({
  getItem: function (key) {
    return function () {
      if (!IN_BROWSER) {
        throw "Not in browser runtime. Bundle with BROWSER_RUNTIME=1";
      }
      const value = localStorage.getItem(key);
      return value === null ? nothing : just(value);
    };
  },
  setItem: function (key) {
    return function (value) {
      return function () {
        if (!IN_BROWSER) {
          throw "Not in browser runtime. Bundle with BROWSER_RUNTIME=1";
        }
        localStorage.setItem(key, value);
      };
    };
  },
});
