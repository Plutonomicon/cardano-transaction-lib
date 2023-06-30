/* global BROWSER_RUNTIME */

import XHR from "xhr2";
import urllib from "url";

const browserDriver = {
  newXHR: function () {
    return new XMLHttpRequest();
  },
  fixupUrl: function (url) {
    return url || "/";
  },
};

const nodeDriver = {
  newXHR: function () {
    return new XHR();
  },
  fixupUrl: function (url, xhr) {
    if (xhr.nodejsBaseUrl === null) {
      let u = urllib.parse(url);
      u.protocol = u.protocol || "http:";
      u.hostname = u.hostname || "localhost";
      return urllib.format(u);
    } else {
      return url || "/";
    }
  },
};

export const driver =
  typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME
    ? browserDriver
    : nodeDriver;
