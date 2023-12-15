/* global BROWSER_RUNTIME */

export const driver = async () => {
  if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
    return browserDriver;
  } else {
    return await nodeDriver();
  }
};

const browserDriver = {
  newXHR: function () {
    return new XMLHttpRequest();
  },
  fixupUrl: function (url) {
    return url || "/";
  }
};

async function nodeDriver() {
  const { default: XHR } = await import("xhr2");
  const { default: urllib } = await import("url");

  return {
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
    }
  };
}
