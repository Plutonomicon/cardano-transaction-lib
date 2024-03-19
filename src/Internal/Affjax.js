export const driver = nodeDriver;

// we rely on the bundler to inject polyfills for the browser

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
