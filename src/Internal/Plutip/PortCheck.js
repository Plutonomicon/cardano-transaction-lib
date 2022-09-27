const net = require("net");

exports._isPortAvailable = port => () =>
  new Promise((resolve, reject) => {
    const server = net
      .createServer()
      .once("error", function (err) {
        if (err.code == "EADDRINUSE") {
          resolve(false);
        } else {
          reject(
            "Failed check for port availability (port: " +
              port +
              ", error: " +
              err.code +
              ")"
          );
        }
      })
      .once("listening", () => {
        server.once("close", () => resolve(true)).close();
      })
      .listen(port);
  });
