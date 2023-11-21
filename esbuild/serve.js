import * as esbuild from "esbuild";
import { buildOptions } from "./config.js";
import http from "node:http";
import * as url from "url";

if (process.argv.length < 6) {
  throw `usage: node serve.js ENTRY_POINT OUTPUT_FILE SERVE_DIR PORT`;
}

const ctx = await esbuild.context(
  buildOptions({
    entryPoint: process.argv[2],
    outfile: process.argv[3]
  })
);

const config = {
  // Use the next port for esbuild server,
  // we need a port for our proxy (see below).
  port: parseInt(process.argv[5]) + 1,
  servedir: process.argv[4]
};

let { host, port } = await ctx.serve(config);

// Proxy Kupo to esbuild server. This is needed due to CORS

const kupoEndpoint = process.env.KUPO_HOST || "http://localhost:1442";
const kupoHost = url.parse(kupoEndpoint).hostname;
const kupoPort = url.parse(kupoEndpoint).port || 80;

http
  .createServer((req, res) => {
    const options = {
      hostname: host,
      port: port,
      path: req.url,
      method: req.method,
      headers: req.headers
    };

    // Forward each incoming request to esbuild
    const proxyReq = http.request(options, proxyRes => {
      // If esbuild returns "not found" and the path starts from /kupo/
      if (proxyRes.statusCode === 404 && req.url.startsWith("/kupo/")) {
        // Connect to Kupo running on port 1442
        const kupoOptions = {
          hostname: kupoHost,
          port: kupoPort,
          path: req.url.slice("/kupo".length),
          method: req.method,
          headers: req.headers
        };
        // Request the corresponding path
        http
          .request(kupoOptions, kupoRes => {
            // Pipe the response from Kupo back to the client
            res.writeHead(kupoRes.statusCode, kupoRes.headers);
            kupoRes.pipe(res, { end: true });
          })
          .end();
      } else {
        // Otherwise, forward the response from esbuild to the client
        res.writeHead(proxyRes.statusCode, proxyRes.headers);
        proxyRes.pipe(res, { end: true });
      }
    });

    // Forward the body of the request to esbuild
    req.pipe(proxyReq, { end: true });
  })
  .listen(parseInt(process.argv[5]));
