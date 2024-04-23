import * as esbuild from "esbuild";
import { wasmLoader } from "esbuild-plugin-wasm";
import { polyfillNode } from "esbuild-plugin-polyfill-node";

esbuild.build({
  entryPoints: ["api/index.ts"],
  outfile: "dist/index.js",
  define: {
    BROWSER_RUNTIME: "1"
  },
  loader: {
    ".plutus": "text"
  },
  plugins: [
    polyfillNode({
      polyfills: {
        crypto: true,
        fs: true,
        os: true
      }
    }),
    wasmLoader({
      mode: "embedded"
    })
  ],
  bundle: true,
  platform: "browser",
  format: "esm",
  treeShaking: true,
  logLevel: "error"
});
