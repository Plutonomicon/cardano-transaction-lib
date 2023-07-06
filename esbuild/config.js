import * as esbuild from "esbuild";
import { wasmLoader } from "esbuild-plugin-wasm";
import { polyfillNode } from "esbuild-plugin-polyfill-node";
import { fileURLToPath } from "node:url";

export const buildOptions = {
  entryPoints: [`output/${process.argv[2]}/index.js`],
  outdir: "dist/esbuild",
  define: {
    BROWSER_RUNTIME: process.env.BROWSER_RUNTIME,
    SCRIPTS_DIR: '"fixtures/scripts/"',
  },
  plugins: [
    polyfillNode({
      polyfills: {
        crypto: true,
        fs: true,
        os: true,
      },
    }),
    wasmLoader({
      mode: "deferred",
    }),
  ],
  bundle: true,
  platform: "browser",
  format: "esm",
  treeShaking: true,
  logLevel: "warning",
};
