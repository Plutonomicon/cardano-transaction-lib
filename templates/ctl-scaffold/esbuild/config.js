import { wasmLoader } from "esbuild-plugin-wasm";
import { polyfillNode } from "esbuild-plugin-polyfill-node";

const isBrowser = !!process.env.BROWSER_RUNTIME;

export const buildOptions = ({ entryPoint, outfile }) => {
  const config = {
    entryPoints: [entryPoint],
    outfile: outfile,
    define: {
      BROWSER_RUNTIME: isBrowser ? "true" : '""',
    },
    plugins: [
      wasmLoader({
        mode: "deferred",
      }),
    ],
    bundle: true,
    platform: isBrowser ? "browser" : "node",
    format: "esm",
    treeShaking: true,
    logLevel: "error",
  };

  // https://esbuild.github.io/api/#packages
  if (!isBrowser) {
    config.packages = "external";
  } else {
    config.plugins.push(
      polyfillNode({
        polyfills: {
          crypto: true,
          fs: true,
          os: true,
        },
      }),
    );
  }

  return config;
};
