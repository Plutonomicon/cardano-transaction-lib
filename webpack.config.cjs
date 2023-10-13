"use strict";

const path = require("path");
const webpack = require("webpack");
const NodePolyfillPlugin = require("node-polyfill-webpack-plugin");

module.exports = env => {
  return {
    mode: "development",

    experiments: {
      asyncWebAssembly: false,
      layers: false,
      lazyCompilation: false,
      outputModule: true,
      syncWebAssembly: true,
      topLevelAwait: true,
    },

    devtool: "eval-source-map",

    stats: { errorDetails: true },

    devServer: {
      static: {
        directory: path.join(__dirname, 'dist'),
      },
      client: {
        overlay: false
      },
      port: 4008,
      proxy: {
        "/kupo": {
          // `KUPO_HOST` env variable must be set to the base URL of the Kupo
          // service, otherwise all requests to Kupo will fail.
          target: process.env.KUPO_HOST || "http://localhost:1442",
          changeOrigin: true,
          pathRewrite: { "^/kupo": "" },
        },
      },
    },

    entry: env.entry,

    output: {
      path: path.resolve(__dirname, "dist"),
      filename: "index.js",
      library: {
        type: "module",
      }
    },

    module: {
      rules: [
        {
          test: /\.(png|jpg|gif)$/i,
          type: "asset",
        },
        {
          test: /\.plutus$/i,
          type: "asset/source",
        },
      ],
    },

    resolve: {
      // We use node_modules provided by Nix shell via an environment variable
      modules: [process.env.NODE_PATH],
      extensions: [".js"],
      fallback: {
        buffer: require.resolve("buffer/"),
        http: false,
        url: false,
        stream: false,
        crypto: false,
        https: false,
        net: false,
        tls: false,
        zlib: false,
        os: false,
        path: false,
        fs: false,
        readline: false,
        child_process: false,
      },
    },

    plugins: [
      new webpack.DefinePlugin({
        BROWSER_RUNTIME: !!process.env.BROWSER_RUNTIME,
      }),
      new NodePolyfillPlugin(),
      new webpack.LoaderOptionsPlugin({
        debug: true,
      }),
      new webpack.ProvidePlugin({
        Buffer: ["buffer", "Buffer"],
      }),
      // ContextReplacementPlugin is used just to suppress a webpack warning:
      // "Critical dependency: the request of a dependency is an expression"
      // See https://stackoverflow.com/a/59235546/17365145
      new webpack.ContextReplacementPlugin(/cardano-serialization-lib-browser/),
      new webpack.ContextReplacementPlugin(/cardano-serialization-lib-nodejs/),
    ],
  };
};
