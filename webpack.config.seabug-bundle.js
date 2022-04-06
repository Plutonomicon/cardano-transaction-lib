"use strict";

const NodePolyfillPlugin = require("node-polyfill-webpack-plugin");
const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const webpack = require("webpack");
const isWebpackDevServer = process.argv.some(
  (a) => path.basename(a) === "webpack-dev-server"
);
const isWatch = process.argv.some((a) => a === "--watch");
const plugins =
  isWebpackDevServer || !isWatch
    ? []
    : [
        () => {
          this.plugin("done", (stats) => {
            process.stderr.write(stats.toString("errors-only"));
          });
        },
      ];

module.exports = {
  mode: "production",

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
    port: 4008,
  },

  // we can add more entrypoints as needed
  entry: "./frontend/index.js",

  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "bundle.js",
  },

  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: "purs-loader",
            options: {
              src: ["src/**/*.purs", "seabug_contracts/**/*.purs"],
              spago: true,
              watch: isWebpackDevServer || isWatch,
              pscIde: true,
              bundle: true
            },
          },
        ],
      },
      {
        test: /\.(png|jpg|gif)$/i,
        use: [
          {
            loader: "url-loader",
            options: {
              limit: 8192,
            },
          },
        ],
      },
    ],
  },

  resolve: {
    modules: ["node_modules"],
    extensions: [".purs", ".js"],
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
    },
  },

  plugins: [
    new webpack.DefinePlugin({
      BROWSER_RUNTIME: !!process.env.BROWSER_RUNTIME
    }),
    new NodePolyfillPlugin(),
    new webpack.LoaderOptionsPlugin({
      debug: true,
    }),
    new HtmlWebpackPlugin({
      title: "seabug-frontend",
      template: "./frontend/index.html",
      inject: false, // See stackoverflow.com/a/38292765/3067181
    }),
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
    }),
  ].concat(plugins),
};
