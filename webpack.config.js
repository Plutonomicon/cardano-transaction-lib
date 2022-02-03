"use strict";

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
    port: 4008,
  },

  // we can add more entrypoints as needed
  entry: "./src/index.js",

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
              src: ["src/**/*.purs", "test/**/*.purs", "examples/**/*.purs"],
              spago: true,
              watch: isWebpackDevServer || isWatch,
              pscIde: true,
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
    },
  },

  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true,
    }),
    new HtmlWebpackPlugin({
      title: "cardano-browser-tx",
      template: "index.html",
      inject: false, // See stackoverflow.com/a/38292765/3067181
    }),
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
    }),
  ].concat(plugins),
};
