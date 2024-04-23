import * as esbuild from "esbuild";

esbuild.build({
  entryPoints: ["src/index.ts"],
  outfile: "index.js",
  plugins: [],
  bundle: true,
  platform: "browser",
  format: "esm",
  treeShaking: true,
  logLevel: "error"
});
