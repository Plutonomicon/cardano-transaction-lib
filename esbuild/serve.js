import * as esbuild from "esbuild";
import { buildOptions } from "./config.js";

if (process.argv.length < 7) {
  throw `usage: nodejs serve.js PURESCRIPT_MODULE OUTPUT_DIR SERVE_DIR HOST PORT`;
}

const ctx = await esbuild.context(
  buildOptions({
    pursEntryPoint: process.argv[2],
    outputDir: process.argv[3],
  })
);

const config = {
  host: process.argv[5],
  port: process.argv[6],
  servedir: process.argv[4],
};

console.log("serving:", config);

await ctx.serve(config);
