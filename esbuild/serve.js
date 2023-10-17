import * as esbuild from "esbuild";
import { buildOptions } from "./config.js";

if (process.argv.length < 6) {
  throw `usage: nodejs serve.js ENTRY_POINT OUTPUT_FILE SERVE_DIR PORT`;
}

const ctx = await esbuild.context(
  buildOptions({
    entryPoint: process.argv[2],
    outfile: process.argv[3],
  })
);

const config = {
  port: parseInt(process.argv[5]),
  servedir: process.argv[4],
};

console.log("serving:", config);

await ctx.serve(config);
