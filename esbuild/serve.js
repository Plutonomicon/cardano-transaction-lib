import * as esbuild from "esbuild";
import { buildOptions } from "./config.js";

const ctx = await esbuild.context(buildOptions);
await ctx.serve({
  host: "127.0.0.1",
  port: 4008,
  servedir: "dist/esbuild"
})

