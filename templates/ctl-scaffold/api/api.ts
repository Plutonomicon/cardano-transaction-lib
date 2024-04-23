import * as Purs from "../output/Scaffold.Api/index.js";

import type { MyType } from "./types";

export const runContract = async (): Promise<void> => Purs.contractApi();
