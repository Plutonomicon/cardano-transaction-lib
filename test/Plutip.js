import stream from "node:stream";

export const readableFromBuffer = buf => () =>
  stream.Readable.from(buf, { objectMode: false });
