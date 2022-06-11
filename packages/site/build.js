import * as fs from "fs/promises";
import * as path from "path";

const dir = "public";
await fs.mkdir(dir, { recursive: true }); // don't throw if it already exists
const filename = "tree-sitter.wasm";
await fs.copyFile(
  path.join("../../node_modules/web-tree-sitter", filename),
  path.join(dir, filename)
);
