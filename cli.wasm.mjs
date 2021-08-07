import fs from "fs";
export default fs.promises.readFile("cli.wasm").then(bufferSource => WebAssembly.compile(bufferSource));
