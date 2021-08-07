import * as rts from "./rts.mjs";
import module from "./web.wasm.mjs";
import req from "./web.req.mjs";
module.then(m => rts.newAsteriusInstance(Object.assign(req, {module: m}))).then(i => {
i.exports.main().catch(err => {if (!(err.startsWith('ExitSuccess') || err.startsWith('ExitFailure '))) i.fs.writeNonMemory(2, `web: ${err}
`)});
});
