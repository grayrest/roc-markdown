// Driver: instantiate the linked module named on the command line (default
// build/app.wasm) in a plain wasm runtime
// (node's WebAssembly, no wasm-bindgen), call the Rust host's wasm_main, and
// read the Roc-produced result string directly out of linear memory.
//
// The final module imports env.roc_panic (and possibly roc_dbg /
// roc_expect_failed) — symbols the minimal host leaves undefined so wasm-ld
// emits them as env imports. We satisfy them from JS.
import { readFileSync } from "node:fs";

const wasmPath = process.argv[2]
  ? new URL(process.argv[2], `file://${process.cwd()}/`)
  : new URL("./build/app.wasm", import.meta.url);
const bytes = readFileSync(wasmPath);

const dec = new TextDecoder();
let memory = null;

const env = {
  roc_panic: (ptr, len) => {
    const msg = memory ? dec.decode(new Uint8Array(memory.buffer, ptr, len)) : "";
    throw new Error("roc panic: " + msg);
  },
  roc_dbg: (ptr, len) => {
    const msg = memory ? dec.decode(new Uint8Array(memory.buffer, ptr, len)) : "";
    console.error("[roc dbg] " + msg);
  },
  roc_expect_failed: (ptr, len) => {
    const msg = memory ? dec.decode(new Uint8Array(memory.buffer, ptr, len)) : "";
    console.error("[roc expect] " + msg);
  },
};

const { instance } = await WebAssembly.instantiate(bytes, { env });
const ex = instance.exports;
memory = ex.memory;

const imports = WebAssembly.Module.imports(await WebAssembly.compile(bytes))
  .map((i) => `${i.module}.${i.name}`);
console.log("imports:", imports.join(", ") || "(none)");

const ptr = ex.wasm_main();
const len = ex.wasm_result_len();
const message = dec.decode(new Uint8Array(memory.buffer, ptr, len));
const n = ex.wasm_n();
const allocs = ex.wasm_alloc_count ? ex.wasm_alloc_count() : -1;
if (ex.wasm_release) ex.wasm_release();

console.log("message:", JSON.stringify(message));
console.log("n:", n);
console.log("roc_alloc calls:", allocs);

// The assertions are app-INDEPENDENT on purpose: this driver runs whichever
// module `build.sh` produced, and the two apps return different payloads
// (`probe` a short string and n = seed * 2, `app` the rendered HTML and n =
// the block count). What it checks is the chain the rig exists to prove —
// the module instantiates, the Roc entrypoint returns a heap string the host
// can read out of linear memory, and `roc_alloc` was genuinely called. Pin an
// exact payload in a test, not here.
let ok = true;
if (message.length === 0) {
  console.error("FAIL: wasm_main returned an empty string");
  ok = false;
}
if (n <= 0n) {
  console.error(`FAIL: expected n > 0, got ${n}`);
  ok = false;
}
if (allocs < 1) {
  console.error(`FAIL: expected >=1 roc_alloc call (heap string), got ${allocs}`);
  ok = false;
}
if (ok) {
  console.log("PASS: Roc->wasm32 linked with a Rust wasm host, called, result read back");
  process.exit(0);
} else {
  process.exit(1);
}
