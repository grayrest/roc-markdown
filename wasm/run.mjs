// G7 gate-zero driver: instantiate the linked app.wasm in a plain wasm runtime
// (node's WebAssembly, no wasm-bindgen), call the Rust host's wasm_main, and
// read the Roc-produced result string directly out of linear memory.
//
// The final module imports env.roc_panic (and possibly roc_dbg /
// roc_expect_failed) — symbols the minimal host leaves undefined so wasm-ld
// emits them as env imports. We satisfy them from JS.
import { readFileSync } from "node:fs";

const wasmPath = new URL("./build/app.wasm", import.meta.url);
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

// The Rust host's roc_host_seed returns 21; the app builds "...seed=21" and
// n = 21 * 2 = 42. i64 returns to JS as a BigInt.
const expectedMsg = "roc-solid wasm gate-zero seed=21";
const expectedN = 42n;
let ok = true;
if (message !== expectedMsg) {
  console.error(`FAIL: message mismatch (got ${JSON.stringify(message)})`);
  ok = false;
}
if (n !== expectedN) {
  console.error(`FAIL: n mismatch (got ${n}, want ${expectedN})`);
  ok = false;
}
if (allocs < 1) {
  console.error(`FAIL: expected >=1 roc_alloc call (heap string), got ${allocs}`);
  ok = false;
}
if (ok) {
  console.log("PASS: Roc->wasm32 linked with a Rust wasm host, called, result verified");
  process.exit(0);
} else {
  process.exit(1);
}
