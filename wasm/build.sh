#!/usr/bin/env bash
# Build the package into a wasm module, so its size can be measured.
#
# There is no wasm target in this repo's own gates — the package declares no
# platform and cannot. This rig supplies the smallest platform that will link:
# a Rust host providing only the roc ABI shims plus one hosted call, so what the
# module weighs is the package plus a rounding error, not a framework.
#
# Adapted from roc-solid's `wasm/gatezero`, which is where the README's table
# was originally measured. The steps are the roc compiler's own RustGlue
# wasm-host recipe: glue -> rustc staticlib -> filter to real wasm members ->
# wasm-ld -r -> roc build --target=wasm32.
set -euo pipefail
cd "$(dirname "$0")"

ROC="${ROC:-$HOME/.bin/roc}"
RUST_GLUE="${RUST_GLUE:-$HOME/.bin/RustGlue.roc}"
# BSD ar cannot extract rustc's wasm32 staticlib; use LLVM ar.
AR="${AR:-$(command -v llvm-ar || echo /opt/homebrew/opt/llvm/bin/llvm-ar)}"
OUT="${OUT:-build/app.wasm}"

echo "== 1. glue =="
rm -rf build glue-out
mkdir -p build glue-out platform/targets/wasm32
"$ROC" glue "$RUST_GLUE" glue-out platform/main.roc
cp glue-out/roc_platform_abi.rs host/roc_platform_abi.rs

echo "== 2. rustc host -> wasm32 staticlib =="
rustc --edition=2021 \
    --target wasm32-unknown-unknown \
    -C panic=abort \
    --crate-type staticlib \
    -C opt-level=3 \
    host/host.rs \
    -o build/host.a

echo "== 3. filter to wasm-only members, relocatable-merge into host.wasm =="
rm -rf build/members
mkdir -p build/members
"$AR" x --output=build/members build/host.a
wasm_members=()
for m in build/members/*; do
    [ -f "$m" ] || continue
    if [ "$(head -c 4 "$m" | xxd -p)" = "0061736d" ]; then
        wasm_members+=("$m")
    fi
done
echo "   wasm members kept: ${#wasm_members[@]}"
rm -f build/host.wasm-only.a
"$AR" rcs build/host.wasm-only.a "${wasm_members[@]}"
wasm-ld -r --whole-archive build/host.wasm-only.a -o platform/targets/wasm32/host.wasm

echo "== 4. roc build --target=wasm32 =="
"$ROC" build --target=wasm32 --no-cache app/main.roc
mv -f main.wasm "$OUT" 2>/dev/null || mv -f app/main.wasm "$OUT"
echo "   $OUT: $(wc -c < "$OUT") bytes"
