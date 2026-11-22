# roc-markdown build & test tasks. Run `just` to list recipes.

roc := env("ROC", "roc")

default:
    @just --list

# The package's gate — the hand-written expects, the source-location
# invariants (`LocTest.roc`) and the href policy (`SchemesTest.roc`), which the
# spec suite cannot see. Builds nothing.
test:
    "{{ roc }}" test package/Test.roc
    "{{ roc }}" test package/LocTest.roc
    "{{ roc }}" test package/SchemesTest.roc

# Conformance — 676 examples, CommonMark 0.31.2 + GFM. Red at 672 by design.
spec:
    "{{ roc }}" test package/SpecTest.roc

# The conformance failures bucketed by spec section, with per-example diffs.
spec-report: spec-regen
    ROC="{{ roc }}" python3 spec/report.py

# Regenerate `SpecTest.roc` (committed) and `SpecDbg.roc` (gitignored).
spec-regen:
    python3 spec/generate.py

# Type-check the examples — the only check on the package's exposed surface.
check-examples:
    "{{ roc }}" check examples/md2html.roc

# Convert markdown files to HTML beside themselves: `just md2html README.md`.
md2html +file:
    "{{ roc }}" examples/md2html.roc -- {{ file }}

# Everything that gates. `spec` is NOT in it — it is the target, and it is red.
check: test check-examples

# Build the package to wasm and report its section sizes — the numbers behind
# the README's "Build size" table. `app` is a path under `wasm/`:
# `app/main.roc` links the package, `probe/main.roc` links only the platform,
# which is the control that separates the package's weight from the harness's.
#
# Uses `roc`: the wasm backend regression that once forced a pinned compiler
# (`release-fast-cb55b063`) was fixed between `release-fast-ec598185` (Oct 22,
# where `--target=wasm32` segfaulted on this app) and `release-fast-f20c8640`
# (Nov 17). Needs `rustc` with the wasm32-unknown-unknown target, `wasm-ld`,
# and `llvm-ar` — BSD `ar` cannot extract rustc's wasm32 staticlib.
wasm-size app="app/main.roc":
    ROC="{{ roc }}" APP="{{ app }}" OUT=build/out.wasm bash wasm/build.sh
    @printf 'app\ttotal\tcode\tdata\n'
    @python3 wasm/sections.py wasm/build/out.wasm {{ app }}

# Build the same module and RUN it under node, which is what proves the
# Roc -> wasm32 -> Rust-host chain works rather than merely links.
wasm-run app="app/main.roc": (wasm-size app)
    node wasm/run.mjs wasm/build/out.wasm

# API docs for the exposed modules.
docs:
    "{{ roc }}" docs package/main.roc
