# roc-markdown build & test tasks. Run `just` to list recipes.

roc := env("ROC", "roc")

default:
    @just --list

# The package's gate — 51 hand-written expects, ~1.2 s, builds nothing.
test:
    "{{ roc }}" test package/Test.roc

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

# API docs for the exposed modules.
docs:
    "{{ roc }}" docs package/main.roc

# The compiler the wasm harness uses.
#
# NOT `roc`: the wasm backend regressed between `release-fast-cb55b063` (Aug 23)
# and `release-fast-c32a5bac` (Aug 25) — newer builds report success and emit a
# 26-byte module holding nothing but a memory section. Verified by building the
# UNMODIFIED upstream gate-zero rig, which produces a real module on cb55b063
# and 26 bytes on every build after it. `cb55b063` is the newest one that works.
wasm_roc := env("WASM_ROC", home_directory() / ".bin/roc-cb55b0")

# Build the package to wasm and report its section sizes — the numbers behind
# the README's "Build size" table. Pass a variant: main (Html5), xml, dyn.
wasm-size variant="main":
    ROC="{{ wasm_roc }}" bash wasm/build.sh
    "{{ wasm_roc }}" build --target=wasm32 --no-cache wasm/app/{{ variant }}.roc
    @mv -f {{ variant }}.wasm wasm/build/{{ variant }}.wasm
    @printf 'variant\ttotal\tcode\tdata\n'
    @python3 wasm/sections.py wasm/build/{{ variant }}.wasm {{ variant }}
