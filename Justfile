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

# API docs for the exposed modules.
docs:
    "{{ roc }}" docs package/main.roc
