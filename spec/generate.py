#!/usr/bin/env python3
"""Generate the conformance suites from the specs' own example sets.

`just spec-regen`. Two vendored sources, because neither covers the target
alone:

  commonmark-0.31.2.json   the CURRENT CommonMark spec — the base language.
  gfm-extensions.txt       GFM's spec, read for its `(extension)` sections
                           ONLY: tables, task lists, strikethrough, extended
                           autolinks, disallowed raw HTML.

GFM's own file also restates CommonMark, at version 0.29, and that copy is not
used. It is three years stale and disagrees with the current spec in ways that
were later corrected upstream — targeting it would mean reproducing bugs
CommonMark has since fixed. There are no external consumers of this package, so
there is nothing to be compatible with except the format as it now stands.

Writes two Roc files beside the package:

  SpecTest.roc   one `expect` per example. The gate.
  SpecDbg.roc    the same examples, each printing what the parser ACTUALLY
                 produced before comparing. Gitignored; the debugging
                 instrument, not a gate.

Both specs are vendored rather than fetched so the gate is hermetic and pinned:
a conformance number that moves when a website changes is not a measurement.
"""

import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
# The generated suites go beside the modules they import, not beside the specs
# they are generated from.
PKG = HERE.parent / "package"


def examples(spec_text):
    """Every ```` ```…example ```` block in a GFM-style spec.txt.

    The fence is 32+ backticks and may carry an extension name (`example table`).
    `→` is the spec's visible tab; it must become a real tab or every example in
    the Tabs section tests the wrong input.
    """
    lines = spec_text.split("\n")
    section, out, i = None, [], 0
    while i < len(lines):
        line = lines[i]
        if line.startswith("## "):
            section = line[3:].strip()
        m = re.match(r"^(`{32,})\s*example(\s+\w+)?\s*$", line)
        if m:
            fence = m.group(1)
            i += 1
            md = []
            while lines[i] != ".":
                md.append(lines[i])
                i += 1
            i += 1
            html = []
            while not lines[i].startswith(fence):
                html.append(lines[i])
                i += 1
            out.append(
                (
                    section,
                    ("\n".join(md) + "\n").replace("→", "\t"),
                    (("\n".join(html) + "\n") if html else "").replace("→", "\t"),
                )
            )
        i += 1
    return out


def suite():
    """The examples to test, in section order: CommonMark then GFM extensions."""
    core = [
        (e["section"], e["markdown"], e["html"])
        for e in json.loads((HERE / "commonmark-0.31.2.json").read_text())
    ]
    ext = [
        (section, md, html)
        for section, md, html in examples((HERE / "gfm-extensions.txt").read_text())
        if "(extension)" in (section or "")
    ]
    return core + ext


def roc_str(s):
    """A Roc string literal. `${` would open an interpolation, so it is rejected
    loudly rather than silently mangled — no spec example contains one today."""
    if "${" in s:
        raise ValueError(f"example contains ${{, which Roc would interpolate: {s!r}")
    return (
        '"'
        + s.replace("\\", "\\\\")
        .replace('"', '\\"')
        .replace("\n", "\\n")
        .replace("\t", "\\t")
        + '"'
    )


HEADER = """## Conformance — GENERATED from `spec/`. Do not hand-edit.
##
## Regenerate with `just spec-regen`; run with `just spec`.
##
## CommonMark 0.31.2 plus GFM's extension sections, one `expect` per example,
## compared EXACTLY. The spec's expected output carries its own block separators
## and trailing newline, so matching it is part of conformance and not an
## artefact of the harness.
import Markdown

{name} := [].{{
    ## `to_html_unsafe`, because the spec's expected output contains raw HTML.
    ## cmark runs this same suite with `--unsafe` for the same reason; the
    ## DEFAULT lowering is the safe one, and that is the point of the split.
    html : Str -> Str
    html = |src| Markdown.to_html_unsafe(Markdown.parse(src))
"""

DBG_EXTRA = """    ## Newlines and tabs encoded, so one example's output is one line of `dbg`
    ## and the run can be diffed against the spec mechanically.
    flat : Str -> Str
    flat = |s| Str.join_with(Str.join_with(s.split_on("\\n"), "<NL>").split_on("\\t"), "<TAB>")
"""


def main():
    ex = suite()
    if not ex:
        sys.exit("no examples found — are both specs intact?")

    gate = [HEADER.format(name="SpecTest") + "}\n"]
    for i, (section, md, html) in enumerate(ex):
        gate.append(f"\n## EX{i:03d} [{section}]")
        gate.append(f"expect SpecTest.html({roc_str(md)}) == {roc_str(html)}")
    (PKG / "SpecTest.roc").write_text("\n".join(gate) + "\n")

    dbg = [HEADER.format(name="SpecDbg") + DBG_EXTRA + "}\n"]
    for i, (section, md, html) in enumerate(ex):
        dbg.append(f"\n## EX{i:03d} [{section}]")
        dbg.append(
            "expect {\n"
            f"    got = SpecDbg.html({roc_str(md)})\n"
            "    dbg SpecDbg.flat(got)\n"
            f"    got == {roc_str(html)}\n"
            "}"
        )
    (PKG / "SpecDbg.roc").write_text("\n".join(dbg) + "\n")

    print(f"{len(ex)} examples -> SpecTest.roc, SpecDbg.roc")


if __name__ == "__main__":
    main()
