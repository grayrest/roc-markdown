#!/usr/bin/env python3
"""Score a `SpecDbg.roc` run against the spec, by section.

    just spec-report

`roc test` reports only pass/fail counts, which cannot say WHICH constructs are
missing. `SpecDbg.roc` prints every example's actual output; this pairs those
with the spec's expected output and buckets the failures by spec section, so the
next thing to implement is the top row rather than a guess.

Three buckets, because they mean different work:

  exact        conformant.
  whitespace   the structure is right and only block separators differ. Cheap.
  structural   a construct is missing or wrong. This is the real number.
"""

import json
import os
import re
import subprocess
import sys
from collections import Counter
from pathlib import Path

HERE = Path(__file__).resolve().parent
PKG = HERE.parent / "package"
sys.path.insert(0, str(HERE))
from generate import suite  # noqa: E402


def actual_outputs(suite_file="SpecDbg.roc"):
    """Run the debug suite and recover one output string per example."""
    if not (PKG / suite_file).exists():
        sys.exit(f"{suite_file} missing — run `just spec-regen` first")
    # `--no-cache` is not optional. A cached run replays a PREVIOUS build's
    # output against the current expectations, so an edit that breaks 168
    # examples reports the old score and the mismatch shows up only as a stale
    # output count. That happened; this is the fix.
    # The same `$ROC` the Justfile honours, so the report and the gate cannot
    # be scored by two different compilers.
    roc = os.environ.get("ROC") or "roc"
    proc = subprocess.run(
        [roc, "test", "--no-cache", suite_file], cwd=PKG, capture_output=True, text=True
    )
    out = []
    for line in (proc.stdout + proc.stderr).splitlines():
        if line.startswith("[dbg] "):
            s = line[len("[dbg] ") :].strip()
            s = s[1:-1].encode().decode("unicode_escape")
            # `unicode_escape` decodes non-escape bytes as Latin-1, so any
            # UTF-8 in the output comes back as mojibake. Re-encoding Latin-1
            # and decoding UTF-8 undoes exactly that. Without this the harness
            # reports every entity and every non-ASCII character as a failure
            # that the parser got right.
            try:
                s = s.encode("latin-1").decode("utf-8")
            except (UnicodeEncodeError, UnicodeDecodeError):
                pass
            out.append(s.replace("<NL>", "\n").replace("<TAB>", "\t"))
    return out


def main():
    ex = suite()
    act = actual_outputs()
    if len(act) != len(ex):
        sys.exit(f"got {len(act)} outputs for {len(ex)} examples — suite is stale")

    # CommonMark has no autolink extension, so its base Autolinks section says a
    # bare URL is text while the GFM extension this package implements says it
    # is a link. GitHub linkifies them, and dropping the extension to satisfy
    # the base section would be a worse parser, so these are counted apart.
    EXTENSION_OVERRIDES = {
        "https://example.com\n",
        "foo@bar.example.com\n",
        "< https://foo.bar >\n",
    }

    squash = lambda s: re.sub(r"\s+", "", s)
    exact = ws = overridden = 0
    failed = Counter()
    total = Counter(section for section, _, _ in ex)
    detail = []
    for (section, md, html), got in zip(ex, act):
        if got == html:
            exact += 1
            continue
        if squash(got) == squash(html):
            kind = "whitespace"
            ws += 1
        elif md in EXTENSION_OVERRIDES:
            kind = "extension"
            overridden += 1
        else:
            kind = "structural"
            failed[section] += 1
        # Every kind is recorded. A bucket you cannot inspect is how a
        # block-separator bug stayed invisible for a whole session.
        detail.append(
            {"kind": kind, "section": section, "markdown": md, "want": html, "got": got}
        )

    n = len(ex)
    print(f"exact              {exact:4d}/{n}   ({100 * exact / n:.1f}%)")
    print(f"whitespace only    {ws:4d}")
    print(f"extension          {overridden:4d}   (GFM autolinks; CommonMark's base section has none)")
    print(f"structural         {sum(failed.values()):4d}\n")
    for section, count in failed.most_common():
        print(f"  {count:4d}/{total[section]:<4d} {section}")
    (HERE / "failures.json").write_text(json.dumps(detail, indent=1))
    print(f"\nper-example diffs: {HERE / 'failures.json'}")


if __name__ == "__main__":
    main()
