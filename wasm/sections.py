#!/usr/bin/env python3
"""Section sizes of a wasm module: total, code, data.

The three numbers the README's entity-table table reports. Code and data are
the only two that move when the table changes, and reporting them separately is
what tells a data win from a code regression.
"""
import sys

NAMES = {0: "custom", 1: "type", 2: "import", 3: "function", 4: "table",
         5: "memory", 6: "global", 7: "export", 8: "start", 9: "elem",
         10: "code", 11: "data", 12: "datacount"}


def leb(b, i):
    n = shift = 0
    while True:
        byte = b[i]
        i += 1
        n |= (byte & 0x7F) << shift
        if not byte & 0x80:
            return n, i
        shift += 7


def sections(b):
    assert b[:4] == b"\0asm", "not a wasm module"
    i, out = 8, {}
    while i < len(b):
        sid = b[i]
        size, i = leb(b, i + 1)
        out[NAMES.get(sid, str(sid))] = out.get(NAMES.get(sid, str(sid)), 0) + size
        i += size
    return out


if __name__ == "__main__":
    path = sys.argv[1]
    b = open(path, "rb").read()
    s = sections(b)
    label = sys.argv[2] if len(sys.argv) > 2 else path
    print(f"{label}\t{len(b)}\t{s.get('code', 0)}\t{s.get('data', 0)}")
