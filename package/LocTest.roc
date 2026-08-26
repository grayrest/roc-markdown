## The source-anchoring gate (`plans/2026-08-26-rocsidian.md` T0).
##
## The CommonMark suite (`SpecTest.roc`) compares rendered HTML and is
## structurally blind to a wrong `Loc` — it never reads one. This asserts the
## invariants the editor depends on, over inputs chosen to hit every stripping
## site the parser has (quote prefixes, tab expansion, list unindent, setext
## join, table cells), because a constant offset shift passes a top-level-only
## check and breaks exactly there.
##
## Block-level (T0a): every `Block` carries `loc`. Inline `Loc` is T0b.
##
## The invariants, at every block depth:
##  - (a) a child's `outer` is contained in its parent's, and children are
##    ordered and disjoint by `outer`;
##  - (c') top-level blocks are ordered, disjoint, and within `[0, len)` — the
##    parser's half of the tiling claim (the app owns the gaps, D11b);
##  - (d') for a node whose payload is a verbatim source slice, the source over
##    `outer` reconstructs the payload.
import Markdown
import Block exposing [Block]
import Loc exposing [Loc]

LocTest := [].{
    ## The immediate block children of a block.
    children : Block -> List(Block)
    children = |b|
        match b.node {
            Quote(bs) => bs
            Items(l) => List.join(l.entries)
            _ => []
        }

    ## Every block invariant holds over the whole tree.
    loc_ok : Str -> Bool
    loc_ok = |src| {
        len = Str.count_utf8_bytes(src)
        blocks = Markdown.parse(src)
        LocTest.level_ok(blocks, { start: 0, end: len })
    }

    ## One level: each block's `outer` sits inside `bound`, the level is ordered
    ## and disjoint, and each child level is checked inside its parent's `outer`.
    level_ok : List(Block), Loc.Ivl -> Bool
    level_ok = |blocks, bound| {
        result = List.fold(
            blocks,
            { ok: True, prev: bound.start },
            |st, b| {
                o = b.loc.outer
                within = o.start >= bound.start and o.end <= bound.end and o.start <= o.end
                ordered = o.start >= st.prev
                kids = LocTest.level_ok(LocTest.children(b), o)
                { ok: st.ok and within and ordered and kids, prev: o.end }
            },
        )
        result.ok
    }

    ## The source slice a block's `outer` covers, for the (d') reconstruction
    ## checks below.
    slice : Str, Block -> Str
    slice = |src, b| {
        o = b.loc.outer
        Str.from_utf8_lossy(List.sublist(Str.to_utf8(src), { start: o.start, len: o.end - o.start }))
    }

    first : Str -> Block
    first = |src|
        match List.first(Markdown.parse(src)) {
            Ok(b) => b
            Err(_) => Block.mk(Loc.none(0), Rule)
        }
}

# --- (a)/(c') the invariant holds across every stripping site ----------------

expect LocTest.loc_ok("hello world")
expect LocTest.loc_ok("# Heading\n\nA paragraph.\n\nAnother one.")
expect LocTest.loc_ok("> a quote\n> second line")
expect LocTest.loc_ok("> outer\n> > nested\n> > deeper")
expect LocTest.loc_ok("- one\n- two\n  - nested\n- three")
expect LocTest.loc_ok("1. first\n2. second\n\n3. loose")
expect LocTest.loc_ok("Para one\n\n- a list\n- of items\n\nPara two")
expect LocTest.loc_ok("| a | b |\n| --- | --- |\n| 1 | 2 |")
expect LocTest.loc_ok("```roc\nmain = 1\n```")
expect LocTest.loc_ok("    indented code\n    line two")
expect LocTest.loc_ok("A setext heading\n================\n\nbody")
expect LocTest.loc_ok("text\n\n---\n\nmore")
expect LocTest.loc_ok("\ttab-indented code\n")
expect LocTest.loc_ok("> - quoted list\n> - second item\n>\n> a paragraph")
expect LocTest.loc_ok("<div>\nraw html block\n</div>\n\ntext after")
expect LocTest.loc_ok("[ref]: /url\n\nSee [the ref][ref] here.")
expect LocTest.loc_ok("- [ ] todo\n- [x] done")
expect LocTest.loc_ok("")
expect LocTest.loc_ok("\n\n\n")

# --- (c') top-level blocks tile in order ------------------------------------

# Two paragraphs separated by a blank line: the first spans [0,3), the second
# starts after the blank at byte 5. The gap [3,5) is the blank line the app owns.
expect LocTest.first("abc\n\ndef").loc.outer == { start: 0, end: 3 }

# --- (d') a verbatim paragraph reconstructs from its outer -------------------

expect LocTest.slice("hello world", LocTest.first("hello world")) == "hello world"
expect LocTest.slice("first para\n\nsecond", LocTest.first("first para\n\nsecond")) == "first para"

# An ATX heading's outer covers the whole line, markers and all — which is what
# the editor reveals under the caret.
expect LocTest.first("## Title").loc.outer == { start: 0, end: 8 }

# A fenced block spans from its opening ``` to its closing ```.
expect LocTest.first("```\ncode\n```").loc.outer == { start: 0, end: 12 }
