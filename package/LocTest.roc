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
import Block
import Inl
import Loc

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
    ## The block's own INLINE nodes are checked inside its `outer` too (T0b).
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
                inls = LocTest.inl_ok(LocTest.inls_of(b), o)
                { ok: st.ok and within and ordered and kids and inls, prev: o.end }
            },
        )
        result.ok
    }

    ## The inline nodes a block owns directly.
    inls_of : Block -> List(Inl)
    inls_of = |b|
        match b.node {
            Para(ns) => ns
            Heading(_, ns) => ns
            ## Row-major, so the cells are already in source order — which is
            ## what `inl_ok` needs to check them the way it checks a paragraph.
            Table(t) => List.join(t.cells)
            _ => []
        }

    ## An inline node's block children (an emphasis/link subtree).
    inl_kids : Inl -> List(Inl)
    inl_kids = |n|
        match n.node {
            Emph(ks) => ks
            Strong(ks) => ks
            Strike(ks) => ks
            Link(l) => l.kids
            Image(m) => m.kids
            _ => []
        }

    ## Every inline node's `outer` sits inside `bound`, siblings are ordered and
    ## disjoint, and each subtree is checked inside its parent's `outer` — the
    ## same shape as `level_ok`, one level down.
    ##
    ## Containment is the weak half of the claim and it always held, even when
    ## every offset was short by the marker phase 1 stripped — clamping to the
    ## block is enough for it. The EXACTNESS assertions at the bottom of this
    ## file are what actually pin the offsets down.
    inl_ok : List(Inl), Loc.Ivl -> Bool
    inl_ok = |ns, bound| {
        result = List.fold(
            ns,
            { ok: True, prev: bound.start },
            |st, n| {
                o = n.loc.outer
                within = o.start >= bound.start and o.end <= bound.end and o.start <= o.end
                ordered = o.start >= st.prev
                kids = LocTest.inl_ok(LocTest.inl_kids(n), o)
                { ok: st.ok and within and ordered and kids, prev: o.end }
            },
        )
        result.ok
    }

    ## The source slice an inline node's `outer` covers.
    inl_slice : Str, Inl -> Str
    inl_slice = |src, n| {
        o = n.loc.outer
        Str.from_utf8_lossy(List.sublist(Str.to_utf8(src), { start: o.start, len: o.end - o.start }))
    }

    ## Every block in the tree, parents before children.
    all_blocks : List(Block) -> List(Block)
    all_blocks = |bs|
        List.join(List.map(bs, |b| List.concat([b], LocTest.all_blocks(LocTest.children(b)))))

    ## Every inline node in a forest, parents before children.
    all_inls : List(Inl) -> List(Inl)
    all_inls = |ns|
        List.join(List.map(ns, |n| List.concat([n], LocTest.all_inls(LocTest.inl_kids(n)))))

    ## The first MARKED inline ANYWHERE in the tree, not just in the first
    ## top-level block. The depth is the point: an offset that is merely shifted
    ## by a constant passes every check made at column 0.
    marked : Str -> Inl
    marked = |src| {
        ns = LocTest.all_inls(List.join(List.map(LocTest.all_blocks(Markdown.parse(src)), LocTest.inls_of)))
        match List.find_first(ns, LocTest.is_marked) {
            Ok(n) => n
            Err(_) => Inl.at(0, 0, Text(""))
        }
    }

    ## The source that node claims.
    marked_slice : Str -> Str
    marked_slice = |src| LocTest.inl_slice(src, LocTest.marked(src))

    ## The first inline node of the first block.
    first_inl : Str -> Inl
    first_inl = |src|
        match List.first(LocTest.inls_of(LocTest.first(src))) {
            Ok(n) => n
            Err(_) => Inl.at(0, 0, Text(""))
        }

    ## The first MARKED inline node (Strong/Emph/Code/Link/Image) of the first
    ## block — the one whose markers the editor elides.
    first_inl_of_kind : Str -> Inl
    first_inl_of_kind = |src|
        match List.find_first(LocTest.inls_of(LocTest.first(src)), LocTest.is_marked) {
            Ok(n) => n
            Err(_) => Inl.at(0, 0, Text(""))
        }

    is_marked : Inl -> Bool
    is_marked = |n|
        match n.node {
            Strong(_) => True
            Emph(_) => True
            Strike(_) => True
            Code(_) => True
            Link(_) => True
            Image(_) => True
            _ => False
        }

    ## The source slice a block's `outer` covers, for the (d') reconstruction
    ## checks below.
    slice : Str, Block -> Str
    slice = |src, b| {
        o = b.loc.outer
        Str.from_utf8_lossy(List.sublist(Str.to_utf8(src), { start: o.start, len: o.end - o.start }))
    }

    ## The first paragraph anywhere in the tree that opens with a checkbox.
    first_task_para : Str -> Try(Block, [NotFound])
    first_task_para = |src|
        List.find_first(
            LocTest.all_blocks(Markdown.parse(src)),
            |b|
                match List.first(LocTest.inls_of(b)) {
                    Ok(n) =>
                        match n.node {
                            Task(_) => True
                            _ => False
                        }
                    Err(_) => False
                },
        )

    first : Str -> Block
    first = |src|
        match List.first(Markdown.parse(src)) {
            Ok(b) => b
            Err(_) => Block.mk(Loc.none(0), Rule)
        }

    ## The location of the first inline node anywhere in the tree whose `outer`
    ## covers exactly `claim` in the source.
    loc_of : Str, Str -> Loc.T
    loc_of = |src, claim| {
        ns = LocTest.all_inls(List.join(List.map(LocTest.all_blocks(Markdown.parse(src)), LocTest.inls_of)))
        match List.find_first(ns, |n| LocTest.inl_slice(src, n) == claim) {
            Ok(n) => n.loc
            Err(_) => Loc.none(0)
        }
    }

    ## The location of the first soft break anywhere in the tree: a node that
    ## covers only the newline between two lines, which no source byte stands
    ## for.
    break_loc : Str -> Loc.T
    break_loc = |src| {
        ns = LocTest.all_inls(List.join(List.map(LocTest.all_blocks(Markdown.parse(src)), LocTest.inls_of)))
        is_break = |n|
            match n.node {
                SoftBreak => True
                _ => False
            }
        match List.find_first(ns, is_break) {
            Ok(n) => n.loc
            Err(_) => Loc.at(0, 0)
        }
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

# --- (T0b) inline outer spans the source, markers included ------------------

# A bold run's Strong spans the whole `**bold**`, so the editor can elide the
# markers: "x **bold** y" -> the Strong is bytes 2..10.
expect LocTest.inl_slice("x **bold** y", LocTest.first_inl_of_kind("x **bold** y")) == "**bold**"

# A code span spans its backticks: "a `code` b" -> bytes 2..8.
expect LocTest.inl_slice("a `code` b", LocTest.first_inl_of_kind("a `code` b")) == "`code`"

# A link spans the whole `[text](url)`.
expect LocTest.inl_slice("see [text](/u) here", LocTest.first_inl_of_kind("see [text](/u) here")) == "[text](/u)"

# The first Text run of a plain paragraph reconstructs verbatim.
expect LocTest.inl_slice("hello world", LocTest.first_inl("hello world")) == "hello world"

# --- (T0b) an inline's source is EXACT wherever phase 1 stripped a prefix -----
#
# Each line below is the same defect at a different stripping site: the scanner
# reports offsets into the payload phase 1 kept, and before the line table those
# were added to the block's start — which is where the MARKER begins, not where
# the text does. `- [x](y)` reported the same offsets as a bare `[x](y)`, so the
# range landed two bytes early and sliced `- [x](`.

# A list marker, a quote marker, an ATX marker, bare indent.
expect LocTest.marked_slice("- [x](y)") == "[x](y)"
expect LocTest.marked_slice("> [x](y)") == "[x](y)"
expect LocTest.marked_slice("## [x](y)") == "[x](y)"
expect LocTest.marked_slice("  [x](y)") == "[x](y)"

# Depth: the prefix is four bytes here, and nesting is where a constant is most
# obviously not one.
expect LocTest.marked_slice("> > **b**") == "**b**"
expect LocTest.marked_slice("- - **b**") == "**b**"

# A checkbox is stripped positionally, before the text it precedes.
expect LocTest.marked_slice("- [ ] **b**") == "**b**"

# Soft-wrapped: line two carries continuation indent line one does not, so no
# single delta can place both.
expect LocTest.marked_slice("- text\n  [x](y)") == "[x](y)"
expect LocTest.marked_slice("> a\n> **b**") == "**b**"

# Tab expansion is LENGTH-CHANGING, which is why the offset is measured back
# from the line's end rather than forward from its start.
expect LocTest.marked_slice("-\t**b**") == "**b**"
expect LocTest.marked_slice(">\t**b**") == "**b**"

# An ATX closing sequence comes off the END and must not move the text.
expect LocTest.marked_slice("## **b** ##") == "**b**"

# A setext heading is built by reaching back into a buffered paragraph; its
# lines keep the offsets they were stamped with.
expect LocTest.marked_slice("> **b**\n> ===") == "**b**"

# A definition lifted out of the paragraph leaves the text below it anchored.
expect LocTest.marked_slice("[r]: /u\n[x](y)") == "[x](y)"

# A table cell anchors to its row, not to the cell string the splitter made.
expect LocTest.marked_slice("| a | **b** |\n| --- | --- |") == "**b**"
expect LocTest.marked_slice("> | a | **b** |\n> | --- | --- |") == "**b**"

# --- (d) `content` is a LIST for the reason it was made one ------------------

# An inline spanning a soft line break is TWO disjoint source ranges with the
# second line's stripped `> ` between them. `outer` covers the prefix; `content`
# does not, and concatenating the source over it reconstructs the payload.
expect LocTest.marked_slice("> **a\n> b**") == "**a\n> b**"
expect LocTest.marked("> **a\n> b**").loc.content == [{ start: 2, end: 5 }, { start: 8, end: 11 }]

# --- (c') a paragraph that lost leading definitions starts at its text (D11b)

expect LocTest.first("[a]: /1\ntext").loc.outer == { start: 8, end: 12 }

# --- a synthesized checkbox sits at its entry, not at byte 0 -----------------

# should_anchor_an_empty_task_item_at_its_entry: `- [ ]` with no content gets a
# paragraph the parser invents to hold the checkbox, and that paragraph used to
# claim byte 0 — outside its list, before the `intro` paragraph.
expect LocTest.loc_ok("intro\n\n- [ ]")
expect
    match LocTest.first_task_para("intro\n\n- [ ]") {
        Ok(b) => b.loc.outer.start == 7
        Err(_) => False
    }

# --- finding a node's line ----------------------------------------------------
#
# A payload offset is placed by finding the line it falls on. These pin where
# the lines meet: a node at a line's first byte, one ending at a line's last
# byte, one inside the last line, and a soft break, which covers only the
# newline between two lines and so lands at the end of the first.
# The source's lines start at bytes 0, 6 and 22, each behind a two-byte `> `.

# should_place_a_node_at_a_lines_first_byte
expect LocTest.loc_of("> a b\n> **c** d **e**\n> f *g* h", "**c**") == { outer: { start: 8, end: 13 }, content: [{ start: 8, end: 13 }] }
# should_place_a_node_ending_at_a_lines_last_byte
expect LocTest.loc_of("> a b\n> **c** d **e**\n> f *g* h", "**e**") == { outer: { start: 16, end: 21 }, content: [{ start: 16, end: 21 }] }
# should_place_a_node_inside_the_last_line
expect LocTest.loc_of("> a b\n> **c** d **e**\n> f *g* h", "*g*") == { outer: { start: 26, end: 29 }, content: [{ start: 26, end: 29 }] }
# should_place_a_soft_break_at_the_end_of_its_line
expect LocTest.break_loc("> a b\n> **c** d **e**\n> f *g* h") == { outer: { start: 5, end: 5 }, content: [] }

# Each node used to walk the lines from the first, so a paragraph of N lines
# cost N² — about 19 s for 10,000 lines.
# should_place_a_node_on_the_last_of_twenty_thousand_lines
expect LocTest.marked_slice("${Str.repeat("a\n", 20000)}**b**") == "**b**"
