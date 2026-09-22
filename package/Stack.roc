## Phase 1: block structure, by the spec's own parsing strategy.
##
## A stack of currently-open blocks. For each line: walk the stack matching each
## container's continuation prefix, close whatever failed to match, look for new
## block starts in what is left, and hand the remainder to the deepest open
## leaf. One pass, each line consumed exactly once.
##
## ## The encoding: a spine list, not a tree with parent pointers
##
## Roc has no mutable interior, so the tip cannot be updated in place inside a
## tree. Instead the stack is a `List(Open)` from root to tip, each entry holding
## the children it has already finished. Closing pops one, turns it into a `Doc`,
## and appends that to the new tip's children.
##
## The finished tree is therefore only ever built bottom-up by appending, and no
## recursive value is ever rebuilt to change one of its variants
import Doc exposing [Doc]
import Loc exposing [Loc]
import Lex
import Scan
import Ents
import Defs

Stack := [].{
    ## What is open. Containers may hold other blocks; leaves accumulate text.
    ##
    ## The stack is always containers first and at most one leaf, last. That
    ## invariant is what lets `descend` stop as soon as it reaches a leaf.
    Kind : [
        ## The document. Never closed, never matched against.
        Root,
        Quote,
        ## The list itself, separate from its entries, because looseness and the
        ## marker character belong to the run and not to any one entry.
        Bullets({ ordered : Bool, start : U64, marker : Str, loose : Bool, pending : Bool }),
        ## `col` is the CONTENT COLUMN: how far a following line must be
        ## indented to belong to this entry. Not a nesting depth — bucketing
        ## indents into depths is what lost `-    one` in the old parser.
        Entry({ col : U64, task : [NoTask, Todo, Done] }),
        Para,
        Fence({ ch : U8, len : U64, indent : U64, lang : Str }),
        Indented,
        ## The string that closes this raw HTML block, or `"BLANK"` for the
        ## conditions that end at a blank line.
        RawHtml(Str),
        Rows,
    ]

    Open : {
        kind : Stack.Kind,
        ## Finished children, for a container.
        kids : List(Doc),
        ## Finished entries, for `Bullets` only.
        entries : List(Doc.Entry),
        ## Pending raw lines, for a leaf — each anchored to the source offset
        ## its text starts at (D9b), because by the time phase 2 reads one, the
        ## marker or prefix that displaced it is gone. `Loc.seg` measures the
        ## offset from the line's END, so every append site passes the text it
        ## kept plus anything it stripped AFTER that text; for all but a closing
        ## ATX sequence those are the same string.
        text : List(Loc.Seg),
        ## The SOURCE byte span this block occupies (rocsidian D9): `s0` is set
        ## when the block opens, `s1` grows as it consumes lines and as `emit`
        ## folds each finished child's far edge upward. `built` reads them.
        s0 : U64,
        s1 : U64,
    }

    ## ## Entry point

    parse : Str -> List(Doc)
    parse = |source| {
        ## A document ending in a newline splits to a trailing empty line that is
        ## the terminator, not a blank line in the document.
        raw = source.split_on("\n")
        trimmed = match List.last(raw) {
            Ok("") => List.drop_last(raw, 1)
            _ => raw
        }
        spans = Stack.line_spans(trimmed, 0, [])
        Stack.finish(List.fold(spans, [Stack.fresh(Root)], |st, l| Stack.line(st, l)))
    }

    ## A line with its SOURCE span, tab-expanded for the parser. `off`/`end` are
    ## computed from the RAW (pre-expansion) split, so they track the original
    ## bytes; `s` is what the parser reads. Tab expansion changes `s`'s length
    ## but not the span, which is the whole point of measuring on the raw split.
    Line : { off : U64, end : U64, s : Str }

    line_spans : List(Str), U64, List(Stack.Line) -> List(Stack.Line)
    line_spans = |lines, off, out|
        match List.first(lines) {
            Err(_) => out
            Ok(l) => {
                len = Str.count_utf8_bytes(l)
                entry = { off, end: off + len, s: Lex.expand_tabs(l) }
                Stack.line_spans(List.drop_first(lines, 1), off + len + 1, List.append(out, entry))
            }
        }

    fresh : Stack.Kind -> Stack.Open
    fresh = |k| { kind: k, kids: [], entries: [], text: [], s0: 0, s1: 0 }

    ## Close everything and return the document's children.
    finish : List(Stack.Open) -> List(Doc)
    finish = |st|
        if List.len(st) <= 1 {
            match List.first(st) {
                Ok(root) => root.kids
                Err(_) => []
            }
        } else {
            Stack.finish(Stack.pop(st))
        }

    ## ## One line

    line : List(Stack.Open), Stack.Line -> List(Stack.Open)
    line = |st, l| {
        raw = l.s
        lo = { start: l.off, end: l.end }
        d = Stack.descend(st, 1, raw)
        ## Blankness belongs to the line with its container prefixes removed:
        ## `>>` inside two blockquotes IS a blank line. Reading the raw line
        ## instead fed an empty string to the open paragraph.
        blank = Str.is_empty(d.rest.trim())
        top = Stack.tip(st)
        ## Every container matched if we ran off the end, or stopped at the leaf.
        stopped_at_leaf = d.depth < List.len(st) and Stack.is_leaf(Stack.kind_at(st, d.depth))
        all_matched = d.depth >= List.len(st) or stopped_at_leaf
        ## Stopping at the leaf means the CONTAINERS all matched, so nothing is
        ## closed here — whether the leaf survives is `starts`' decision, and it
        ## is the difference between a paragraph continuing and a new one
        ## beginning on every line.
        target = if stopped_at_leaf {
            List.len(st)
        } else {
            d.depth
        }
        if all_matched and Stack.raw_leaf(top.kind) {
            Stack.leaf_line(st, d.rest, blank, lo)
        } else if !all_matched and !blank and Stack.lazy_ok(top.kind) and !Lex.starts_block(d.rest) {
            ## Lazy continuation. The containers did not match, but an open
            ## paragraph claims the line anyway — that IS the rule, and here it
            ## needs no flag because the matching step just told us.
            Stack.push_text(st, Str.trim_start(d.rest), lo)
        } else {
            Stack.starts(Stack.close_to(st, target), d.rest, blank, lo)
        }
    }

    tip : List(Stack.Open) -> Stack.Open
    tip = |st|
        match List.last(st) {
            Ok(o) => o
            Err(_) => Stack.fresh(Root)
        }

    kind_at : List(Stack.Open), U64 -> Stack.Kind
    kind_at = |st, i|
        match List.get(st, i) {
            Ok(o) => o.kind
            Err(_) => Root
        }

    is_leaf : Stack.Kind -> Bool
    is_leaf = |k|
        match k {
            Root => False
            Quote => False
            Bullets(_) => False
            Entry(_) => False
            _ => True
        }

    ## A leaf whose content is raw lines, which claims every line it is given.
    raw_leaf : Stack.Kind -> Bool
    raw_leaf = |k|
        match k {
            Fence(_) => True
            Indented => True
            RawHtml(_) => True
            _ => False
        }

    lazy_ok : Stack.Kind -> Bool
    lazy_ok = |k|
        match k {
            Para => True
            _ => False
        }

    ## ## Matching open containers
    ##
    ## Returns the index of the first container that did NOT match, and the line
    ## with every matched container's prefix removed.

    descend : List(Stack.Open), U64, Str -> { depth : U64, rest : Str }
    descend = |st, i, raw|
        if i >= List.len(st) {
            { depth: i, rest: raw }
        } else {
            match Stack.kind_at(st, i) {
                Quote =>
                    match Stack.strip_quote(raw) {
                        Ok(r) => Stack.descend(st, i + 1, r)
                        Err(_) => { depth: i, rest: raw }
                    }
                ## A list is continued by its entries, never by a prefix of its
                ## own, so it always matches and the decision belongs to the
                ## `Entry` below it.
                Bullets(_) => Stack.descend(st, i + 1, raw)
                Entry(e) =>
                    if Str.is_empty(raw.trim()) {
                        ## A blank line belongs to whatever is open; whether it
                        ## ends the entry depends on the NEXT line.
                        Stack.descend(st, i + 1, "")
                    } else if Lex.indent(raw, 0) >= e.col {
                        Stack.descend(st, i + 1, Lex.unindent(raw, e.col))
                    } else {
                        { depth: i, rest: raw }
                    }
                _ => { depth: i, rest: raw }
            }
        }

    ## Up to three spaces, a `>`, and at most one space after it.
    strip_quote : Str -> Try(Str, [NoMatch])
    strip_quote = |raw| {
        t = Lex.unindent(raw, 3)
        if t.starts_with(">") {
            r = t.drop_prefix(">")
            if r.starts_with(" ") {
                Ok(r.drop_prefix(" "))
            } else {
                Ok(r)
            }
        } else {
            Err(NoMatch)
        }
    }

    ## ## Raw leaves

    leaf_line : List(Stack.Open), Str, Bool, Loc.Ivl -> List(Stack.Open)
    leaf_line = |st, rest, blank, lo| {
        top = Stack.tip(st)
        match top.kind {
            Fence(f) =>
                if Lex.indent(rest, 0) < 4 and Lex.closes_fence(rest.trim(), f.ch, f.len) {
                    ## The closing fence line belongs to the block's SOURCE span
                    ## even though it is not content — the editor reveals it under
                    ## the caret — so extend `s1` to cover it before closing.
                    Stack.pop(Stack.set_tip(st, { ..top, s1: lo.end }))
                } else {
                    Stack.set_tip(st, { ..top, text: top.text.append(Loc.seg_of(lo, Lex.unindent(rest, f.indent))), s1: lo.end })
                }
            Indented =>
                if blank {
                    ## Not `""`: a blank line inside indented code keeps
                    ## whatever sits past the four spaces, and the spec compares
                    ## it exactly.
                    Stack.set_tip(st, { ..top, text: top.text.append(Loc.seg_of(lo, Lex.unindent(rest, 4))), s1: lo.end })
                } else if Lex.indent(rest, 0) >= 4 {
                    Stack.set_tip(st, { ..top, text: top.text.append(Loc.seg_of(lo, Lex.unindent(rest, 4))), s1: lo.end })
                } else {
                    ## Ends at the first line that is neither blank nor
                    ## indented. Re-dispatched to `starts` and NOT to `line`:
                    ## the containers matched on the way in, so `rest` has
                    ## already had their prefixes removed and matching it a
                    ## second time would fail against its own entry.
                    Stack.starts(Stack.pop(st), rest, False, lo)
                }
            RawHtml(ending) =>
                if ending == "BLANK" and blank {
                    Stack.pop(st)
                } else {
                    added = Stack.set_tip(st, { ..top, text: top.text.append(Loc.seg_of(lo, rest)), s1: lo.end })
                    if Lex.html_closes(rest.trim(), ending) {
                        Stack.pop(added)
                    } else {
                        added
                    }
                }
            _ => st
        }
    }

    ## ## New block starts

    starts : List(Stack.Open), Str, Bool, Loc.Ivl -> List(Stack.Open)
    starts = |raw_st, rest, blank, lo| {
        ## An entry has closed and left its list as the tip. Only a new entry
        ## may continue that list; anything else closes it, or the block lands
        ## in `Bullets.kids`, which nothing ever reads, and vanishes.
        st = if !blank and !Stack.marker_ahead(raw_st, rest) {
            Stack.close_dangling(raw_st)
        } else {
            raw_st
        }
        t = rest.trim()
        ind = Lex.indent(rest, 0)
        top = Stack.tip(st)
        para_open = Stack.lazy_ok(top.kind) and !List.is_empty(top.text)
        if blank {
            Stack.blank_line(st)
        } else if ind >= 4 and !para_open {
            Stack.push(Stack.mark_content(st), Indented, [Loc.seg_of(lo, Lex.unindent(rest, 4))], lo)
        } else if ind < 4 and Lex.opens_fence(t) {
            b = Str.to_utf8(t)
            ch = Scan.at(b, 0)
            n = Lex.leading_run(b, ch, 0)
            info = Str.from_utf8_lossy(List.drop_first(b, n)).trim()
            Stack.push(
                Stack.mark_content(Stack.close_leaf(st)),
                Fence({ ch: ch, len: n, indent: ind, lang: Scan.unescape(Ents.decode(Lex.first_word(info))) }),
                [],
                lo,
            )
        } else if ind < 4
        and Lex.html_start(t) != ""
        and (!para_open or List.contains(Lex.block_tags, Str.with_ascii_lowercased(Lex.tag_name(t)))) {
            opened = Stack.push(Stack.mark_content(Stack.close_leaf(st)), RawHtml(Lex.html_start(t)), [Loc.seg_of(lo, rest)], lo)
            if Lex.html_closes(t, Lex.html_start(t)) {
                Stack.pop(opened)
            } else {
                opened
            }
        } else if ind < 4 and para_open and Lex.setext_level(t) > 0 {
            Stack.setext(st, Lex.setext_level(t), rest, lo)
        } else if ind < 4 and Lex.is_rule(t) {
            Stack.emit(Stack.mark_content(Stack.close_leaf(st)), Doc.mk(lo, Rule))
        } else if ind < 4 and Stack.atx_level(t) > 0 {
            Stack.emit(
                Stack.mark_content(Stack.close_leaf(st)),
                Doc.mk(lo, Heading(Stack.atx_level(t), [Loc.seg(lo, Stack.atx_tail(rest), Stack.atx_text(t))])),
            )
        } else if ind < 4 and t.starts_with(">") {
            match Stack.strip_quote(rest) {
                Ok(inner) =>
                    Stack.starts(
                        Stack.push(Stack.mark_content(Stack.close_leaf(st)), Quote, [], lo),
                        inner,
                        Str.is_empty(inner.trim()),
                        lo,
                    )
                Err(_) => Stack.push_para(st, rest, lo)
            }
        } else if ind < 4 and Stack.is_marker(rest, !para_open) {
            Stack.open_entry(st, rest, lo)
        } else if Stack.rows_open(top.kind) or (ind < 4 and t.starts_with("|")) {
            Stack.push_rows(st, rest, lo)
        } else {
            Stack.push_para(st, rest, lo)
        }
    }

    ## Would this line open a list entry? Asked before closing a dangling list,
    ## so that a sibling entry keeps the run alive.
    marker_ahead : List(Stack.Open), Str -> Bool
    marker_ahead = |st, rest| {
        top = Stack.tip(st)
        para_open = Stack.lazy_ok(top.kind) and !List.is_empty(top.text)
        Lex.indent(rest, 0) < 4 and !Lex.is_rule(rest.trim()) and Stack.is_marker(rest, !para_open)
    }

    close_dangling : List(Stack.Open) -> List(Stack.Open)
    close_dangling = |st|
        match Stack.tip(st).kind {
            Bullets(_) => Stack.close_dangling(Stack.pop(st))
            _ => st
        }

    rows_open : Stack.Kind -> Bool
    rows_open = |k|
        match k {
            Rows => True
            _ => False
        }

    push_rows : List(Stack.Open), Str, Loc.Ivl -> List(Stack.Open)
    push_rows = |st, rest, lo| {
        top = Stack.tip(st)
        if Stack.rows_open(top.kind) {
            Stack.set_tip(st, { ..top, text: top.text.append(Loc.seg_of(lo, rest)), s1: lo.end })
        } else {
            Stack.push(Stack.mark_content(Stack.close_leaf(st)), Rows, [Loc.seg_of(lo, rest)], lo)
        }
    }

    push_para : List(Stack.Open), Str, Loc.Ivl -> List(Stack.Open)
    push_para = |st, rest, lo| {
        top = Stack.tip(st)
        if Stack.lazy_ok(top.kind) {
            Stack.set_tip(st, { ..top, text: top.text.append(Loc.seg_of(lo, Str.trim_start(rest))), s1: lo.end })
        } else {
            Stack.push(Stack.mark_content(Stack.close_leaf(st)), Para, [Loc.seg_of(lo, Str.trim_start(rest))], lo)
        }
    }

    push_text : List(Stack.Open), Str, Loc.Ivl -> List(Stack.Open)
    push_text = |st, t, lo| {
        top = Stack.tip(st)
        Stack.set_tip(st, { ..top, text: top.text.append(Loc.seg_of(lo, t)), s1: lo.end })
    }

    ## A blank line closes a paragraph and is remembered by every open list, so
    ## that content arriving afterwards can make the list loose.
    blank_line : List(Stack.Open) -> List(Stack.Open)
    blank_line = |st| Stack.mark_blank(Stack.seal_empty(Stack.close_leaf(st)))

    ## "A list item can begin with at most one blank line." An entry holding
    ## nothing when the blank arrives is done — there is no paragraph left for a
    ## later line to continue, so `-\n\n  foo` is an empty item and a separate
    ## paragraph rather than an item containing foo.
    seal_empty : List(Stack.Open) -> List(Stack.Open)
    seal_empty = |st| {
        top = Stack.tip(st)
        match top.kind {
            Entry(_) =>
                if List.is_empty(top.kids) {
                    Stack.pop(st)
                } else {
                    st
                }
            _ => st
        }
    }

    ## ## ATX headings

    ## Both of these are asked of nearly every line, so both start with the
    ## cheapest possible rejection. Without it they each ran a `split_on` and a
    ## `join_with` — two allocations and a substring search — to normalise tabs
    ## in a line that is not a heading and usually has no tab either.
    atx_level : Str -> U8
    atx_level = |t|
        if !t.starts_with("#") {
            0
        } else {
            Stack.atx_at(t)
        }

    ## `######` is the deepest heading; seven or more hashes are prose.
    max_atx_level : U64
    max_atx_level = 6

    atx_at : Str -> U8
    atx_at = |t| {
        detabbed = Stack.detab(t)
        lvl = Lex.leading_hashes(Str.to_utf8(detabbed), 0)
        rest = Str.drop_first_bytes(detabbed, lvl).ok_or("")
        if lvl == 0 or lvl > Stack.max_atx_level {
            0
        } else if Str.is_empty(rest) or rest.starts_with(" ") {
            ## In range by the test above, so the narrowing cannot wrap.
            lvl.to_u8_wrap()
        } else {
            ## No space after the run — `#hashtag` is prose.
            0
        }
    }

    detab : Str -> Str
    detab = |t|
        if t.contains("\t") {
            Str.join_with(t.split_on("\t"), " ")
        } else {
            t
        }

    ## The heading's text plus everything phase 1 dropped after it — the closing
    ## `##` sequence and any trailing spaces — so `Loc.seg` can measure back from
    ## the line's end. Taken from the UNTRIMMED line for that reason: `atx_text`
    ## works on `rest.trim()`, and a trimmed tail would place the text as many
    ## bytes late as the line has trailing whitespace.
    atx_tail : Str -> Str
    atx_tail = |rest| {
        detabbed = Stack.detab(Str.trim_start(rest))
        lvl = Lex.leading_hashes(Str.to_utf8(detabbed), 0)
        Str.trim_start(Str.drop_first_bytes(detabbed, lvl).ok_or(""))
    }

    atx_text : Str -> Str
    atx_text = |t| {
        detabbed = Stack.detab(t)
        lvl = Lex.leading_hashes(Str.to_utf8(detabbed), 0)
        rest = Str.drop_first_bytes(detabbed, lvl).ok_or("")
        if Str.is_empty(rest) {
            ""
        } else {
            Lex.drop_closing(rest.trim())
        }
    }

    ## The paragraph above a `===` or `---` becomes a heading. It reaches BACK
    ## into what is already buffered, which is why it cannot be an ordinary
    ## block start.
    setext : List(Stack.Open), U8, Str, Loc.Ivl -> List(Stack.Open)
    setext = |st, lvl, under, lo| {
        top = Stack.tip(st)
        ## Leading definitions are not heading content. They stay behind as a
        ## paragraph of their own so phase 2 still collects them — dropping them
        ## here would lose the definition along with the text.
        n = Defs.lead_lines(List.map(top.text, |g| g.text), 0)
        lead = List.sublist(top.text, { start: 0, len: n })
        kept = List.drop_first(top.text, n)
        base = Stack.pop_discard(st)
        carried = if List.is_empty(lead) {
            base
        } else {
            Stack.emit(base, Doc.mk({ start: top.s0, end: top.s1 }, Para(lead)))
        }
        if List.is_empty(kept) {
            ## The paragraph was nothing but definitions, so there is nothing to
            ## underline and the `===` is ordinary text.
            Stack.push_para(carried, under, lo)
        } else {
            Stack.emit(
                Stack.mark_content(carried),
                Doc.mk({ start: top.s0, end: lo.end }, Heading(lvl, Loc.trim_last(kept))),
            )
        }
    }

    ## ## Lists

    is_marker : Str, Bool -> Bool
    is_marker = |rest, no_para|
        match Lex.marker_here(rest, no_para) {
            NotList => False
            _ => True
        }

    open_entry : List(Stack.Open), Str, Loc.Ivl -> List(Stack.Open)
    open_entry = |st, rest, lo| {
        top = Stack.tip(st)
        para_open = Stack.lazy_ok(top.kind) and !List.is_empty(top.text)
        match Lex.marker_here(rest, !para_open) {
            Bullet(col, _h) => Stack.open_entry_at(st, col, False, 1, Lex.marker_char(rest.trim()), Lex.entry_head(rest, col), lo)
            Numbered(col, num, _h) => Stack.open_entry_at(st, col, True, num, Lex.marker_char(rest.trim()), Lex.entry_head(rest, col), lo)
            NotList => Stack.push_para(st, rest, lo)
        }
    }

    open_entry_at : List(Stack.Open), U64, Bool, U64, Str, Str, Loc.Ivl -> List(Stack.Open)
    open_entry_at = |st, col, ord, num, ch, head, lo| {
        closed = Stack.close_leaf(st)
        ## The tip is a `Bullets` only when this line's own entry already failed
        ## to match and was closed — i.e. this is a SIBLING. If the tip is still
        ## an `Entry`, the line matched that entry's indentation and the marker
        ## opens a list NESTED inside it.
        ##
        ## A different marker character ends the run either way: `- a` then
        ## `+ b` is two lists, which is what HTML can express and the spec
        ## requires.
        continuing = Stack.list_matches(Stack.tip(closed).kind, ord, ch)
        base = if continuing {
            closed
        } else {
            Stack.close_dangling(closed)
        }
        listed = if continuing {
            Stack.mark_content(base)
        } else {
            Stack.push(
                Stack.mark_content(base),
                Bullets({ ordered: ord, start: num, marker: ch, loose: False, pending: False }),
                [],
                lo,
            )
        }
        task = Lex.task_of(head)
        with_entry = Stack.push(listed, Entry({ col: col, task: task.state }), [], lo)
        if Str.is_empty(task.rest.trim()) {
            with_entry
        } else {
            Stack.starts(with_entry, task.rest, False, lo)
        }
    }

    ## The open list directly enclosing the open entry, if there is one.
    tip_kind_below_entry : List(Stack.Open) -> Stack.Kind
    tip_kind_below_entry = |st|
        match Stack.tip(st).kind {
            Entry(_) => Stack.kind_at(st, List.len(st) - 2)
            other => other
        }

    list_matches : Stack.Kind, Bool, Str -> Bool
    list_matches = |k, ord, ch|
        match k {
            Bullets(l) => l.ordered == ord and l.marker == ch
            _ => False
        }

    ## ## Closing

    close_to : List(Stack.Open), U64 -> List(Stack.Open)
    close_to = |st, depth|
        if List.len(st) > depth and depth > 0 {
            Stack.close_to(Stack.pop(st), depth)
        } else {
            st
        }

    close_leaf : List(Stack.Open) -> List(Stack.Open)
    close_leaf = |st|
        if Stack.is_leaf(Stack.tip(st).kind) {
            Stack.pop(st)
        } else {
            st
        }

    close_entry_only : List(Stack.Open) -> List(Stack.Open)
    close_entry_only = |st|
        match Stack.tip(st).kind {
            Entry(_) => Stack.pop(st)
            _ => st
        }

    close_lists : List(Stack.Open) -> List(Stack.Open)
    close_lists = |st|
        match Stack.tip(st).kind {
            Entry(_) => Stack.close_lists(Stack.pop(st))
            Bullets(_) => Stack.close_lists(Stack.pop(st))
            _ => st
        }

    ## Pop the tip, turn it into a `Doc`, and append it to the new tip.
    pop : List(Stack.Open) -> List(Stack.Open)
    pop = |st| {
        top = Stack.tip(st)
        rest = List.drop_last(st, 1)
        match top.kind {
            ## An entry does not become a `Doc`; its blocks become one entry of
            ## the list below it.
            Entry(e) => {
                parent = Stack.tip(rest)
                Stack.set_tip(
                    rest,
                    { ..parent, entries: parent.entries.append({ task: e.task, start: top.s0, blocks: top.kids }), s1: Stack.max_u64(parent.s1, top.s1) },
                )
            }
            _ =>
                match Stack.built(top) {
                    Ok(d) => Stack.emit(rest, d)
                    Err(_) => rest
                }
        }
    }

    ## Pop WITHOUT building — for setext, which rewrites the paragraph it ends.
    pop_discard : List(Stack.Open) -> List(Stack.Open)
    pop_discard = |st| List.drop_last(st, 1)

    emit : List(Stack.Open), Doc -> List(Stack.Open)
    emit = |st, d| {
        parent = Stack.tip(st)
        Stack.set_tip(st, { ..parent, kids: parent.kids.append(d), s1: Stack.max_u64(parent.s1, d.at.end) })
    }

    max_u64 : U64, U64 -> U64
    max_u64 = |a, b| if a > b { a } else { b }

    push : List(Stack.Open), Stack.Kind, List(Loc.Seg), Loc.Ivl -> List(Stack.Open)
    push = |st, k, txt, at| List.append(st, { kind: k, kids: [], entries: [], text: txt, s0: at.start, s1: at.end })

    set_tip : List(Stack.Open), Stack.Open -> List(Stack.Open)
    set_tip = |st, o| List.append(List.drop_last(st, 1), o)

    ## Build the `Doc` an open block has accumulated. `Err` means it produced
    ## nothing — an empty paragraph, or an entry, whose contents its parent takes.
    built : Stack.Open -> Try(Doc, [Nothing])
    built = |o|
        match o.kind {
            Root => Err(Nothing)
            ## Handled by `pop`; an entry never becomes a `Doc` of its own.
            Entry(_) => Err(Nothing)
            Quote => Ok(Doc.mk({ start: o.s0, end: o.s1 }, Quote(o.kids)))
            Bullets(l) =>
                if List.is_empty(o.entries) {
                    Err(Nothing)
                } else {
                    Ok(
                        Doc.mk({ start: o.s0, end: o.s1 }, Items({
                            ordered: l.ordered,
                            start: l.start,
                            tight: !l.loose,
                            entries: o.entries,
                        })),
                    )
                }
            Para =>
                if List.is_empty(o.text) {
                    Err(Nothing)
                } else {
                    Ok(Doc.mk({ start: o.s0, end: o.s1 }, Para(Loc.trim_last(o.text))))
                }
            Fence(f) => Ok(Doc.mk({ start: o.s0, end: o.s1 }, Code({ lang: f.lang, text: Loc.joined(o.text) })))
            Indented => Ok(Doc.mk({ start: o.s0, end: o.s1 }, Code({ lang: "", text: Loc.joined(Stack.drop_blanks(o.text)) })))
            RawHtml(_) => Ok(Doc.mk({ start: o.s0, end: o.s1 }, Html(Loc.joined(o.text))))
            Rows =>
                if List.is_empty(o.text) {
                    Err(Nothing)
                } else {
                    Ok(Doc.mk({ start: o.s0, end: o.s1 }, Table(o.text)))
                }
        }

    ## A blank line does not end an indented code block, so trailing blanks are
    ## buffered and dropped here if they turn out to have been the terminator.
    drop_blanks : List(Loc.Seg) -> List(Loc.Seg)
    drop_blanks = |ls|
        match List.last(ls) {
            Ok(g) =>
                if Str.is_empty(g.text) {
                    Stack.drop_blanks(List.drop_last(ls, 1))
                } else {
                    ls
                }
            Err(_) => ls
        }

    ## ## Looseness
    ##
    ## A list is loose when a blank line falls between two of its blocks. The
    ## blank arms every open list; the next piece of content fires them.

    ## A blank arms every open list; the first content to follow fires the one
    ## it lands in, and disarms the rest.
    ##
    ## Both halves are load-bearing, and each was wrong on its own:
    ##
    ##  - Firing every armed list made `- foo / - bar / - baz / blank / bim`
    ##    loose at all three levels, when `bim` belongs to the innermost item
    ##    and only that list's blocks are separated.
    ##  - Arming only the innermost missed `* foo / * bar / blank / baz`, where
    ##    the blank separates the inner LIST from `baz` — two blocks of the
    ##    OUTER item, so it is the outer list that is loose.
    ##
    ## Disarming on the first content is what keeps `- a / - b / blank / c / - d`
    ## tight at the outer level: the blank is spent on `c`, and `d` is not
    ## separated from anything.
    mark_blank : List(Stack.Open) -> List(Stack.Open)
    mark_blank = |st| {
        ## A `Quote` below a list means the blank is a line of the quote and
        ## does not separate the list's items at all.
        q = Stack.last_quote(st, 0, 0)
        List.map_with_index(st, |o, i| if i > q { Stack.arm(o) } else { o })
    }

    last_quote : List(Stack.Open), U64, U64 -> U64
    last_quote = |st, i, found|
        if i >= List.len(st) {
            found
        } else {
            match Stack.kind_at(st, i) {
                Quote => Stack.last_quote(st, i + 1, i)
                _ => Stack.last_quote(st, i + 1, found)
            }
        }

    ## The deepest open list — the one any content added now would belong to.
    deepest_list : List(Stack.Open), U64, Try(U64, [None]) -> Try(U64, [None])
    deepest_list = |st, i, found|
        if i >= List.len(st) {
            found
        } else {
            match Stack.kind_at(st, i) {
                Bullets(_) => Stack.deepest_list(st, i + 1, Ok(i))
                _ => Stack.deepest_list(st, i + 1, found)
            }
        }

    mark_content : List(Stack.Open) -> List(Stack.Open)
    mark_content = |st|
        match Stack.deepest_list(st, 0, Err(None)) {
            Ok(i) => List.map_with_index(st, |o, j| if j == i { Stack.fire(o) } else { Stack.disarm(o) })
            Err(_) => List.map(st, Stack.disarm)
        }

    arm : Stack.Open -> Stack.Open
    arm = |o|
        match o.kind {
            Bullets(l) => { ..o, kind: Bullets({ ..l, pending: True }) }
            _ => o
        }

    disarm : Stack.Open -> Stack.Open
    disarm = |o|
        match o.kind {
            Bullets(l) => { ..o, kind: Bullets({ ..l, pending: False }) }
            _ => o
        }

    fire : Stack.Open -> Stack.Open
    fire = |o|
        match o.kind {
            Bullets(l) =>
                if l.pending {
                    { ..o, kind: Bullets({ ..l, loose: True, pending: False }) }
                } else {
                    o
                }
            _ => o
        }
}
