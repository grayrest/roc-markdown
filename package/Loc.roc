## A source location: the byte range a node occupies in the original source,
## plus the sub-ranges that literally make up its payload.
##
## `plans/2026-08-26-rocsidian.md` D9/D9b. The editor holds the raw markdown
## source as one string and re-parses it on every edit; `Loc` is how it maps a
## parsed node back to the bytes it came from, so it can reveal a block's markers
## under the caret and decorate a source range.
##
## - `outer` is the ONE interval the node spans, markers and all. It is
##   strip-invariant: quote stripping, tab expansion and trimming change a node's
##   PAYLOAD but not where it starts and ends in the source, so `outer` is exact
##   for every node regardless of what phase 1 did to the text.
## - `content` is the list of source sub-intervals whose bytes literally appear,
##   in order, in the node's payload — a LIST rather than a pair because a
##   blockquote or a fenced block is non-contiguous in the source (the `> ` and
##   the fence lines are gaps interior to the node). Concatenating the source
##   over `content` reconstructs the payload.
##
## `Seg` is D9b's line table, and it is what makes an inline `Loc` exact rather
## than merely contained. Phase 1 hands phase 2 a leaf's payload with every
## marker already stripped, so a scanner offset into that payload is short by
## whatever came off the front of its line; a segment carries the offset back.
##
## **Inline locations are projected through that table; BLOCK locations are not**
## — a block's `content` is still `[outer]`. A block is stamped by phase 1, which
## knows the source span it is consuming and never needs to map an offset back;
## the split is why the two passes disagree about what a leaf is at all.
Loc := [].{
    ## A half-open byte interval `[start, end)` into the source.
    Ivl : { start : U64, end : U64 }

    ## The location itself.
    T : { outer : Loc.Ivl, content : List(Loc.Ivl) }

    ## An interval.
    ivl : U64, U64 -> Loc.Ivl
    ivl = |start, end| { start, end }

    ## A location spanning `[start, end)`, its content the whole interval.
    at : U64, U64 -> Loc.T
    at = |start, end| { outer: { start, end }, content: [{ start, end }] }

    ## A zero-width location at `pos`, for a synthesized node with no source of
    ## its own (a padded table cell, a checkbox the parser inferred).
    none : U64 -> Loc.T
    none = |pos| { outer: { start: pos, end: pos }, content: [] }

    ## Shift a location by `base` and clamp its far edge to `cap`, moving a
    ## block-relative interval into the document's absolute byte space. The clamp
    ## guards the one case where phase-1 text is LONGER than its source span — a
    ## tab expanded to spaces — so a shifted inline interval cannot poke past the
    ## block that contains it.
    shift : Loc.T, U64, U64 -> Loc.T
    shift = |loc, base, cap|
        {
            outer: Loc.shift_ivl(loc.outer, base, cap),
            content: List.map(loc.content, |c| Loc.shift_ivl(c, base, cap)),
        }

    shift_ivl : Loc.Ivl, U64, U64 -> Loc.Ivl
    shift_ivl = |i, base, cap| {
        start: Loc.min(base + i.start, cap),
        end: Loc.min(base + i.end, cap),
    }

    min : U64, U64 -> U64
    min = |a, b| if a < b { a } else { b }

    ## ## Line tables (D9b)

    ## One line of a leaf's payload, anchored to the source it came from: the
    ## text phase 1 KEPT, and the source offset of that text's first byte.
    ##
    ## A leaf's payload is a list of these rather than one string because the
    ## amount stripped is decided per LINE — `> a\nb` and `> a\n> b` produce
    ## identical text from different sources, and a soft-wrapped entry's second
    ## line carries continuation indent its first does not. One delta per block
    ## cannot express that, which is why a constant shift was wrong.
    Seg : { off : U64, text : Str }

    ## Anchor `text` within the source line spanning `line`.
    ##
    ## `tail` is `text` PLUS whatever phase 1 stripped after it, so the offset
    ## can be measured backwards from the line's end. That direction is the
    ## whole trick: `Lex.expand_tabs` rewrites a line's PREFIX, so counting
    ## forwards from the line's start crosses bytes that no longer correspond to
    ## the source, while everything from the first content byte onward is
    ## verbatim. Measuring from the end never touches the expanded region.
    ##
    ## The clamp covers the one case where the payload is LONGER than its
    ## source: a tab expanded to spaces of which only some were then stripped.
    seg : Loc.Ivl, Str, Str -> Loc.Seg
    seg = |line, tail, text| {
        n = Str.count_utf8_bytes(tail)
        off = if line.start + n <= line.end {
            line.end - n
        } else {
            line.start
        }
        { off, text }
    }

    ## A segment whose text had nothing stripped after it.
    seg_of : Loc.Ivl, Str -> Loc.Seg
    seg_of = |line, text| Loc.seg(line, text, text)

    ## The payload a segment list carries: its lines, joined the way phase 1
    ## joined them. `Scan` reads this; `project` maps its offsets back.
    joined : List(Loc.Seg) -> Str
    joined = |segs| Str.join_with(List.map(segs, |s| s.text), "\n")

    ## Trailing whitespace comes off the payload's LAST line — the only line it
    ## can be on, since a blank line ends a paragraph before it is buffered.
    trim_last : List(Loc.Seg) -> List(Loc.Seg)
    trim_last = |segs|
        match List.last(segs) {
            Ok(s) => List.append(List.drop_last(segs, 1), { ..s, text: Str.trim_end(s.text) })
            Err(_) => segs
        }

    ## Map a payload interval `[start, end)` back to the source.
    ##
    ## `content` is one interval per line the payload interval touches, which is
    ## why it is a LIST: an inline node spanning a soft line break is two
    ## disjoint source ranges with the second line's stripped prefix between
    ## them. `outer` spans from the first to the last.
    project : List(Loc.Seg), U64, U64 -> Loc.T
    project = |segs, start, end| {
        parts = Loc.parts(segs, start, end, 0, [])
        match List.first(parts) {
            Err(_) => Loc.none(Loc.point(segs, start))
            Ok(f) =>
                match List.last(parts) {
                    Ok(l) => { outer: { start: f.start, end: l.end }, content: parts }
                    Err(_) => { outer: f, content: parts }
                }
        }
    }

    parts : List(Loc.Seg), U64, U64, U64, List(Loc.Ivl) -> List(Loc.Ivl)
    parts = |segs, start, end, p, out|
        match List.first(segs) {
            Err(_) => out
            Ok(s) => {
                n = Str.count_utf8_bytes(s.text)
                lo = Loc.max(start, p)
                hi = Loc.min(end, p + n)
                next = if hi > lo {
                    List.append(out, { start: s.off + (lo - p), end: s.off + (hi - p) })
                } else {
                    out
                }
                ## `+ 1` for the newline `joined` put between the lines. It is a
                ## payload byte with no source of its own — the source has the
                ## stripped prefix there instead — so no interval covers it.
                Loc.parts(List.drop_first(segs, 1), start, end, p + n + 1, next)
            }
        }

    ## Where one payload offset lands in the source, for a zero-width node.
    point : List(Loc.Seg), U64 -> U64
    point = |segs, pos| Loc.point_in(segs, pos, 0, 0)

    point_in : List(Loc.Seg), U64, U64, U64 -> U64
    point_in = |segs, pos, p, last|
        match List.first(segs) {
            Err(_) => last
            Ok(s) => {
                n = Str.count_utf8_bytes(s.text)
                if pos <= p + n {
                    s.off + (Loc.max(pos, p) - p)
                } else {
                    Loc.point_in(List.drop_first(segs, 1), pos, p + n + 1, s.off + n)
                }
            }
        }

    max : U64, U64 -> U64
    max = |a, b| if a > b { a } else { b }
}
