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

    ## Where each segment's text starts in the payload `joined` builds: 0, then
    ## one past the end of the segment before, for the newline between them.
    ##
    ## A leaf builds this once, and `project` and `point` binary-search it for
    ## the line an offset falls on. They used to walk the segments from the
    ## first for every inline node, so a paragraph of N lines cost N², about
    ## 19 s for 10,000 lines.
    line_starts : List(Loc.Seg) -> List(U64)
    line_starts = |segs| {
        built = List.fold(
            segs,
            { next: 0, out: [] },
            |acc, s| { next: acc.next + Str.count_utf8_bytes(s.text) + 1, out: List.append(acc.out, acc.next) },
        )
        built.out
    }

    ## Map a payload interval `[start, end)` back to the source. `starts` is
    ## `Loc.line_starts(segs)`.
    ##
    ## `content` is one interval per line the payload interval touches, which is
    ## why it is a LIST: an inline node spanning a soft line break is two
    ## disjoint source ranges with the second line's stripped prefix between
    ## them. `outer` spans from the first to the last.
    project : List(Loc.Seg), List(U64), U64, U64 -> Loc.T
    project = |segs, starts, start, end| {
        parts = Loc.parts(segs, starts, start, end, Loc.line_of(starts, start), [])
        match List.first(parts) {
            Err(_) => Loc.none(Loc.point(segs, starts, start))
            Ok(f) =>
                match List.last(parts) {
                    Ok(l) => { outer: { start: f.start, end: l.end }, content: parts }
                    Err(_) => { outer: f, content: parts }
                }
        }
    }

    ## The source intervals of segments `k` onward that `[start, end)` touches.
    ## `project` starts at the line `start` falls on: every segment before it
    ## ends before that line starts, so none of them can touch the interval.
    parts : List(Loc.Seg), List(U64), U64, U64, U64, List(Loc.Ivl) -> List(Loc.Ivl)
    parts = |segs, starts, start, end, k, out| {
        p = Loc.start_of(starts, k)
        if k >= List.len(segs) or p >= end {
            out
        } else {
            n = Loc.len_of(segs, k)
            lo = Loc.max(start, p)
            hi = Loc.min(end, p + n)
            next = if hi > lo {
                off = Loc.off_of(segs, k)
                List.append(out, { start: off + (lo - p), end: off + (hi - p) })
            } else {
                out
            }
            ## The next segment starts `+ 1` past this one's end, for the
            ## newline `joined` put between the lines. It is a payload byte
            ## with no source of its own — the source has the stripped prefix
            ## there instead — so no interval covers it.
            Loc.parts(segs, starts, start, end, k + 1, next)
        }
    }

    ## Where one payload offset lands in the source, for a zero-width node.
    ## Past the end of the last segment it is that segment's end.
    point : List(Loc.Seg), List(U64), U64 -> U64
    point = |segs, starts, pos| {
        k = Loc.line_of(starts, pos)
        p = Loc.start_of(starts, k)
        n = Loc.len_of(segs, k)
        off = Loc.off_of(segs, k)
        if pos <= p + n {
            off + (pos - p)
        } else {
            off + n
        }
    }

    ## The last segment that starts at or before payload offset `pos` — the
    ## line it falls on, or the newline after that line. 0 when there are no
    ## segments.
    line_of : List(U64), U64 -> U64
    line_of = |starts, pos| Loc.search(starts, pos, 0, List.len(starts))

    ## Binary search in `[lo, hi)`: segment `lo` starts at or before `pos`
    ## (the first segment starts at 0), and none from `hi` on does.
    search : List(U64), U64, U64, U64 -> U64
    search = |starts, pos, lo, hi|
        if hi - lo <= 1 {
            lo
        } else {
            mid = lo + (hi - lo) // 2
            if Loc.start_of(starts, mid) <= pos {
                Loc.search(starts, pos, mid, hi)
            } else {
                Loc.search(starts, pos, lo, mid)
            }
        }

    ## Segment `k`'s payload start, byte length and source offset. Each is 0
    ## past the last segment, which is what `point` answers for a leaf with
    ## none.
    start_of : List(U64), U64 -> U64
    start_of = |starts, k|
        match List.get(starts, k) {
            Ok(p) => p
            Err(_) => 0
        }

    len_of : List(Loc.Seg), U64 -> U64
    len_of = |segs, k|
        match List.get(segs, k) {
            Ok(s) => Str.count_utf8_bytes(s.text)
            Err(_) => 0
        }

    off_of : List(Loc.Seg), U64 -> U64
    off_of = |segs, k|
        match List.get(segs, k) {
            Ok(s) => s.off
            Err(_) => 0
        }

    max : U64, U64 -> U64
    max = |a, b| if a > b { a } else { b }
}
