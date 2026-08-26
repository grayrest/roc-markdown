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
## In this cut `content` is `[outer]` for every node — containment holds, which
## is what the editor's decoration and reveal logic needs. The exact
## per-segment reconstruction (D9b's line tables) is the refinement that makes
## the T0 gate's assertion (d) hold at every depth for stripped content.
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
}
