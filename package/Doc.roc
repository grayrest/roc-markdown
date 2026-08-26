## The block tree, with its leaves still RAW.
##
## Phase 1's output and phase 2's input. Same shape as `Block` — the same
## containers, the same nesting — except that everything an inline scanner would
## touch is still the source text it came from: a paragraph is a `Str`, a table
## is its unparsed rows.
##
## **Two types rather than one, because the passes genuinely disagree about the
## leaves.** A link reference definition may be written after the reference that
## uses it, so inlines cannot be resolved until the whole document has been
## seen.
##
## Every node carries `at`, the SOURCE byte interval it spans
## (`plans/2026-08-26-rocsidian.md` D9). Phase 1 stamps it as lines flow, and
## `Lower` carries it onto the `Block` it produces. `at` is strip-invariant —
## quote stripping and tab expansion change a node's text but not where it began
## and ended in the source — so it is exact even where the payload is not a
## verbatim slice.
import Loc exposing [Loc]

Doc := { at : Loc.Ivl, node : Doc.Node }.{
    Node : [
        ## Raw paragraph text: soft-wrapped source lines joined with `\n`, so the
        ## inline scanner can still see where the breaks were and emit `SoftBreak`.
        Para(Str),
        ## `Heading(level, raw)` — level 1..6, ATX or setext. The raw text has
        ## already had its markers and any closing sequence stripped, because that
        ## is lexical and belongs to phase 1.
        Heading(U8, Str),
        ## Already final: a fence's contents are not inline-parsed in either pass,
        ## so this leaf is carried through untouched.
        Code({ lang : Str, text : Str }),
        Quote(List(Doc)),
        ## Entries carry their GFM task marker separately rather than as text.
        ##
        ## `[ ]` at the head of an entry is positional — it is only a checkbox
        ## because of where it sits — so it has to be recognised while that position
        ## is still known, which is phase 1. Phase 2 turns it back into the leading
        ## `Inl.Task` node that `Block` wants.
        Items({
            ordered : Bool,
            start : U64,
            tight : Bool,
            entries : List({ task : [NoTask, Todo, Done], blocks : List(Doc) }),
        }),
        ## The table's raw lines, delimiter row and all.
        ##
        ## Kept unsplit because a cell's text is inline content, and because the
        ## header/delimiter/body distinction is only decidable once the run is
        ## complete — which it is, by the time this is built.
        Table(List(Str)),
        Html(Str),
        Rule,
    ]

    ## Build a `Doc` from its source interval and node.
    mk : Loc.Ivl, Doc.Node -> Doc
    mk = |at, node| Doc.{ at, node }
}
