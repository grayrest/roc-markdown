## The inline TREE — what a paragraph's content actually is.
##
## `Span` is a flat run with independent flags, and that shape cannot express
## CommonMark: `**a *b* c**` is one `<strong>` containing an `<em>`, and a flat
## run list lowers it to three adjacent `<strong>`s. The spec compares output
## exactly, so nesting has to be REPRESENTED, not approximated.
##
## Every inline node carries `loc`, the SOURCE byte interval it spans, markers
## and all (`plans/2026-08-26-rocsidian.md` D9/T0b). `**bold**`'s `Strong` spans
## the two `**` runs as well as the word, so the editor can elide the markers and
## reveal them under the caret. The offsets are captured during the scan and
## carried through emphasis matching; matching a node means matching `i.node`.
import Loc exposing [Loc]

Inl := { loc : Loc.T, node : Inl.Node }.{
    Node : [
        ## **`Str`, and that is a measured choice, not the obvious one.**
        ##
        ## These started as `List(U8)` with the reasoning that `Str.to_utf8` is
        ## free — a seamless slice sharing the allocation — while
        ## `Str.from_utf8_lossy` allocates and walks its input twice, so the
        ## parser should slice and let "the output path" pay for the conversion.
        ##
        ## That is right for HTML, which lowers a document once. It is wrong here,
        ## because **the other output path can run multiple times per second**.
        ##
        ## The asymmetry argues for `Str` once you follow it all the way: because
        ## `to_utf8` is free and `from_utf8_lossy` is not, a `Str` leaf is viewable
        ## as bytes for nothing, while a byte leaf costs an allocation every time
        ## anyone wants text. `Str` is the form that is cheap in both directions.
        Text(Str),
        ## Already-decoded: `&amp;` and `\!` have become `&` and `!` by the time
        ## they are here, so a renderer escapes for its own medium and never
        ## un-escapes. **`loc` still spans the SOURCE** — `a&amp;b` is ten source
        ## bytes with ten caret positions even though its payload is five (D11).
        Code(Str),
        Emph(List(Inl)),
        Strong(List(Inl)),
        Strike(List(Inl)),
        Link({ href : Str, title : Str, kids : List(Inl) }),
        Image({ src : Str, title : Str, kids : List(Inl) }),
        ## A newline inside a paragraph. Rendered as `\n` in HTML and as a SPACE
        ## by a reflowing native renderer — the distinction the flat model lost.
        SoftBreak,
        HardBreak,
        ## Raw inline HTML, already passed through GFM's disallowed-tag filter.
        Raw(Str),
        ## A GFM task-list checkbox, at the head of a list entry's first paragraph.
        ##
        ## An INLINE node rather than a field on the entry: it keeps `Items` the
        ## same shape for every list, and it is genuinely inline — it sits in the
        ## text flow and a renderer draws it beside the words.
        Task(Bool),
    ]

    ## Build an inline node spanning `[start, end)`.
    at : U64, U64, Inl.Node -> Inl
    at = |start, end, node| Inl.{ loc: Loc.at(start, end), node }

    ## Build one from a location and node.
    mk : Loc.T, Inl.Node -> Inl
    mk = |loc, node| Inl.{ loc, node }

    ## Map a subtree's locations from PAYLOAD offsets to SOURCE offsets, through
    ## the leaf's line table (D9b). `Lower` calls it once per block.
    ##
    ## This is what a plain shift could not do. The scanner's offsets are into
    ## the text phase 1 handed it — markers, quote prefixes and indentation
    ## already gone — and how much came off is a fact about each LINE, so the
    ## table is consulted per line rather than a single base added to everything.
    project_all : List(Inl), List(Loc.Seg) -> List(Inl)
    project_all = |ks, segs| List.map(ks, |k| Inl.project(k, segs))

    project : Inl, List(Loc.Seg) -> Inl
    project = |i, segs| {
        node = match i.node {
            Emph(ks) => Emph(Inl.project_all(ks, segs))
            Strong(ks) => Strong(Inl.project_all(ks, segs))
            Strike(ks) => Strike(Inl.project_all(ks, segs))
            Link(l) => Link({ ..l, kids: Inl.project_all(l.kids, segs) })
            Image(m) => Image({ ..m, kids: Inl.project_all(m.kids, segs) })
            other => other
        }
        Inl.{ loc: Loc.project(segs, i.loc.outer.start, i.loc.outer.end), node }
    }

    ## Shift a subtree's locations by `base`, clamping to `cap`. Both offsets are
    ## in the SAME payload — `Scan` uses it to lift a reference label's inlines
    ## into the enclosing scan — so no line table is involved and none is needed.
    shift_all : List(Inl), U64, U64 -> List(Inl)
    shift_all = |ks, base, cap| List.map(ks, |k| Inl.shift(k, base, cap))

    shift : Inl, U64, U64 -> Inl
    shift = |i, base, cap| {
        node = match i.node {
            Emph(ks) => Emph(Inl.shift_all(ks, base, cap))
            Strong(ks) => Strong(Inl.shift_all(ks, base, cap))
            Strike(ks) => Strike(Inl.shift_all(ks, base, cap))
            Link(l) => Link({ ..l, kids: Inl.shift_all(l.kids, base, cap) })
            Image(m) => Image({ ..m, kids: Inl.shift_all(m.kids, base, cap) })
            other => other
        }
        Inl.{ loc: Loc.shift(i.loc, base, cap), node }
    }

    ## The concatenated text of a subtree, used for an image's alt attribute —
    ## the spec renders `![*a*](u)` as `alt="a"`, markup discarded.
    text_of : List(Inl) -> Str
    text_of = |ks| Str.join_with(List.map(ks, Inl.one_text), "")

    one_text : Inl -> Str
    one_text = |i|
        match i.node {
            Text(t) => t
            Code(t) => t
            Raw(_) => ""
            Task(_) => ""
            SoftBreak => "\n"
            HardBreak => "\n"
            Emph(ks) => Inl.text_of(ks)
            Strong(ks) => Inl.text_of(ks)
            Strike(ks) => Inl.text_of(ks)
            Link(l) => Inl.text_of(l.kids)
            Image(m) => Inl.text_of(m.kids)
        }
}
