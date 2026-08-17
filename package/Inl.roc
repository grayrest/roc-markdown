## The inline TREE — what a paragraph's content actually is.
##
## `Span` is a flat run with independent flags, and that shape cannot express
## CommonMark: `**a *b* c**` is one `<strong>` containing an `<em>`, and a flat
## run list lowers it to three adjacent `<strong>`s. The spec compares output
## exactly, so nesting has to be REPRESENTED, not approximated.
Inl := [
    ## **`Str`, and that is a measured choice, not the obvious one.**
    ##
    ## These started as `List(U8)` with the reasoning that
    ## `Str.to_utf8` is free — a seamless slice sharing the allocation — while
    ## `Str.from_utf8_lossy` allocates and walks its input twice, so the parser
    ## should slice and let "the output path" pay for the conversion.
    ##
    ## That is right for HTML, which lowers a document once. It is wrong here,
    ## because **the other output path can run multiple times per second**.
    ##
    ## The asymmetry that motivated the change argues for `Str` once you follow
    ## it all the way: because `to_utf8` is free and `from_utf8_lossy` is not,
    ## a `Str` leaf is viewable as bytes for nothing, while a byte leaf costs an
    ## allocation every time anyone wants text. `Str` is the form that is cheap
    ## in both directions.
    Text(Str),
    ## Already-decoded: `&amp;` and `\!` have become `&` and `!` by the time they
    ## are here, so a renderer escapes for its own medium and never un-escapes.
    Code(Str),
    Emph(List(Inl)),
    Strong(List(Inl)),
    Strike(List(Inl)),
    Link({ href : Str, title : Str, kids : List(Inl) }),
    Image({ src : Str, title : Str, kids : List(Inl) }),
    ## A newline inside a paragraph. Rendered as `\n` in HTML and as a SPACE by
    ## a reflowing native renderer — the distinction the flat model lost.
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
].{
    ## The concatenated text of a subtree, used for an image's alt attribute —
    ## the spec renders `![*a*](u)` as `alt="a"`, markup discarded.
    text_of : List(Inl) -> Str
    text_of = |ks| Str.join_with(List.map(ks, Inl.one_text), "")

    one_text : Inl -> Str
    one_text = |i|
        match i {
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
