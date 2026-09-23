## A styled run of text — the vocabulary of a rich paragraph.
##
## **A NOMINAL, and one file per published type.**
##
## This used to be forced: a bare type alias could not cross a module boundary
## without HANGING the compiler outright (`rc=142`, no output), while the
## identical file with the alias restated locally checked in 502 ms. **That is
## fixed** — re-probed 2026-08-14 on `release-fast-28c3e6f7`, where a member
## alias on a nominal (`Ref.Def`) checks in 16 ms and survives lowering. Shared
## type vocabulary across modules is available again; see `Ref.roc`, which is
## exactly the shape the old bug forbade.
##
## Nominals stay here anyway, for the ordinary reason: `Span` has methods.
##
## `link` is `""` when the span is not a link — a sentinel rather than a tag,
## and it is NOT airtight. `Link.safe_href` is a DENYLIST — `javascript:`,
## `vbscript:`, `data:`, `file:` — so an empty destination passes it, and
## `[x]()` parses to a `Link` whose href is `""`. Flattened, that is a span this
## type cannot distinguish from ordinary text, while `Html.render` lowers the
## same input to `<a href="">x</a>`. The two lowerings disagree exactly there.
##
## The trade was made knowingly: a link to nowhere is not worth a tag on every
## run. This is the input that pays for it.
##
import Inl exposing [Inl]

## `bold`, `italic` and `code` are independent flags, so the record can express
## combinations the parser does not currently produce (bold code, an italic
## link). That is deliberate slack: a `kind`-per-span design *could not*
## represent `**a *b***` even in principle, so a nesting-aware inline pass would
## be a change to the type and every consumer of it. Flags make it a change to
## the parser alone.
Span := { text : Str, bold : Bool, italic : Bool, code : Bool, link : Str }.{
    ## `<br>`, `<br/>`, `<br />`, any case.
    is_br : Str -> Bool
    is_br = |t| {
        inner = Str.drop_suffix(t.drop_prefix("<"), ">").trim()
        stripped = Str.drop_suffix(inner, "/").trim()
        stripped == "br" or stripped == "BR" or stripped == "Br" or stripped == "bR"
    }

    ## The tree lowered to flat runs — the second consumer of a parse, beside
    ## HTML, and the one every native renderer wants: a shaper takes runs.
    ##
    ## Nesting collapses into the flags, which is exactly the information a run
    ## list can carry. `**a *b* c**` becomes three runs, the middle one bold AND
    ## italic — the same picture on screen, and the reason this lowering is
    ## lossless for rendering even though it is lossy for structure.
    flatten : List(Inl) -> List(Span)
    flatten = |ns| Span.walk(ns, { bold: False, italic: False, code: False, link: "" })

    walk : List(Inl), { bold : Bool, italic : Bool, code : Bool, link : Str } -> List(Span)
    walk = |ns, st| List.join(List.map(ns, |n| Span.lower(n, st)))

    lower : Inl, { bold : Bool, italic : Bool, code : Bool, link : Str } -> List(Span)
    lower = |n, st|
        match n.node {
            Text(t) => [Span.{ text: t, bold: st.bold, italic: st.italic, code: st.code, link: st.link }]
            Code(t) => [Span.{ text: t, bold: st.bold, italic: st.italic, code: True, link: st.link }]
            Emph(ks) => Span.walk(ks, { ..st, italic: True })
            Strong(ks) => Span.walk(ks, { ..st, bold: True })
            ## No strikethrough flag on a run, so the text survives unmarked
            ## rather than vanishing.
            Strike(ks) => Span.walk(ks, st)
            Link(l) => Span.walk(l.kids, { ..st, link: l.href })
            ## An image becomes its alt text. A `Span` is a styled run and an
            ## image is a box with an intrinsic size; it belongs beside `Code` in
            ## `Block` if it belongs anywhere.
            Image(m) => Span.walk(m.kids, st)
            ## A soft break is a SPACE to a reflowing renderer — the newline is
            ## the source's wrapping, not the author's.
            SoftBreak => [Span.{ text: " ", bold: st.bold, italic: st.italic, code: st.code, link: st.link }]
            HardBreak => [Span.{ text: "\n", bold: st.bold, italic: st.italic, code: st.code, link: st.link }]
            ## `<br>` is the one raw tag with a native meaning — it is a line
            ## break, and this renderer has those. Everything else becomes
            ## LITERAL TEXT rather than vanishing: a native renderer has no HTML
            ## engine, and silently dropping content a document contains is the
            ## one outcome a viewer must not have.
            ## A checkbox has no run of its own, so it renders as the glyph a
            ## reader expects. `Rtext` has no widget slot and inventing one for
            ## this would be a platform change, not a markdown one.
            Task(done) => [
                Span.{
                    text: if done {
                        "☑ "
                    } else {
                        "☐ "
                    },
                    bold: st.bold,
                    italic: st.italic,
                    code: st.code,
                    link: st.link,
                },
            ]
            Raw(raw) =>
                if Span.is_br(raw) {
                    [Span.{ text: "\n", bold: st.bold, italic: st.italic, code: st.code, link: st.link }]
                } else {
                    [Span.{ text: raw, bold: st.bold, italic: st.italic, code: st.code, link: st.link }]
                }
        }

    ## A span in the paragraph's own weight and color.
    plain : Str -> Span
    plain = |t| Span.{ text: t, bold: False, italic: False, code: False, link: "" }

    bold : Str -> Span
    bold = |t| Span.{ text: t, bold: True, italic: False, code: False, link: "" }

    italic : Str -> Span
    italic = |t| Span.{ text: t, bold: False, italic: True, code: False, link: "" }

    code : Str -> Span
    code = |t| Span.{ text: t, bold: False, italic: False, code: True, link: "" }

    link : Str, Str -> Span
    link = |t, href| Span.{ text: t, bold: False, italic: False, code: False, link: href }

    is_link : Span -> Bool
    is_link = |s| !Str.is_empty(s.link)
}
