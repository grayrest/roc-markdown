## Blocks to a semantic HTML fragment.
import Inl exposing [Inl]
import Block exposing [Block]

Html := [].{
    ## Block separators and a trailing newline are part of the OUTPUT SHAPE the
    ## CommonMark spec defines, not cosmetics — `spec/` compares exactly, and an
    ## empty document is the empty string rather than a bare newline.
    render : List(Block), Bool -> Str
    render = |blocks, safe|
        if List.is_empty(blocks) {
            ""
        } else {
            Str.concat(Str.join_with(List.map(blocks, |b| Html.block(b, safe)), "\n"), "\n")
        }

    block : Block, Bool -> Str
    block = |b, safe|
        match b.node {
            Para(sp) => "<p>${Html.spans(sp, safe)}</p>"
            Heading(lvl, sp) => {
                tag = Html.heading_tag(lvl)
                "<${tag}>${Html.spans(sp, safe)}</${tag}>"
            }
            ## `lang` becomes `class="language-…"`, the convention highlighters
            ## read. A bare fence gets a bare `<code>` rather than an empty
            ## class attribute.
            ## The content ends with a NEWLINE, because a code block's last line
            ## has one — an empty block is the one case with nothing to end.
            Code(c) => {
                body = if Str.is_empty(c.text) {
                    ""
                } else {
                    "${Html.escape(c.text)}\n"
                }
                if Str.is_empty(c.lang) {
                    "<pre><code>${body}</code></pre>"
                } else {
                    "<pre><code class=\"language-${Html.escape(c.lang)}\">${body}</code></pre>"
                }
            }
            Quote(bs) =>
                if List.is_empty(bs) {
                    "<blockquote>\n</blockquote>"
                } else {
                    "<blockquote>\n${Str.join_with(List.map(bs, |x| Html.block(x, safe)), "\n")}\n</blockquote>"
                }
            Items(l) => Html.items(l, safe)
            Table(t) => Html.table(t.ncols, t.align, t.cells, safe)
            Html(h) =>
                if safe {
                    "<!-- raw HTML omitted -->"
                } else {
                    h
                }
            Rule => "<hr />"
        }

    heading_tag : U8 -> Str
    heading_tag = |lvl|
        if lvl <= 1 {
            "h1"
        } else if lvl == 2 {
            "h2"
        } else if lvl == 3 {
            "h3"
        } else if lvl == 4 {
            "h4"
        } else if lvl == 5 {
            "h5"
        } else {
            "h6"
        }

    ## ## Lists
    ##
    ## The parser hands over real nesting, so this is a walk rather than the
    ## depth-run reconstruction it used to be — a nested list is simply an
    ## `Items` block inside an entry, and recursion handles it for free.
    ##
    ## `tight` decides whether an entry's paragraph keeps its `<p>`. That is a
    ## property of the source (were the entries separated by blank lines?), so
    ## the parser decides it and this only obeys.
    items : { ordered : Bool, start : U64, tight : Bool, entries : List(List(Block)) }, Bool -> Str
    items = |l, safe| {
        tag = if l.ordered {
            "ol"
        } else {
            "ul"
        }
        ## `start` is emitted only when it is not 1: the spec's expected output
        ## carries no explicit `start="1"`, and neither should this.
        attr = if l.ordered and l.start != 1 {
            " start=\"${l.start.to_str()}\""
        } else {
            ""
        }
        body = Str.join_with(List.map(l.entries, |e| Html.entry(e, l.tight, safe)), "")
        "<${tag}${attr}>\n${body}</${tag}>"
    }

    entry : List(Block), Bool, Bool -> Str
    entry = |bs, tight, safe|
        if List.is_empty(bs) {
            ## An empty entry is empty in either mode — a loose one would
            ## otherwise render a blank line as its content.
            "<li></li>\n"
        } else if tight {
            ## Every block opens on its own line except a paragraph that opens
            ## the entry — that one runs on from the `<li>`, which is what
            ## "tight" means. The entry closes on its own line unless it ends on
            ## such a paragraph.
            ##
            ## One newline per block, decided here rather than inside each
            ## piece: giving a block both of its newlines put a blank line
            ## between two adjacent ones.
            body = Str.join_with(
                List.map_with_index(bs, |b, i| Html.tight_at(b, i, safe)),
                "",
            )
            tail = match List.last(bs) {
                Ok(b) =>
                    match b.node {
                        Para(_) => ""
                        _ => "\n"
                    }
                _ => "\n"
            }
            "<li>${body}${tail}</li>\n"
        } else {
            "<li>\n${Str.join_with(List.map(bs, |b| Html.block(b, safe)), "\n")}\n</li>\n"
        }

    ## In a tight entry the paragraph sheds its `<p>`; anything else — a nested
    ## list, a fence — is a real block and opens on its own line.
    tight_at : Block, U64, Bool -> Str
    tight_at = |b, i, safe|
        match b.node {
            Para(ns) =>
                if i == 0 {
                    Html.spans(ns, safe)
                } else {
                    "\n${Html.spans(ns, safe)}"
                }
            _ => "\n${Html.block(b, safe)}"
        }

    table : U64, List(Block.Align), List(Block.Cell), Bool -> Str
    table = |ncols, align, cells, safe|
        if ncols == 0 {
            "<table>\n</table>"
        } else {
            head = Html.row("th", List.take_first(cells, ncols), align, safe)
            body = Html.rows(List.drop_first(cells, ncols), ncols, align, safe)
            ## A header-only table has no body, and an empty `<tbody>` is not
            ## what the spec's expected output carries.
            if Str.is_empty(body) {
                "<table>\n<thead>\n${head}</thead>\n</table>"
            } else {
                "<table>\n<thead>\n${head}</thead>\n<tbody>\n${body}</tbody>\n</table>"
            }
        }

    rows : List(Block.Cell), U64, List(Block.Align), Bool -> Str
    rows = |cells, ncols, align, safe|
        if List.is_empty(cells) {
            ""
        } else {
            Str.concat(
                Html.row("td", List.take_first(cells, ncols), align, safe),
                Html.rows(List.drop_first(cells, ncols), ncols, align, safe),
            )
        }

    row : Str, List(Block.Cell), List(Block.Align), Bool -> Str
    row = |tag, cs, align, safe| {
        cells = List.map_with_index(cs, |c, i| {
            a = Html.align_attr(align, i)
            "<${tag}${a}>${Html.spans(c, safe)}</${tag}>\n"
        })
        "<tr>\n${Str.join_with(cells, "")}</tr>\n"
    }

    align_attr : List(Block.Align), U64 -> Str
    align_attr = |align, i|
        match List.get(align, i) {
            Ok(Left) => " align=\"left\""
            Ok(Center) => " align=\"center\""
            Ok(Right) => " align=\"right\""
            ## `None`, and a column with no entry at all — a delimiter row
            ## shorter than its header. Both mean no attribute.
            _ => ""
        }

    spans : List(Inl), Bool -> Str
    spans = |ns, safe| Str.join_with(List.map(ns, |n| Html.node(n, safe)), "")

    ## Nesting is structural here, which is the whole reason the inline phase
    ## produces a tree: `**a *b* c**` is one `<strong>` around an `<em>`, and a
    ## flat run list can only emit three adjacent `<strong>`s.
    node : Inl, Bool -> Str
    node = |n, safe|
        match n {
            Text(t) => Html.escape(t)
            Code(t) => "<code>${Html.escape(t)}</code>"
            Emph(ks) => "<em>${Html.spans(ks, safe)}</em>"
            Strong(ks) => "<strong>${Html.spans(ks, safe)}</strong>"
            Strike(ks) => "<del>${Html.spans(ks, safe)}</del>"
            ## `href` is escaped as well as allowlisted. The allowlist stops a
            ## `javascript:` scheme; the escaping stops a `"` closing the
            ## attribute early. They guard different holes.
            Link(l) => "<a href=\"${Html.href(l.href)}\"${Html.title_attr(l.title)}>${Html.spans(l.kids, safe)}</a>"
            ## An image's alt text is the subtree's TEXT, markup discarded —
            ## `![*a*](u)` is `alt="a"`, per the spec.
            Image(m) => "<img src=\"${Html.href(m.src)}\" alt=\"${Html.escape(Inl.text_of(m.kids))}\"${Html.title_attr(m.title)} />"
            Task(done) =>
                if done {
                    "<input checked=\"\" disabled=\"\" type=\"checkbox\"> "
                } else {
                    "<input disabled=\"\" type=\"checkbox\"> "
                }
            SoftBreak => "\n"
            HardBreak => "<br />\n"
            ## The ONE place the two lowerings differ, which is why this is a
            ## flag and not a pre-pass over the tree: rebuilding every node to
            ## replace this one took `roc test` from 0.55 s to over 600 s.
            Raw(r) =>
                if safe {
                    "<!-- raw HTML omitted -->"
                } else {
                    r
                }
        }

    title_attr : Str -> Str
    title_attr = |t|
        if Str.is_empty(t) {
            ""
        } else {
            " title=\"${Html.escape(t)}\""
        }

    ## A URL, percent-encoded then HTML-escaped.
    ##
    ## Both, in that order, and they are not the same job: percent-encoding
    ## makes the bytes legal in a URL (a space, a quote, any non-ASCII byte),
    ## and escaping makes them legal in an attribute. An existing `%` is left
    ## alone so an already-encoded URL is not double-encoded.
    href : Str -> Str
    href = |u| Html.escape(Str.from_utf8_lossy(Html.pct(Str.to_utf8(u), 0, [])))

    pct : List(U8), U64, List(U8) -> List(U8)
    pct = |b, i, out|
        match List.get(b, i) {
            Ok(c) =>
                if Html.url_safe(c) {
                    Html.pct(b, i + 1, List.append(out, c))
                } else {
                    Html.pct(b, i + 1, List.concat(out, Html.hex_of(c)))
                }
            Err(_) => out
        }

    ## The set cmark leaves alone. Deliberately generous: over-encoding a URL
    ## breaks it, while under-encoding only makes it ugly.
    url_safe : U8 -> Bool
    url_safe = |c|
        (c >= 48 and c <= 57)
        or (c >= 65 and c <= 90)
        or (c >= 97 and c <= 122)
        ## `[` and `]` are NOT safe: the spec percent-encodes them, and leaving
        ## them raw is the difference between `%5C%5B` and `%5C[`.
        or List.contains([45, 95, 46, 43, 33, 42, 39, 40, 41, 44, 37, 35, 64, 63, 61, 59, 58, 47, 38, 36, 126], c)

    hex_of : U8 -> List(U8)
    hex_of = |c| [37, Html.nib(c / 16), Html.nib(c % 16)]

    nib : U8 -> U8
    nib = |n|
        if n < 10 {
            48 + n
        } else {
            55 + n
        }

    esc_bytes : List(U8), U64, List(U8) -> List(U8)
    esc_bytes = |b, i, out|
        match List.get(b, i) {
            Err(_) => out
            Ok(c) =>
                if c == 38 {
                    Html.esc_bytes(b, i + 1, List.concat(out, Str.to_utf8("&amp;")))
                } else if c == 60 {
                    Html.esc_bytes(b, i + 1, List.concat(out, Str.to_utf8("&lt;")))
                } else if c == 62 {
                    Html.esc_bytes(b, i + 1, List.concat(out, Str.to_utf8("&gt;")))
                } else if c == 34 {
                    Html.esc_bytes(b, i + 1, List.concat(out, Str.to_utf8("&quot;")))
                } else {
                    Html.esc_bytes(b, i + 1, List.append(out, c))
                }
        }

    ## Escape `&`, `<`, `>` and `"`, and do nothing at all when there is
    ## nothing to escape.
    ##
    ## The guard is the point. Most text contains none of these, and the four
    ## `Str.replace_each`es this replaces allocated up to four times per piece
    ## regardless. `Str.to_utf8` is free — a seamless slice — and `contains`
    ## only scans, so the common path returns the input untouched.
    ##
    ## `&` is handled FIRST inside the walk for the usual reason: replacing it
    ## after `<` would turn the `&lt;` just written into `&amp;lt;`. Doing it in
    ## one pass sidesteps that entirely, since a byte is examined once and
    ## written once.
    escape : Str -> Str
    escape = |s|
        if s.contains("&") or s.contains("<") or s.contains(">") or s.contains("\"") {
            Str.from_utf8_lossy(Html.esc_bytes(Str.to_utf8(s), 0, []))
        } else {
            s
        }
}
