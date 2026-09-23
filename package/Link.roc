## Links and images: the parts of `[label](dest "title")` and `[label][ref]`
## that are decided by looking rather than by scanning — bracket and paren
## matching, the destination and title, label folding for reference lookup, the
## no-link-in-a-link rule, and the href policy.
##
## Scanning the label and deciding what the bracket becomes (`Scan.bracket`,
## `Scan.resolve`, `Scan.settle`) stays in `Scan`: it scans the label with
## `Scan.tokens`, and a module cannot import the one that imports it.
import Ref
import Inl exposing [Inl]
import Ents
import Bytes
import Tag
import Autolink
import Emph

Link := [].{
    ## `dest "title"` — the title is optional and quoted; everything before the
    ## first space is the destination. `<dest>` sheds its angle brackets.
    ## `dest "title"`, with `ok` saying whether it is a destination at all.
    ##
    ## A BARE destination may not contain a space — `[link](/my uri)` is not a
    ## link, it is text — while an angle-bracketed one may, and may be empty.
    ## Splitting on the first space before deciding got both of those wrong.
    dest_title : Str -> { dest : Str, title : Str, ok : Bool }
    dest_title = |raw| {
        t = raw.trim()
        if t.starts_with("<") {
            tb = Str.to_utf8(t)
            ## The closing `>` must be UNESCAPED — `<foo\\>` never closes, so
            ## `[link](<foo\\>)` is text rather than a link.
            match Bytes.find_unescaped(tb, 1, 62) {
                Ok(gt) => {
                    inner = Bytes.slice(tb, 1, gt)
                    {
                        dest: Link.unescape(Ents.decode(inner)),
                        title: Link.unescape(Ents.decode(Link.unquote(Bytes.slice(tb, gt + 1, List.len(tb)).trim()))),
                        ## Whatever follows the closing `>` must be a title or
                        ## nothing — `[a](<b>c)` is text, not a link to `b`
                        ## titled `c`.
                        ok: !inner.contains("<")
                        and !inner.contains("\n")
                        and (
                            Str.is_empty(Bytes.slice(tb, gt + 1, List.len(tb)).trim())
                            or Link.is_quoted(Bytes.slice(tb, gt + 1, List.len(tb)).trim())
                        ),
                    }
                }
                Err(_) => { dest: "", title: "", ok: False }
            }
        } else {
            match Link.split_ws(t) {
                Ok(p) => {
                    rest = p.after.trim()
                    {
                        dest: Link.unescape(Ents.decode(p.before)),
                        title: Link.unescape(Ents.decode(Link.unquote(rest))),
                        ## Whatever follows the destination must be a quoted
                        ## title; anything else means the space was inside a
                        ## destination that is not allowed to hold one.
                        ok: Link.is_quoted(rest),
                    }
                }
                Err(_) => { dest: Link.unescape(Ents.decode(t)), title: "", ok: !t.contains("\n") }
            }
        }
    }

    ## `\(` in a destination is a literal paren. The inline scanner does this
    ## for ordinary text, but a destination is sliced out whole and never
    ## passes through it.
    unescape : Str -> Str
    unescape = |t|
        if t.contains("\\") {
            Str.from_utf8_lossy(Link.unesc(Str.to_utf8(t), 0, []))
        } else {
            t
        }

    unesc : List(U8), U64, List(U8) -> List(U8)
    unesc = |b, i, out|
        match List.get(b, i) {
            Ok(92) =>
                if Bytes.is_punct(Bytes.at(b, i + 1)) and i + 1 < List.len(b) {
                    Link.unesc(b, i + 2, List.append(out, Bytes.at(b, i + 1)))
                } else {
                    Link.unesc(b, i + 1, List.append(out, 92))
                }
            Ok(c) => Link.unesc(b, i + 1, List.append(out, c))
            Err(_) => out
        }

    ## A title is `"…"`, `'…'` or `(…)`, and the closing delimiter may not
    ## appear inside it — `"title "and" title"` is not a title, which is what
    ## makes that whole line ordinary text rather than a link.
    ## An ESCAPED delimiter does not close a title, so the interior test cannot
    ## be a plain `contains`.
    has_bare : List(U8), U64, U64, U8 -> Bool
    has_bare = |b, i, stop, c|
        if i >= stop {
            False
        } else if Bytes.at(b, i) == 92 {
            Link.has_bare(b, i + 2, stop, c)
        } else if Bytes.at(b, i) == c {
            True
        } else {
            Link.has_bare(b, i + 1, stop, c)
        }

    ## Split at the first WHITESPACE, not the first space. A destination may be
    ## followed by a newline before its title.
    split_ws : Str -> Try({ before : Str, after : Str }, [NotFound])
    split_ws = |t| {
        b = Str.to_utf8(t)
        i = Link.ws_at(b, 0)
        if i >= List.len(b) {
            Err(NotFound)
        } else {
            Ok({ before: Bytes.slice(b, 0, i), after: Bytes.slice(b, i + 1, List.len(b)) })
        }
    }

    ws_at : List(U8), U64 -> U64
    ws_at = |b, i|
        if i >= List.len(b) or Bytes.is_space(Bytes.at(b, i)) {
            i
        } else {
            Link.ws_at(b, i + 1)
        }

    is_quoted : Str -> Bool
    is_quoted = |t| {
        b = Str.to_utf8(t)
        n = List.len(b)
        if n < 2 {
            False
        } else {
            open = Bytes.at(b, 0)
            close = Bytes.at(b, n - 1)
            pair =
                (open == 34 and close == 34)
                or (open == 39 and close == 39)
                or (open == 40 and close == 41)
            ## An ESCAPED delimiter does not close the title, so this cannot
            ## be a plain `contains` — `"title \\"…"` is one title.
            pair and !Link.has_bare(b, 1, n - 1, close)
        }
    }

    ## The length test is not redundant: a lone `"` both starts and ends with a
    ## quote, and stripping one character off each end of a one-character string
    ## underflows.
    unquote : Str -> Str
    unquote = |t|
        if Link.is_quoted(t) {
            Bytes.slice(Str.to_utf8(t), 1, List.len(Str.to_utf8(t)) - 1)
        } else {
            t
        }

    ## What bracket walks have learned, by where a bracket's content starts:
    ## the `]` that closes it, or that none does.
    Known : Dict(U64, Try(U64, [NotFound]))

    ## The `]` closing the bracket whose content starts at `from`, by balanced
    ## counting, and `known` grown by the answer for every `[` the walk passed.
    ##
    ## The walk is a pure function of where it is, and a `[` it passes at depth
    ## `d` closes exactly where the walk next returns to `d`, so each answer is
    ## what a walk of its own would find. That is cmark's bracket stack
    ## reached another way: `"[" * 30_000 + "]"` walks once rather than from
    ## every `[` to the `]` at the end. A `[` whose answer is already known is
    ## stepped over, or ends the walk when nothing closes it.
    ##
    ## `gt_end` is one past the last `>` in `b` (`opaque_end`).
    match_bracket : List(U8), U64, U64, Link.Known -> { close : Try(U64, [NotFound]), known : Link.Known }
    match_bracket = |b, from, gt_end, known| {
        walk = Link.bracket_walk(b, from, [], gt_end, known)
        { close: walk.close, known: Dict.insert(walk.known, from, walk.close) }
    }

    ## `open` holds the content start of every `[` passed and not yet closed,
    ## innermost last.
    bracket_walk : List(U8), U64, List(U64), U64, Link.Known -> { close : Try(U64, [NotFound]), known : Link.Known }
    bracket_walk = |b, i, open, gt_end, known|
        if i >= List.len(b) {
            Link.unclosed(open, known)
        } else {
            c = Bytes.at(b, i)
            if c == 92 {
                Link.bracket_walk(b, i + 2, open, gt_end, known)
            } else if c == 96 {
                ## A code span outranks a bracket, so its contents are opaque —
                ## `[foo`](/uri)`` is a bracket followed by code, not a link.
                n = Bytes.run_len(b, i, 96, 0)
                match Bytes.find_run(b, i + n, 96, n) {
                    Ok(close) => Link.bracket_walk(b, close + n, open, gt_end, known)
                    Err(_) => Link.bracket_walk(b, i + n, open, gt_end, known)
                }
            } else if c == 60 {
                match Link.opaque_end(b, i, gt_end) {
                    Ok(gt) => Link.bracket_walk(b, gt + 1, open, gt_end, known)
                    Err(_) => Link.bracket_walk(b, i + 1, open, gt_end, known)
                }
            } else if c == 91 {
                match Dict.get(known, i + 1) {
                    Ok(Ok(close)) => Link.bracket_walk(b, close + 1, open, gt_end, known)
                    ## This `[` never closes, and every bracket around it
                    ## would have to close after it did.
                    Ok(Err(_)) => Link.unclosed(open, known)
                    Err(_) => Link.bracket_walk(b, i + 1, List.append(open, i + 1), gt_end, known)
                }
            } else if c == 93 {
                match List.last(open) {
                    Ok(inner) => Link.bracket_walk(b, i + 1, List.drop_last(open, 1), gt_end, Dict.insert(known, inner, Ok(i)))
                    Err(_) => { close: Ok(i), known }
                }
            } else {
                Link.bracket_walk(b, i + 1, open, gt_end, known)
            }
        }

    ## The walk ran out: neither it nor any bracket still open closes.
    unclosed : List(U64), Link.Known -> { close : Try(U64, [NotFound]), known : Link.Known }
    unclosed = |open, known| {
        close: Err(NotFound),
        known: List.fold(open, known, |k, o| Dict.insert(k, o, Err(NotFound))),
    }

    ## Where a tag or autolink opened by the `<` at `i` ends, for bracket
    ## matching, which treats one as opaque (`match_bracket`). Its end is
    ## the FIRST `>`, quotes or not; `gt_end` is one past the last `>` in `b`.
    ##
    ## Before slicing out and checking what lies between, the walk gives up
    ## at a `<` outside quotes, for the reason `Tag.find_tag_end` gives — and it
    ## is the same set of answers: a quote this pairs holds no `>`, since a
    ## `>` in it would have been the first one. Without it, each `<` of
    ## `[` + `"<a" * 20_000` + `>]` sliced and checked up to the `>` at the end.
    opaque_end : List(U8), U64, U64 -> Try(U64, [NotFound])
    opaque_end = |b, i, gt_end|
        if i + 1 >= gt_end {
            Err(NotFound)
        } else {
            c = Bytes.at(b, i + 1)
            found = if c == 33 or c == 63 {
                Bytes.find_byte(b, i + 1, 62)
            } else {
                Link.first_gt(b, i + 1)
            }
            match found {
                Ok(gt) =>
                    if Link.is_opaque(Bytes.slice(b, i + 1, gt)) {
                        Ok(gt)
                    } else {
                        Err(NotFound)
                    }
                Err(_) => Err(NotFound)
            }
        }

    ## The first `>` at or after `i`, or `NotFound` at a `<` outside quotes.
    first_gt : List(U8), U64 -> Try(U64, [NotFound])
    first_gt = |b, i|
        if i >= List.len(b) {
            Err(NotFound)
        } else {
            c = Bytes.at(b, i)
            if c == 62 {
                Ok(i)
            } else if c == 60 {
                Err(NotFound)
            } else if c == 34 or c == 39 {
                Link.quote_or_gt(b, i + 1, c)
            } else {
                Link.first_gt(b, i + 1)
            }
        }

    ## Inside a quote opened by `q`: the `>` that is first, or on past the
    ## closing quote.
    quote_or_gt : List(U8), U64, U8 -> Try(U64, [NotFound])
    quote_or_gt = |b, i, q|
        if i >= List.len(b) {
            Err(NotFound)
        } else {
            c = Bytes.at(b, i)
            if c == 62 {
                Ok(i)
            } else if c == q {
                Link.first_gt(b, i + 1)
            } else {
                Link.quote_or_gt(b, i + 1, q)
            }
        }

    ## A raw tag or an autolink is opaque to bracket matching, so
    ## `[foo <bar attr="](baz)">` does not close at the `]` sitting inside an
    ## attribute value.
    is_opaque : Str -> Bool
    is_opaque = |inner| Tag.is_tag(inner) or Autolink.is_autolink(inner)

    match_paren : List(U8), U64, U64 -> Try(U64, [NotFound])
    match_paren = |b, i, depth|
        if i >= List.len(b) {
            Err(NotFound)
        } else {
            c = Bytes.at(b, i)
            if c == 92 {
                Link.match_paren(b, i + 2, depth)
            } else if c == 60 {
                ## An angle-bracketed destination is opaque: `(<b)c>)` closes at
                ## the LAST paren, not the one inside the brackets.
                match Bytes.find_byte(b, i + 1, 62) {
                    Ok(gt) => Link.match_paren(b, gt + 1, depth)
                    Err(_) => Link.match_paren(b, i + 1, depth)
                }
            } else if c == 40 {
                Link.match_paren(b, i + 1, depth + 1)
            } else if c == 41 {
                if depth == 1 {
                    Ok(i)
                } else {
                    Link.match_paren(b, i + 1, depth - 1)
                }
            } else {
                Link.match_paren(b, i + 1, depth)
            }
        }

    lookup : Ref.Index, Str -> Try(Ref.Def, [NotFound])
    lookup = |defs, key|
        match Dict.get(defs, key) {
            Ok(d) => Ok(d)
            Err(_) => Err(NotFound)
        }

    ## The definitions in document order, by label. The FIRST definition of a
    ## label is the one that counts, so a later one never replaces it.
    index : List(Ref.Def) -> Ref.Index
    index = |defs|
        List.fold(
            defs,
            Dict.empty(),
            |ix, d|
                if Dict.contains(ix, d.label) {
                    ix
                } else {
                    Dict.insert(ix, d.label, d)
                },
        )

    ## Labels match case-insensitively with runs of whitespace collapsed, so
    ## `[Foo  Bar]` and `[foo bar]` are one label.
    fold_label : Str -> Str
    ## NOT unescaped: a label matches by its literal text, so `[foo\\!]` and
    ## `[foo!]` are different labels. Unescaping here made them the same.
    fold_label = |k| {
        ## Every kind of whitespace collapses, not just spaces — a label may
        ## span lines, so `[Foo\n  bar]` and `[Foo bar]` are one label.
        flat = Str.from_utf8_lossy(
            List.map(Str.to_utf8(k), |c| if Bytes.is_space(c) { 32 } else { c }),
        )
        collapsed = Str.join_with(List.keep_if(flat.trim().split_on(" "), |x| !Str.is_empty(x)), " ")
        Link.fold_case(Str.with_ascii_lowercased(collapsed))
    }

    ## Case-folds the two-byte scripts that appear in labels: Latin-1
    ## supplement, Greek and Cyrillic. ASCII is already folded by the caller.
    ##
    ## Not the full Unicode table. This is the shape of the problem — a
    ## codepoint range mapped by a fixed offset — and these three ranges cover
    ## what a document actually writes. A label in a script outside them folds
    ## to itself, which fails to match rather than matching wrongly.
    fold_case : Str -> Str
    fold_case = |t|
        if Link.is_ascii(Str.to_utf8(t), 0) {
            t
        } else {
            Str.from_utf8_lossy(Link.fold_bytes(Str.to_utf8(t), 0, []))
        }

    is_ascii : List(U8), U64 -> Bool
    is_ascii = |b, i|
        if i >= List.len(b) {
            True
        } else if Bytes.at(b, i) >= 128 {
            False
        } else {
            Link.is_ascii(b, i + 1)
        }

    ## Two-byte sequences only: `0xC3 0x80..0x9E` (Latin-1 capitals),
    ## `0xCE 0x91..0xA9` (Greek), `0xD0 0x90..0xAF` (Cyrillic А-Я).
    fold_bytes : List(U8), U64, List(U8) -> List(U8)
    fold_bytes = |b, i, out|
        ## `ẞ` folds to TWO characters. One-to-many foldings are the reason a
        ## fold is not a per-character map; this is the one that appears in
        ## practice, and the rest of the table is left alone.
        if Bytes.at(b, i) == 225 and Bytes.at(b, i + 1) == 186 and Bytes.at(b, i + 2) == 158 {
            Link.fold_bytes(b, i + 3, List.concat(out, [115, 115]))
        } else
        match List.get(b, i) {
            Ok(195) => {
                c = Bytes.at(b, i + 1)
                if c >= 128 and c <= 158 and c != 151 {
                    Link.fold_bytes(b, i + 2, List.concat(out, [195, c + 32]))
                } else {
                    Link.fold_bytes(b, i + 2, List.concat(out, [195, c]))
                }
            }
            Ok(206) => {
                c = Bytes.at(b, i + 1)
                if c >= 145 and c <= 169 {
                    ## Greek capitals run past the two-byte lead: Α-Ο fold
                    ## within 0xCE, Π-Ω carry into 0xCF.
                    if c <= 159 {
                        Link.fold_bytes(b, i + 2, List.concat(out, [206, c + 32]))
                    } else {
                        Link.fold_bytes(b, i + 2, List.concat(out, [207, c - 32]))
                    }
                } else {
                    Link.fold_bytes(b, i + 2, List.concat(out, [206, c]))
                }
            }
            Ok(208) => {
                c = Bytes.at(b, i + 1)
                if c >= 144 and c <= 159 {
                    Link.fold_bytes(b, i + 2, List.concat(out, [208, c + 32]))
                } else if c >= 160 and c <= 175 {
                    Link.fold_bytes(b, i + 2, List.concat(out, [209, c - 32]))
                } else {
                    Link.fold_bytes(b, i + 2, List.concat(out, [208, c]))
                }
            }
            Ok(c) => Link.fold_bytes(b, i + 1, List.append(out, c))
            Err(_) => out
        }

    ## A link may not contain another link at any depth, so an outer bracket
    ## whose label already produced one is not a link — it is literal text with
    ## the inner link intact. Images are exempt: their label is alt text.
    has_link : List(Inl) -> Bool
    has_link = |ns| List.any(ns, Link.one_has_link)

    one_has_link : Inl -> Bool
    one_has_link = |n|
        match n.node {
            Link(_) => True
            Emph(ks) => Link.has_link(ks)
            Strong(ks) => Link.has_link(ks)
            Strike(ks) => Link.has_link(ks)
            Image(m) => Link.has_link(m.kids)
            _ => False
        }

    ## `has_link` over tokens. Emphasis is not built yet, so a link is either a
    ## token of its own or inside an image's alt text.
    tks_have_link : List(Emph.Tk) -> Bool
    tks_have_link = |tks|
        List.any(
            tks,
            |tk|
                match tk {
                    Node(n) => Link.one_has_link(n)
                    Delim(_) => False
                },
        )

    link_node : U64, U64, List(Inl), { dest : Str, title : Str, ok : Bool }, Bool, Link.Schemes -> List(Emph.Tk)
    link_node = |start, end, label, parts, image, schemes|
        if !Link.admits(schemes, parts.dest, image) {
            ## The label survives as ordinary content — spliced, not wrapped, so
            ## a rejected href leaves no trace in the tree. An image's label is
            ## its alt text, so a refused image becomes that.
            List.map(label, |n| Node(n))
        } else if image {
            [Node(Inl.at(start, end, Image({ src: parts.dest, title: parts.title, kids: label })))]
        } else {
            [Node(Inl.at(start, end, Link({ href: parts.dest, title: parts.title, kids: label })))]
        }

    ## `allows`, except that `Any` leaves an image's source alone (see
    ## `Schemes`).
    admits : Link.Schemes, Str, Bool -> Bool
    admits = |schemes, dest, image|
        match schemes {
            Any => image or Link.allows(schemes, dest)
            Only(_) => Link.allows(schemes, dest)
        }

    ## Which hrefs a parse lets become links (`Markdown.parse_with`).
    ##
    ## `Any` is the denylist alone (`safe_href`). `Only(list)` is the denylist
    ## AND an allowlist of schemes: an href with no scheme — `/path`, `#frag`,
    ## `rel/x`, `?q` — always passes, and one with a scheme passes only when
    ## that scheme is in `list`, compared ignoring ASCII case. Listing
    ## `javascript` does not let it through; the denylist is not optional.
    ##
    ## An image's source is filtered only under `Only`, by the same rule. Under
    ## `Any` it is not filtered at all, which is deliberate: a browser does not
    ## execute `javascript:` from `<img src>`, and trusted documents embed
    ## `data:` images, which the denylist would break.
    Schemes : [Any, Only(List(Str))]

    ## Does `schemes` let `href` become a link? A refused href becomes text,
    ## at every site that asks: inline and reference links, every autolink,
    ## and (under `Only`) images.
    allows : Link.Schemes, Str -> Bool
    allows = |schemes, href|
        match schemes {
            Any => Link.safe_href(href)
            Only(list) => Link.safe_href(href) and Link.scheme_listed(list, href)
        }

    scheme_listed : List(Str), Str -> Bool
    scheme_listed = |list, href|
        match Link.scheme_of(href.trim()) {
            Ok(scheme) => {
                low = Str.with_ascii_lowercased(scheme)
                List.any(list, |s| Str.with_ascii_lowercased(s) == low)
            }
            Err(NoScheme) => True
        }

    ## The scheme of `href`: everything before a `:` that comes before any
    ## `/`, `?` or `#`. With none of those first, it is a relative reference
    ## and has no scheme.
    ##
    ## RFC 3986 spells a scheme `ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )`,
    ## and that grammar is deliberately NOT checked. A prefix that breaks it
    ## cannot equal a listed scheme, so it is refused, where checking would
    ## call it scheme-less and let it pass. That is the safe side: a consumer
    ## that strips tabs and control bytes from a URL, as a browser's URL
    ## parser does, reads `java\tscript:` (an entity-decoded tab) and
    ## `\u(1)javascript:` as the scheme they spell. `localhost:5001/foo` has
    ## the scheme `localhost`, as the RFC and a browser both read it.
    scheme_of : Str -> Try(Str, [NoScheme])
    scheme_of = |href| {
        b = Str.to_utf8(href)
        Link.colon_at(b, 0).map_ok(|i| Bytes.slice(b, 0, i))
    }

    colon_at : List(U8), U64 -> Try(U64, [NoScheme])
    colon_at = |b, i|
        match List.get(b, i) {
            Err(_) => Err(NoScheme)
            Ok(c) =>
                if c == Link.colon {
                    Ok(i)
                } else if c == Link.slash or c == Link.question or c == Link.hash {
                    Err(NoScheme)
                } else {
                    Link.colon_at(b, i + 1)
                }
        }

    colon : U8
    colon = 58

    slash : U8
    slash = 47

    question : U8
    question = 63

    hash : U8
    hash = 35

    ## The dangerous schemes, refused; everything else allowed.
    ##
    ## This was an allowlist, which is the safer instinct and the wrong rule
    ## here: it also refused `irc:`, `localhost:5001/foo` and every scheme
    ## invented after it was written, so real autolinks silently became text.
    ## cmark's safe mode names the four that matter, and that is the whole
    ## risk surface — a scheme cannot execute unless a browser executes it.
    ##
    ## `data:` is refused outright rather than allowing image subtypes: nothing
    ## here renders a data URL, so the exception would buy nothing.
    safe_href : Str -> Bool
    safe_href = |href| {
        low = Str.with_ascii_lowercased(href.trim())
        !low.starts_with("javascript:")
        and !low.starts_with("vbscript:")
        and !low.starts_with("data:")
        and !low.starts_with("file:")
    }
}
