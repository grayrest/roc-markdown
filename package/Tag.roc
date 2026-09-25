## Raw inline HTML: whether the text inside `<…>` is a well-formed tag, and
## where a tag ends. Shape-based, not an allowlist of element names: the spec's
## rule is about syntax, and an unknown element is still raw HTML.
import Bytes

Tag := [].{
    is_tag : Str -> Bool
    is_tag = |inner| {
        c = Bytes.at(Str.to_utf8(inner), 0)
        n = List.len(Str.to_utf8(inner))
        ## No blanket `contains("<")` check: a QUOTED attribute value may hold
        ## one, as `bam = 'baz <em>"</em>'` does. `attrs_ok` walks the grammar
        ## and knows where a `<` is legal.
        if n == 0 {
            False
        } else if Bytes.is_alpha(c) {
            b = Str.to_utf8(inner)
            ## A TAG name is narrower than an attribute name — letters, digits
            ## and hyphens only. Reusing the attribute charset let `m:abc` be a
            ## tag named `m:abc` instead of the text it is.
            name_len = Tag.tag_name_len(b, 0)
            after = Bytes.at(b, name_len)
            ## A tag NAME ends at whitespace, `/` or `>` — nothing else may
            ## follow it directly. Without this `<m:abc>` was a tag named `m`
            ## rather than the text it is.
            (name_len >= List.len(b) or Bytes.is_space(after) or after == 47 or after == 62)
            and Tag.attrs_ok(b, name_len)
        } else if c == 47 {
            ## A CLOSING tag takes no attributes — `</a href="foo">` is text.
            cb = Str.to_utf8(inner)
            close_len = Tag.tag_name_len(cb, 1)
            Bytes.is_alpha(Bytes.at(cb, 1))
            and Str.is_empty(Bytes.slice(cb, close_len, List.len(cb)).trim())
        } else {
            False
        }
    }

    ## The `>` that closes a tag, skipping quoted attribute values — one inside
    ## `bam = 'baz <em>"</em>'` closes nothing, and stopping at it cut the tag
    ## in half. Comments, instructions and declarations (`<!…`, `<?…`) are
    ## not tags and end by other rules (`special_end`).
    ##
    ## The walk gives up at a `<` outside quotes: inside `<…>` that one fails
    ## every test `Scan.angle` makes of it — not an autolink or an email,
    ## which may hold no `<`, and not a tag, whose grammar admits one only
    ## inside a quoted value, paired exactly as this pairs them. Searching on
    ## to the `>` found the answer no sooner, and `"<a" * 30_000 + ">"`
    ## searched from every `<` to the one at the end.
    find_tag_end : List(U8), U64 -> Try(U64, [NotFound])
    find_tag_end = |b, i|
        if i >= List.len(b) {
            Err(NotFound)
        } else {
            c = Bytes.at(b, i)
            if c == 34 or c == 39 {
                match Bytes.find_byte(b, i + 1, c) {
                    Ok(close) => Tag.find_tag_end(b, close + 1)
                    Err(_) => Err(NotFound)
                }
            } else if c == 62 {
                Ok(i)
            } else if c == 60 {
                Err(NotFound)
            } else {
                Tag.find_tag_end(b, i + 1)
            }
        }

    ## An open tag or a closing tag. Deliberately shape-based rather than a
    ## tag-name allowlist: the spec's rule is about syntax, and an unknown
    ## element is still raw HTML.
    ##
    ## `a < b` is not a tag because `is_tag` requires a letter or `/` straight
    ## after the `<` — which is also the rule that keeps arithmetic readable.
    ## A tag's ATTRIBUTES have a grammar, and without checking it
    ## `<a h*#ref="hi">` and `<a href="hi'>` were raw HTML rather than the text
    ## they are — and `<http://foo.bar/baz bim>` was a tag named `http`.
    attrs_ok : List(U8), U64 -> Bool
    attrs_ok = |b, i| Tag.attrs_walk(b, { i, ok: True })

    ## Where the attribute walk stands: at `i`, or, when `ok` is false, done
    ## with a malformed value.
    AttrStep : { i : U64, ok : Bool }

    ## One loop: a value is read by `value_end`, whose step comes straight
    ## back here. The value reader used to carry on the walk itself, and that
    ## mutual recursion is not a loop, so each attribute cost a stack frame
    ## and about 120,000 of them overflowed. The state is one record,
    ## destructured in the parameter list, for the reason `Emph.emph_walk`
    ## gives.
    attrs_walk : List(U8), Tag.AttrStep -> Bool
    attrs_walk = |b, { i, ok }|
        if !ok {
            False
        } else if i >= List.len(b) {
            True
        } else {
            c = Bytes.at(b, i)
            if c == 47 or c == 62 {
                i + 1 >= List.len(b) or Bytes.at(b, i + 1) == 62
            } else if Bytes.is_space(c) {
                Tag.attrs_walk(b, { i: i + 1, ok: True })
            } else if Bytes.is_alpha(c) or c == 95 or c == 58 {
                n = Tag.attr_name_len(b, i, 0)
                ## Whitespace is allowed on BOTH sides of `=`, so the name is
                ## followed by an optional gap before it — `bam = 'x'` is one
                ## attribute, not a boolean `bam` followed by junk.
                eq = Tag.skip_spaces(b, i + n)
                if Bytes.at(b, eq) == 61 {
                    Tag.attrs_walk(b, Tag.value_end(b, Tag.skip_spaces(b, eq + 1)))
                } else {
                    Tag.attrs_walk(b, { i: i + n, ok: True })
                }
            } else {
                False
            }
        }

    tag_name_len : List(U8), U64 -> U64
    tag_name_len = |b, i| {
        c = Bytes.at(b, i)
        if Bytes.is_alpha(c) or (i > 0 and ((c >= 48 and c <= 57) or c == 45)) {
            Tag.tag_name_len(b, i + 1)
        } else {
            i
        }
    }

    skip_spaces : List(U8), U64 -> U64
    skip_spaces = |b, i|
        if i < List.len(b) and Bytes.is_space(Bytes.at(b, i)) {
            Tag.skip_spaces(b, i + 1)
        } else {
            i
        }

    attr_name_len : List(U8), U64, U64 -> U64
    attr_name_len = |b, i, acc| {
        c = Bytes.at(b, i + acc)
        if Bytes.is_alpha(c) or (c >= 48 and c <= 57) or c == 95 or c == 58 or c == 45 or c == 46 {
            Tag.attr_name_len(b, i, acc + 1)
        } else {
            acc
        }
    }

    ## Where the attribute value starting at `i` ends, for the walk to carry
    ## on from, or `ok: False` when no well-formed value starts there.
    value_end : List(U8), U64 -> Tag.AttrStep
    value_end = |b, i| {
        c = Bytes.at(b, i)
        if c == 34 or c == 39 {
            match Bytes.find_byte(b, i + 1, c) {
                Ok(close) => {
                    ## Attributes are separated by WHITESPACE — without that,
                    ## `<a href='bar'title=title>` looked well-formed.
                    nxt = Bytes.at(b, close + 1)
                    { i: close + 1, ok: close + 1 >= List.len(b) or Bytes.is_space(nxt) or nxt == 62 or nxt == 47 }
                }
                Err(_) => { i, ok: False }
            }
        } else {
            n = Tag.unquoted_len(b, i, 0)
            { i: i + n, ok: n > 0 }
        }
    }

    unquoted_len : List(U8), U64, U64 -> U64
    unquoted_len = |b, i, acc| {
        c = Bytes.at(b, i + acc)
        if i + acc >= List.len(b) or Bytes.is_space(c) or c == 62 or c == 34 or c == 39 or c == 61 or c == 60 or c == 96 {
            acc
        } else {
            Tag.unquoted_len(b, i, acc + 1)
        }
    }

    ## Where the terminator of each construct `special_end` reads last
    ## starts in the text, plus one, and 0 when it never does. "Some
    ## occurrence starts at or after `p`" holds exactly when `p` is below
    ## that, so each search is known to succeed before it runs, and one that
    ## would fail, running to the end of the text, never does.
    Lasts : { comment : U64, pi : U64, cdata : U64 }

    ## `Lasts` for `b`, whose three backward scans are skipped when nothing
    ## in it opens a comment, instruction or CDATA section: unguarded, they
    ## cost a fifth more on a long paragraph of autolinks.
    lasts_of : List(U8) -> Tag.Lasts
    lasts_of = |b|
        if Tag.has_special(b, 0) {
            n = List.len(b)
            {
                comment: Bytes.seq_last(b, n, Tag.comment_close),
                pi: Bytes.seq_last(b, n, Tag.pi_close),
                cdata: Bytes.seq_last(b, n, Tag.cdata_close),
            }
        } else {
            { comment: 0, pi: 0, cdata: 0 }
        }

    ## Does a `<!` or `<?` start at or after `i`?
    has_special : List(U8), U64 -> Bool
    has_special = |b, i|
        if i + 1 >= List.len(b) {
            False
        } else if Bytes.at(b, i) == 60 and (Bytes.at(b, i + 1) == 33 or Bytes.at(b, i + 1) == 63) {
            True
        } else {
            Tag.has_special(b, i + 1)
        }

    ## `-->`
    comment_close : List(U8)
    comment_close = [45, 45, 62]

    ## `?>`
    pi_close : List(U8)
    pi_close = [63, 62]

    ## `]]>`
    cdata_close : List(U8)
    cdata_close = [93, 93, 62]

    ## One past the end of the comment, processing instruction, CDATA section
    ## or declaration opened by the `<` at `i`, by CommonMark's rules (0.31.2
    ## §6.6): each ends at the FIRST occurrence of its own terminator, and
    ## quotes mean nothing inside any of them. A comment is `<!-->`, `<!--->`
    ## or `<!--` up to `-->`; an instruction `<?` up to `?>`; CDATA
    ## `<![CDATA[` up to `]]>`; a declaration `<!` and a letter up to `>`.
    ##
    ## They were read as tags, pairing quotes the way attribute values pair,
    ## which cut `<!-- a > b -->` at its first `>`, and made each `<!"` of a
    ## paragraph re-pair every quote after it, which was quadratic.
    ##
    ## A declaration searches without a `Lasts` entry: the scanner only asks
    ## when a `>` follows, and it resumes past the one found.
    special_end : List(U8), U64, Tag.Lasts -> Try(U64, [NotFound])
    special_end = |b, i, lasts|
        if Bytes.starts(b, i, "<!-->") {
            Ok(i + 5)
        } else if Bytes.starts(b, i, "<!--->") {
            Ok(i + 6)
        } else if Bytes.starts(b, i, "<!--") {
            Tag.close_after(b, i + 4, Tag.comment_close, lasts.comment)
        } else if Bytes.starts(b, i, "<![CDATA[") {
            Tag.close_after(b, i + 9, Tag.cdata_close, lasts.cdata)
        } else if Bytes.starts(b, i, "<?") {
            Tag.close_after(b, i + 2, Tag.pi_close, lasts.pi)
        } else if Bytes.starts(b, i, "<!") and Bytes.is_alpha(Bytes.at(b, i + 2)) {
            Bytes.find_byte(b, i + 3, 62).map_ok(|gt| gt + 1)
        } else {
            Err(NotFound)
        }

    ## One past the first `close` at or after `from`, searched for only when
    ## `last` (its `Lasts` entry) says there is one.
    close_after : List(U8), U64, List(U8), U64 -> Try(U64, [NotFound])
    close_after = |b, from, close, last|
        if from < last {
            Bytes.find_seq(b, from, close).map_ok(|at| at + List.len(close))
        } else {
            Err(NotFound)
        }
}
