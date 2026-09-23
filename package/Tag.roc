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
            c == 33 or c == 63
        }
    }

    ## The `>` that closes a tag, skipping quoted attribute values — one inside
    ## `bam = 'baz <em>"</em>'` closes nothing, and stopping at it cut the tag
    ## in half.
    ##
    ## Only a declaration or an instruction (`<!…>`, `<?…>`) searches all the
    ## way. Anything else gives up at a `<` outside quotes: inside `<…>` that
    ## one fails every test `Scan.angle` makes of it — not an autolink or an
    ## email, which may hold no `<`, and not a tag, whose grammar admits one
    ## only inside a quoted value, paired exactly as this pairs them. Searching
    ## on to the `>` found the answer no sooner, and `"<a" * 30_000 + ">"`
    ## searched from every `<` to the one at the end.
    find_tag_end : List(U8), U64 -> Try(U64, [NotFound])
    find_tag_end = |b, i| {
        c = Bytes.at(b, i)
        Tag.close_walk(b, i, !(c == 33 or c == 63))
    }

    close_walk : List(U8), U64, Bool -> Try(U64, [NotFound])
    close_walk = |b, i, lt_ends|
        if i >= List.len(b) {
            Err(NotFound)
        } else {
            c = Bytes.at(b, i)
            if c == 34 or c == 39 {
                match Bytes.find_byte(b, i + 1, c) {
                    Ok(close) => Tag.close_walk(b, close + 1, lt_ends)
                    Err(_) => Err(NotFound)
                }
            } else if c == 62 {
                Ok(i)
            } else if c == 60 and lt_ends {
                Err(NotFound)
            } else {
                Tag.close_walk(b, i + 1, lt_ends)
            }
        }

    ## An open tag, a closing tag, or a declaration/instruction. Deliberately
    ## shape-based rather than a tag-name allowlist: the spec's rule is about
    ## syntax, and an unknown element is still raw HTML.
    ##
    ## `a < b` is not a tag because `is_tag` requires a letter or `/` straight
    ## after the `<` — which is also the rule that keeps arithmetic readable.
    ## A tag's ATTRIBUTES have a grammar, and without checking it
    ## `<a h*#ref="hi">` and `<a href="hi'>` were raw HTML rather than the text
    ## they are — and `<http://foo.bar/baz bim>` was a tag named `http`.
    attrs_ok : List(U8), U64 -> Bool
    attrs_ok = |b, i|
        if i >= List.len(b) {
            True
        } else {
            c = Bytes.at(b, i)
            if c == 47 or c == 62 {
                i + 1 >= List.len(b) or Bytes.at(b, i + 1) == 62
            } else if Bytes.is_space(c) {
                Tag.attrs_ok(b, i + 1)
            } else if Bytes.is_alpha(c) or c == 95 or c == 58 {
                n = Tag.attr_name_len(b, i, 0)
                ## Whitespace is allowed on BOTH sides of `=`, so the name is
                ## followed by an optional gap before it — `bam = 'x'` is one
                ## attribute, not a boolean `bam` followed by junk.
                eq = Tag.skip_spaces(b, i + n)
                if Bytes.at(b, eq) == 61 {
                    Tag.attr_value(b, Tag.skip_spaces(b, eq + 1))
                } else {
                    Tag.attrs_ok(b, i + n)
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

    attr_value : List(U8), U64 -> Bool
    attr_value = |b, i| {
        c = Bytes.at(b, i)
        if c == 34 or c == 39 {
            match Bytes.find_byte(b, i + 1, c) {
                Ok(close) => {
                    ## Attributes are separated by WHITESPACE — without that,
                    ## `<a href='bar'title=title>` looked well-formed.
                    nxt = Bytes.at(b, close + 1)
                    (close + 1 >= List.len(b) or Bytes.is_space(nxt) or nxt == 62 or nxt == 47)
                    and Tag.attrs_ok(b, close + 1)
                }
                Err(_) => False
            }
        } else {
            n = Tag.unquoted_len(b, i, 0)
            n > 0 and Tag.attrs_ok(b, i + n)
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
}
